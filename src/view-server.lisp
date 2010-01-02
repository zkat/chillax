(defpackage #:chillax-server
  (:use :cl)
  (:export :mkhash :emit :hashget :log-message))
(defpackage #:chillax-server-user
  (:use :cl :chillax-server))
(in-package :chillax-server)

(defmacro with-user-package (&body body)
  "Evaluates BODY in the :chillax-server-user package."
  `(let ((*package* (find-package :chillax-server-user)))
     ,@body))

(defmacro fun (&body body)
  "This macro puts the FUN back in FUNCTION."
  `(lambda (&optional _) (declare (ignorable _)) ,@body))

(defun mkhash (&rest keys-and-values &aux (table (make-hash-table :test #'equal)))
  "Convenience function for `literal' hash table definition."
  (loop for (key val) on keys-and-values by #'cddr do (setf (gethash key table) val)
     finally (return table)))

(define-condition chillax-server-error () ())
(define-condition function-compilation-error (chillax-server-error)
  ((function-string :initarg :string :reader function-string)))

(defun compile-view-function (string)
  "Compiles an anonymous function from STRING."
  (multiple-value-bind (function warningsp failurep)
      (compile nil (read-from-string string))
    (when warningsp
      (log-message "View function did not compile cleanly: ~A" (remove #\Newline string)))
    (if failurep
        (error 'function-compilation-error :string string)
        function)))

(defvar *map-results*)
(defun emit (key value)
  "Adds an entry to the current map function results."
  (when (boundp *map-results*)
    (push (list key value) *map-results*)))

(defun hashget (hash key &rest more-keys)
  "Convenience function for recursively accessing hash tables."
  (flet ((reverse-gethash (hash key) (gethash key hash)))
    (reduce #'reverse-gethash more-keys :initial-value (gethash key hash))))

(defun log-message (format-string &rest format-args)
  "Like FORMAT, but the resulting string is written to CouchDB's log."
  (format t "~&[\"log\", Chillax View Server: ~S]~%" (apply #'format nil format-string format-args))
  (finish-output))

(defun add-fun (string)
  "Compiles and adds a function whose source code is in STRING to the current list of
active CouchDB functions."
  (push (with-user-package (compile-view-function string)) *functions*)
  (respond t))

(defun reset (&optional config)
  "Resets the view server. Any caches should be emptied at this point, and any stored
map functions should be cleared out."
  (declare (ignore config))
  (setf *functions* nil)
  (respond t))

(defun call-map-function (function doc &aux *map-results*)
  "Calls a stored compile function on a document. *MAP-RESULTS* is where EMIT will send k/v pairs."
  (with-user-package (funcall function doc)) (or *map-results* '(#())))

(defun map-doc (doc)
  "Responds to CouchDB with the results of calling all the currently-active map functions on DOC."
  (respond (or (mapcar (fun (call-map-function _ doc)) *functions*) '((#())))))

(defun reduce-results (fun-strings keys-and-values)
  "Responds to CouchDb with the results of calling the functions in FUN-STRINGS on KEYS-AND-VALUES."
  ;; It's quite possible a good idea to cache the compiled version of these functions in a hash
  ;; table, indexed by their source string. The cache will be cleared out whenever couch asks for
  ;; a ["reset"].
  (loop for result in keys-and-values
     collect (caar result) into keys
     collect (cadr result) into values
     finally (respond (list t (mapcar (fun (with-user-package
                                             (funcall (compile-view-function _)
                                                      keys values nil)))
                                      fun-strings)))))

(defun rereduce (fun-strings values)
  "Responds to CouchDB with the results of rereducing FUN-STRINGS on VALUES."
  ;; Should -definitely- cache the reduce functions. Recompiling all of these is insane.
  (respond (list t (mapcar (fun (with-user-package
                                  (funcall (compile-view-function _) nil values t)))
                           fun-strings))))

(defun filter (docs req user-context)
  "Responds to CouchDB with the result of filtering DOCS using the current filter function."
  ;; Yes. I know it only uses the first function only. The JS view server does the same thing.
  (respond (list t (mapcar (fun (funcall (car *functions*) _ req user-context)) docs))))

(defparameter *dispatch*
  `(("reset" . ,#'reset)
    ("add_fun" . ,#'add-fun)
    ("map_doc" . ,#'map-doc)
    ("reduce" . ,#'reduce-results)
    ("rereduce" . ,#'rereduce)
    ("filter" . filter)
    ;; Not implemented
    ;; ("validate" . validate)
    ;; ("show" . show)
    ;; ("update" . update)
    ;; ("list" . couch-list)
    )
  "Dispatch table holding Couch command -> Chillax function associations.")

(defun respond (response)
  (handler-case
      (json:encode response)
    (error () (log-message "Error encoding response: ~A." response)))
  (terpri)
  (finish-output))

(defvar *functions*)
(defun run-server (&aux *functions*
                   (black-hole (make-broadcast-stream))
                   (*error-output* black-hole)
                   (*debug-io* black-hole)
                   (*trace-output* black-hole))
  "Toplevel function that parses view requests from CouchDB and sends responses back."
  (handler-case
      (loop for (name . args) = (json:parse (read-line)) do
           (handler-case
               (let ((dispatch-result (assoc name *dispatch* :test #'string=)))
                 (if dispatch-result
                     (apply (cdr dispatch-result) args)
                     (progn
                       (log-message "Unknown message: ~A" name)
                       (mkhash "error" "unknown_message"
                               "reason" "Received an unknown message from CouchDB"))))
             (error (e)
               (respond (mkhash "error" (princ-to-string (type-of e))
                                "reason" (prin1-to-string e))))))
    (end-of-file () (values))))
