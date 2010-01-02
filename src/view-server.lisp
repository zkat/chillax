(defpackage #:chillax-server
  (:use :cl)
  (:export :mkhash :emit))
(defpackage #:chillax-server-user
  (:use :cl :chillax-server))
(in-package :chillax-server)

(defmacro with-user-package (&body body)
  `(let ((*package* (find-package :chillax-server-user)))
     ,@body))

(defmacro fun (&body body)
  "This macro puts the FUN back in FUNCTION."
  `(lambda (&optional _) (declare (ignorable _)) ,@body))

(defun mkhash (&rest keys &aux (table (make-hash-table :test #'equal)))
  (loop for (key val) on keys by #'cddr do (setf (gethash key table) val)
     finally (return table)))

(define-condition chillax-server-error () ())
(define-condition function-compilation-error (chillax-server-error)
  ((function-string :initarg :string :reader function-string)))

(defun compile-view-function (string)
  "Compiles an anonymous function from STRING."
  (multiple-value-bind (function warningsp failurep)
      (compile nil (read-from-string string))
    (when warningsp
      (couch-log "View function did not compile cleanly: ~A" (remove #\Newline string)))
    (if failurep
        ;; FIXME: This is basically a constant.
        (error 'function-compilation-error :string string)
        function)))

(defvar *map-results*)
(defun emit (k v)
  (when (boundp *map-results*)
    (push (list k v) *map-results*)))

(defun couch-log (format-string &rest format-args)
  (format t "~&[\"log\", ~S]~%" (apply #'format nil format-string format-args))
  (finish-output))

(defun add-fun (string)
  (push (with-user-package (compile-view-function string)) *functions*)
  (respond t))

(defun reset (&optional config)
  (declare (ignore config))
  (setf *functions* nil)
  (respond t))

(defun call-map-function (function doc &aux *map-results*)
  (with-user-package (funcall function doc)) (or *map-results* '(#())))

(defun map-doc (doc)
  (respond (or (mapcar (fun (call-map-function _ doc)) *functions*) '((#())))))

(defun reduce-map (fun-strings keys-and-values)
  (loop for result in keys-and-values
     collect (caar result) into keys
     collect (cadr result) into values
     finally (respond (list t (mapcar (fun (with-user-package
                                             (funcall (compile-view-function _)
                                                      keys values nil)))
                                      fun-strings)))))

(defun rereduce (fun-strings values)
  (respond (list t (mapcar (fun (with-user-package
                                  (funcall (compile-view-function _) nil values t))) fun-strings))))

(defparameter *dispatch*
  `(("reset" . ,#'reset)
    ("add_fun" . ,#'add-fun)
    ("map_doc" . ,#'map-doc)
    ("reduce" . ,#'reduce-map)
    ("rereduce" . ,#'rereduce)
    ;; Not implemented
    ;; ("validate" . validate)
    ;; ("show" . show)
    ;; ("update" . update)
    ;; ("list" . couch-list)
    ;; ("filter" . filter)
    ))

(defun respond (response)
  (handler-case
      (json:encode response)
    (error () (couch-log "Error encoding response: ~A." response)))
  (terpri)
  (finish-output))

(defvar *functions*)
(defun run-server (&aux *functions*
                   (black-hole (make-broadcast-stream))
                   (*error-output* black-hole)
                   (*debug-io* black-hole)
                   (*trace-output* black-hole))
  (handler-case
      (loop for (name . args) = (json:parse (read-line)) do
           (handler-case
               (let ((dispatch-result (assoc name *dispatch* :test #'string=)))
                 (if dispatch-result
                     (apply (cdr dispatch-result) args)
                     (progn
                       (couch-log "Unknown message: ~A" name)
                       (mkhash "error" "unknown_message"
                               "reason" "Received an unknown message from CouchDB"))))
             (error (e)
               (respond (mkhash "error" (princ-to-string (type-of e))
                                "reason" (prin1-to-string e))))))
    (end-of-file () (values))))
