(defpackage #:chillax-server
  (:use :cl :chillax.utils)
  (:export :mkhash :strcat :fun :emit :hashget :log-message :validation-failure :forbidden
           :*user-package* :*encode-json* :*decode-json*))
(defpackage #:chillax-server-user
  (:use :cl :chillax-server))
(in-package :chillax-server)

;;;
;;; Configuration
;;;
(defparameter *user-package* (find-package :chillax-server-user)
  "Package that user view functions will be compiled and executed in.")
(defparameter *encode-json* #'yason:encode
  "Function to use when encoding Lisp->JSON. Must return a string.")
(defparameter *decode-json* #'yason:parse
  "Function to use to decode JSON->Lisp. Must accept a string.")

;;;
;;; Utils
;;;
(defmacro with-user-package (&body body)
  "Evaluates BODY in the :chillax-server-user package."
  `(let ((*package* *user-package*))
     ,@body))

(defmacro fun (&body body)
  "This macro puts the FUN back in FUNCTION."
  `(lambda (&optional _) (declare (ignorable _)) ,@body))

;;;
;;; Conditions
;;;
(define-condition chillax-server-error (error) ())

(define-condition function-compilation-error (chillax-server-error)
  ((function-string :initarg :string :reader function-string))
  (:report (lambda (c s) (format s "Function compilation failed: ~A" (function-string c)))))

(define-condition validation-failure (chillax-server-error)
  ((message :initarg :message :reader failure-message))
  (:report (lambda (c s) (format s "Validation failed: ~A" (failure-message c)))))

(define-condition forbidden (validation-failure)
  ()
  (:report (lambda (c s) (format s "Operation Forbidden: ~A" (failure-message c)))))

;;;
;;; User functions
;;;
(defvar *config* nil)
(defvar *functions*) ; holds a list of functions CouchDB is currently dealing with.
(defvar *function-cache* (make-hash-table :test #'equal)
  "Cache of compiled user functions. This is cleared whenever the reset command is run.")

(defun ensure-view-function (maybe-function)
  "Returns a compiled lisp function. MAYBE-FUNCTION can be a function object or a string
with the source code to compile a function from."
  (or (when (functionp maybe-function) maybe-function)
      (gethash maybe-function *function-cache*)
      (compile-view-function maybe-function)
      (error "Can't return a function based on ~S" maybe-function)))

(defun compile-view-function (string)
  "Compiles an anonymous function from STRING."
  (multiple-value-bind (function warningsp failurep)
      (with-user-package
        (compile nil (read-from-string string)))
    (when warningsp
      (log-message "A view function did not compile cleanly."))
    (if failurep
        (error 'function-compilation-error :string string)
        (setf (gethash string *function-cache*) function))))

(defun call-user-function (function &rest args)
  (with-user-package (apply (ensure-view-function function) args)))

(defun respond (response)
  (handler-case
      (funcall *encode-json* response)
    (error () (log-message "Error encoding response: ~A." response)))
  (terpri)
  (finish-output))

;;;
;;; User-accessible functions
;;;
(defvar *map-results*)
(defun emit (key value)
  "Adds an entry to the current map function results."
  (push (list key value) *map-results*))

(defun log-message (format-string &rest format-args)
  "Like FORMAT, but the resulting string is written to CouchDB's log."
  (format t "~&[\"log\", \"Chillax View Server: ~A\"]~%"
          (apply #'format nil format-string format-args))
  (finish-output))

;;;
;;; CouchDB Commands
;;;
(defun add-fun (string)
  "Compiles and adds a function whose source code is in STRING to the current list of
active CouchDB functions."
  (push (ensure-view-function string) *functions*)
  (respond t))

(defun reset (&optional config)
  "Resets the view server. Any caches should be emptied at this point, and any stored
map functions should be cleared out."
  (setf *config* config)
  (when (boundp '*functions*)
    (setf *functions* nil))
  (clrhash *function-cache*)
  (respond t))

(defun call-map-function (function doc &aux *map-results*)
  "Calls a stored compile function on a document. *MAP-RESULTS* is where EMIT will send k/v pairs."
  (call-user-function function doc)
  (or *map-results* '(#())))

(defun map-doc (doc)
  "Responds to CouchDB with the results of calling all the currently-active map functions on DOC."
  (respond (or (mapcar (fun (call-map-function _ doc)) (reverse *functions*)) '((#())))))

(defun reduce-results (fun-strings keys-and-values)
  "Responds to CouchDb with the results of calling the functions in FUN-STRINGS on KEYS-AND-VALUES."
  (loop for result in keys-and-values
     collect (caar result) into keys
     collect (cadr result) into values
     finally (respond (list t (mapcar (fun (call-user-function _ keys values nil))
                                      fun-strings)))))

(defun rereduce (fun-strings values)
  "Responds to CouchDB with the results of rereducing FUN-STRINGS on VALUES."
  (respond (list t (mapcar (fun (call-user-function _ nil values t)) fun-strings))))

(defun filter (docs req user-context)
  "Responds to CouchDB with the result of filtering DOCS using the current filter function."
  ;; Yes. I know it only uses the first function only. The JS view server does the same thing.
  (respond (list t (mapcar (fun (call-user-function (car (last *functions*)) _ req user-context)) docs))))

(defun validate (fun-string new-doc old-doc user-context)
  "Executes a view function that validates NEW-DOC as a new version of OLD-DOC.
Validation passes when the function returns normally. Validation will fail if the function errors in
any way, and the condition's name and printout will be send to CouchDB as the exception."
  (handler-case (progn
                  (call-user-function fun-string new-doc old-doc user-context)
                  (respond "1")) ; the JS server does this. Cargo cult culture dictates
                                 ; that I should copy behavior regardless of understanding.
    (error (e)
      (respond (mkhash (string-downcase (princ-to-string (type-of e)))
                       (remove #\Newline (princ-to-string e)))))))

(defun ensure-response (maybe-response)
  (if (hash-table-p maybe-response)
      maybe-response
      (mkhash "body" (princ-to-string maybe-response))))

(defun show (fun-string doc request)
  "Show functions are used to render documents. See CouchDB documentation for more details."
  (respond (list "resp" (ensure-response (call-user-function fun-string doc request)))))

(defun update (fun-string doc request)
  "See CouchDB documentation for how to use _update. Functions written for the Chillax view server
should return (values document response)."
  (respond (multiple-value-call #'list "up" (call-user-function fun-string doc request))))

;;;
;;; View server
;;;
(defparameter *dispatch*
  `(("reset" . reset)
    ("add_fun" . add-fun)
    ("map_doc" . map-doc)
    ("reduce" . reduce-results)
    ("rereduce" . rereduce)
    ;; TODO - everything below here has changed.
    ;;        CouchDB now uses some sort of 'DDoc'
    ;;        thing, so this needs updating.
    ;; ("filter" . ,#'filter)
    ;; ("validate" . ,#'validate)
    ;; ("show" . ,#'show)
    ;; ("update" . ,#'update)
    ;; ("list" . couch-list)
    )
  "Dispatch table holding Couch command -> Chillax function associations.")

(defun run-server (&aux *functions*
                   (black-hole (make-broadcast-stream))
                   (*error-output* black-hole)
                   (*debug-io* black-hole)
                   (*trace-output* black-hole))
  "Toplevel function that parses view requests from CouchDB and sends responses back."
  (loop for line = (read-line *standard-input* nil nil)
     while (< 0 (length line))
     for (name . args) =  (funcall *decode-json* line) do
     (handler-case
         (let ((dispatch-result (assoc name *dispatch* :test #'string=)))
           (if dispatch-result
               (apply (cdr dispatch-result) args)
               (progn
                 (log-message "Unknown message: ~A" name)
                 (respond (mkhash "error" "unknown_message"
                                  "reason" "Received an unknown message from CouchDB")))))
       (end-of-file () (return-from run-server nil))
       (error (e)
         (respond (mkhash "error" (princ-to-string (type-of e))
                          "reason" (remove #\Newline (princ-to-string e))))))))
