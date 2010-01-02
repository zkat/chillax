(defpackage #:chillax-server (:use :cl))
(in-package :chillax-server)

(defmacro fun (&body body)
  "This macro puts the FUN back in FUNCTION."
  `(lambda (&optional _) (declare (ignorable _)) ,@body))

(defvar *map-results*)
(defvar *functions*)

(defun equal-hash (&rest keys &aux (table (make-hash-table :test #'equal)))
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

(defun emit (k v)
  (when (boundp *map-results*)
    (push (list k v) *map-results*)))

(defun couch-log (format-string &rest format-args)
  (format t "~&[\"log\", ~S]~%" (apply #'format nil format-string format-args))
  (finish-output))

(defun add-fun (string)
  (push (compile-view-function string) *functions*) t)

(defun reset () (setf *functions* nil) t)

(defun call-map-function (function doc &aux *map-results*)
  (funcall function doc) (or *map-results* '(#())))

(defun map-doc (doc)
  (or (mapcar (fun (call-map-function _ doc)) *functions*) '((#()))))

(defun reduce-map (fun-strings keys-and-values)
  (loop for result in keys-and-values
     collect (caar result) into keys
     collect (cadr result) into values
     finally (return
               (list t (mapcar (fun (funcall (compile-view-function _)
                                             keys values nil))
                               fun-strings)))))

(defun rereduce (fun-strings values)
  (list t (mapcar (fun (funcall (compile-view-function _) nil values t)) fun-strings)))

(defvar *dispatch*
  '(("reset" . reset)
    ("add_fun" . add-fun)
    ("map_doc" . map-doc)
    ("reduce" . reduce-map)
    ("rereduce" . rereduce)
    ;; Not implemented
    ;; ("validate" . validate)
    ;; ("show" . show)
    ;; ("update" . update)
    ;; ("list" . couch-list)
    ;; ("filter" . filter)
    ))

(defun run-server (&aux *functions* (*package* (find-package :chillax-server))
                   (black-hole (make-broadcast-stream))
                   (*error-output* black-hole)
                   (*debug-io* black-hole)
                   (*trace-output* black-hole))
  (handler-case
      (loop for input = (read-line)
         for (name . args) = (json:parse input)
         do (json:encode
             (handler-case
                 (let ((dispatch-result (assoc name *dispatch* :test #'string=)))
                   (if dispatch-result
                       (apply (fdefinition (cdr dispatch-result)) args)
                       (progn
                         (couch-log "Unknown message: ~A" name)
                         (equal-hash "error" "unknown_message"
                                     "reason" "Received an unknown message from CouchDB"))))
               (error (e)
                 (equal-hash "error" (princ-to-string (type-of e))
                             "reason" (prin1-to-string e)))))
         (terpri)
         (finish-output))
    (end-of-file () (values))))
