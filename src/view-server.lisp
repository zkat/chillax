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

(defun emit (k v)
  (when (boundp *map-results*)
    (push (list k v) *map-results*)))

(defun couch-log (format-string &rest format-args)
  (format t "~&[\"log\", ~S]~%" (apply #'format nil format-string format-args))
  (finish-output))

(defun add-fun (string)
  (multiple-value-bind (function warningsp failurep)
      (compile nil (read-from-string string))
    (when warningsp
      (couch-log "A view did not compile cleanly."))
    (if failurep
        ;; FIXME: This is basically a constant.
        (equal-hash "error" "function_compilation_failed"
                    "reason" "Errors were signaled during compilation")
        (progn (push function *functions*) t))))

(defun reset () (setf *functions* nil) t)

(defun call-map-function (function doc &aux *map-results*)
  (funcall function doc) (or *map-results* '(#())))

(defun map-doc (doc)
  (or (mapcar (fun (call-map-function _ doc)) *functions*) '((#()))))

(defun reduce-map (fun-strings keys values)
  (list t (mapcar (fun (funcall (compile nil (read-from-string _)) keys values nil)) fun-strings)))

(defun rereduce (fun-strings values)
  (list t (mapcar (fun (funcall (compile nil (read-from-string _)) keys values t)) fun-strings)))

(defun run-server (&aux *functions* (*package* (find-package :chillax-server))
                   (black-hole (make-broadcast-stream))
                   (*error-output* black-hole)
                   (*debug-io* black-hole)
                   (*trace-output* black-hole))
  (handler-case
      (loop for input = (read-line)
         for parsed = (json:parse input)
         do (json:encode
             (cond ((string= (car parsed) "reset") (reset))
                   ((string= (car parsed) "add_fun") (add-fun (cadr parsed)))
                   ((string= (car parsed) "map_doc") (map-doc (cadr parsed)))
                   ((string= (car parsed) "reduce")
                    (loop for result in (third parsed)
                       collect (caar result) into keys
                       collect (cadr result) into values
                       finally (return (reduce-map (second parsed) keys values))))
                   ((string= (car parsed) "rereduce") (apply #'rereduce (cdr parsed)))
                   (t (couch-log "Unknown-message: ~A" (car parsed))
                      (equal-hash "error" "unknown_message"
                                  "reason" "Received an unknown message from CouchDB"))))
         (terpri)
         (finish-output))
    (end-of-file () nil)))
