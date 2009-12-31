(defpackage #:chillax-server (:use :cl))
(defpackage #:chillax-server-user (:use :cl))
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

(defmacro with-user-package (&body body)
  `(let ((*package* (find-package :chillax-server-user)))
     ,@body))

(defun couch-log (format-string &rest format-args)
  (format t "[\"log\", ~S]~%" (apply #'format nil format-string format-args))
  (finish-output))

(defun add-fun (string)
  (handler-case (prog1 t (push (eval (read-from-string string)) *functions*))
    (error (err)
      (equal-hash "error" "function_compilation_failed"
                  "reason" (princ-to-string (type-of err))))))

(defun reset () (setf *functions* nil) t)

(defun call-map-function (function doc &aux *map-results*)
  (with-user-package (funcall function doc)) (or *map-results* '(#())))

(defun map-doc (doc)
  (or (mapcar (fun (call-map-function _ doc)) *functions*) '((#()))))

(defun reduce-map (fun-strings keys values)
  (list t (mapcar (fun (with-user-package (funcall (eval (read-from-string _)) keys values)))
                  fun-strings)))

(defun rereduce (fun-strings reduce-results)
  (list t (mapcar (fun (with-user-package (funcall (eval (read-from-string _)) reduce-results)))
                  fun-strings)))

(defun run-server (&aux *functions*)
  (handler-case
      (loop for input = (read-line)
         for parsed = (json:parse input)
         do (json:encode (cond ((string= (car parsed) "reset") (reset))
                               ((string= (car parsed) "add_fun") (add-fun (cadr parsed)))
                               ((string= (car parsed) "map_doc") (map-doc (cadr parsed)))
                               ((string= (car parsed) "reduce")
                                (loop for result in (third parsed)
                                   collect (caar result) into keys
                                   collect (cadr result) into values
                                   finally (return (reduce-map (second parsed) keys values))))
                               ((string= (car parsed) "rereduce") (apply #'rereduce (cdr parsed)))
                               (t (error "Unknown message: ~S" input))))
         (terpri)
         (finish-output))
    (end-of-file () nil)))
