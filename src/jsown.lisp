(defpackage #:chillax.jsown
  (:use :cl :chillax.core)
  (:export :jsown-server :call-with-key-container :with-key-container))
(in-package :chillax.jsown)

(defclass jsown-server (standard-server)
  ()
  (:documentation
   "JSOWN-SERVERs use JSOWN's JSON parser/encoder to automatically translate content going to/coming
   from the associated CouchDB server."))

(defmethod data->json ((server jsown-server) data &key)
  (jsown:to-json data))

(defvar *key-container* nil)
(defun call-with-key-container (function container)
  "Calls FUNCTION, which require zero arguments, in a context where CONTAINER will be used for
jsown:parse-with-container."
  (let ((*key-container* container))
    (funcall function)))
(defmacro with-key-container ((container) &body body)
  "Convenience macro for call-with-key-container."
  `(call-with-key-container (lambda () ,@body) ,container))

(defmethod json->data ((server jsown-server) json &key)
  (if *key-container*
      (jsown:parse-with-container json *key-container*)
      (jsown:parse json)))

