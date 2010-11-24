(defpackage #:chillax.yason
  (:use :cl :chillax.core)
  (:export :yason-server))
(in-package :chillax.yason)

(defgeneric parse-json-arrays-as-vectors-p (server))
(defgeneric parse-json-booleans-as-symbols-p (server))
(defgeneric parse-object-as-alist-p (server))
(defgeneric parse-object-key-fun (server))

(defclass yason-server (standard-server)
  ((array-as-vector-p
    :initarg :array-as-vector-p
    :reader parse-json-arrays-as-vectors-p)
   (boolean-as-symbol-p
    :initarg :boolean-as-symbol-p
    :reader parse-json-booleans-as-symbols-p)
   (object-as-alist-p
    :initarg :object-as-alist-p
    :reader parse-object-as-alist-p)
   (parse-object-key-fun
    :initarg :parse-object-key-fun
    :reader parse-object-key-fun))
  (:documentation
   "YASON-SERVERs use Yason's JSON parser/encoder to automatically translate content going to/coming
   from the associated CouchDB server.")
  (:default-initargs
      :array-as-vector-p nil
    :boolean-as-symbol-p nil
    :object-as-alist-p nil
    :parse-object-key-fun #'identity))

(defmethod data->json ((server yason-server) data &key)
  (with-output-to-string (s)
    (if (listp data)
        (yason:encode-alist data s)
        (yason:encode data s))))

(defmethod json->data ((server yason-server) json &key)
  (let ((yason:*parse-json-arrays-as-vectors* (parse-json-arrays-as-vectors-p server))
        (yason:*parse-json-booleans-as-symbols* (parse-json-booleans-as-symbols-p server))
        (yason:*parse-object-as-alist* (parse-object-as-alist-p server))
        (yason:*parse-object-key-fn* (parse-object-key-fun server))) 
    (yason:parse json)))

