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
    (if (parse-object-as-alist-p server)
        (yason:encode-alist data s)
        (yason:encode data s))))

(defmethod json->data ((server yason-server) json &key)
  (yason:parse json
               :object-key-fn (parse-object-key-fun server)
               :object-as (if (parse-object-as-alist-p server) :alist :hash-table)
               :json-booleans-as-symbols (parse-json-booleans-as-symbols-p server)
               :json-arrays-as-vectors (parse-json-arrays-as-vectors-p server)))

