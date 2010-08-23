(in-package :chillax)

;;;
;;; Standard convenience implementations for Chillax protocols.
;;;

;;; Server
(defclass standard-server ()
  ((host
    :reader server-host
    :initarg :host
    :initform "127.0.0.1")
   (port
    :reader server-port
    :initarg :port
    :initform 5984)
   (username
    :reader server-username
    :initarg :username
    :initform nil)
   (password
    :reader server-password
    :initarg :password
    :initform nil)
   (securep
    :reader server-secure-p
    :initarg :securep
    :initform nil))
  (:documentation
   "Default implementation of the server protocol."))

(defmethod data->json ((server standard-server) data)
  data)
(defmethod json->data ((server standard-server) json)
  json)

(defclass hash-server ()
  ()
  (:documentation
   "HASH-SERVERs are used to dispatch couch-request in a way that will make it automatically handle
   encoding/decoding of JSON to and from hash tables."))

(defmethod data->json ((server hash-server) data)
  (with-output-to-string (s)
    (json:encode data s)))

(defmethod json->data ((server hash-server) json)
  (json:parse json))

(defclass standard-hash-server (standard-server hash-server) ())

;;; database
(defclass standard-database ()
  ((server
    :initarg :server
    :reader database-server)
   (name
    :initarg :name
    :reader database-name))
  (:documentation
   "Base database class. These objects represent the information required in order to communicate
   with a particular CouchDB database."))

(defmethod print-object ((db standard-database) stream)
  (print-database db stream))

(defmethod make-db-object ((server standard-server) name)
  (make-instance 'standard-database :server server :name name))
