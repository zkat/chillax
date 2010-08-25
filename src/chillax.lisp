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

(defmethod data->json ((server standard-server) data &key)
  data)
(defmethod json->data ((server standard-server) json &key)
  json)

(defclass hash-server (standard-server)
  ()
  (:documentation
   "HASH-SERVERs are used to dispatch couch-request in a way that will make it automatically handle
   encoding/decoding of JSON to and from hash tables."))

(defmethod data->json ((server hash-server) data &key)
  (with-output-to-string (s)
    (json:encode data s)))

(defmethod json->data ((server hash-server) json &key)
  (json:parse json))

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

;;;
;;; Design-doc/view helper.
;;;
(defun make-view (map-def &optional reduce-def)
  (let ((view (mkhash "map" map-def)))
    (when reduce-def (setf (at view "reduce") reduce-def))
    view))

(defgeneric view-map-definition (view)
  (:method ((view hash-table))
    (at view "map")))
(defgeneric view-reduce-definition (view)
  (:method ((view hash-table))
    (at view "reduce")))

(defun get-temporary-view (db view &optional (language "common-lisp"))
  "A temporary view needs at least a \"map\" field. \"language\":\"foo\" is necessary if the view is
  supposed to be in any language other than JavaScript."
  (let ((json (with-output-to-string (s)
                (format s "{")
                (format s "\"map\":~S" (view-map-definition view))
                (when (view-reduce-definition view)
                  (format s ",\"reduce\":~S" (view-reduce-definition view)))
                (format s ",\"language\":~S" language)
                (format s "}"))))
    (handle-request (response db "_temp_view" :method :post
                              :content json
                              :convert-data-p nil)
      (:ok response))))

(defun view-cleanup (db)
  (handle-request (response db "_view_cleanup" :method :post)
    (:accepted response)))

(defun compact-design-doc (db design-doc-name)
  (handle-request (response db (strcat "_compact/" design-doc-name) :method :post)
    (:accepted response)))

(defun design-doc-info (db design-doc-name)
  (handle-request (response db (strcat "_design/" design-doc-name "/_info"))
    (:ok response)))
