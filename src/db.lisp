(in-package :chillax)

;;; util
(defun strcat (string &rest more-strings)
  "Concatenates a series of strings."
  (apply 'concatenate 'string string more-strings))

(defun mkhash (&rest keys-and-values &aux (table (make-hash-table :test #'equal)))
  "Convenience function for `literal' hash table definition."
  (loop for (key val) on keys-and-values by #'cddr do (setf (gethash key table) val)
     finally (return table)))

(defun hashget (hash key &rest more-keys)
  "Convenience function for recursively accessing hash tables."
  (flet ((reverse-gethash (hash key) (gethash key hash)))
    (reduce #'reverse-gethash more-keys :initial-value (gethash key hash))))

(defun (setf hashget) (new-value hash key &rest more-keys)
  "Uses the last key given to hashget to insert NEW-VALUE into the hash table returned by the
second-to-last key. tl;dr: DWIM SETF function for HASHGET."
  (if more-keys
      (setf (gethash (car (last more-keys))
                     (apply #'hashget hash key (butlast more-keys)))
            new-value)
      (setf (gethash key hash) new-value)))

;;;
;;; Status codes
;;;
(defparameter +status-codes+
  '((200 . :ok)
    (201 . :created)
    (202 . :accepted)
    (304 . :not-modified)
    (400 . :bad-request)
    (404 . :not-found)
    (405 . :resource-not-allowed)
    (409 . :conflict)
    (412 . :precondition-failed)
    (415 . :bad-content-type)
    (500 . :internal-server-error))
  "A simple alist of keyword names for HTTP status codes, keyed by status code.")

;;;
;;; Conditions
;;;
(define-condition couchdb-error () ())

(define-condition unexpected-response (couchdb-error)
  ((status-code :initarg :status-code :reader error-status-code)
   (response :initarg :response :reader error-response))
  (:report (lambda (condition stream)
             (format stream "Unexpected response with status code: ~A~@
                             HTTP Response: ~A"
                     (error-status-code condition)
                     (error-response condition)))))

;;;
;;; Database errors
;;;
(define-condition database-error (couchdb-error)
  ((uri :initarg :uri :reader database-error-uri)))

(define-condition db-not-found (database-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Database ~A not found." (database-error-uri condition)))))

(define-condition db-already-exists (database-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Database ~A already exists." (database-error-uri condition)))))

;;;
;;; Document errors
;;;
(define-condition document-error (couchdb-error) ())

(define-condition document-not-found (document-error)
  ((id :initarg :id :reader document-404-id)
   (db :initarg :db :reader document-404-db))
  (:report (lambda (e s)
             (format s "No document with id ~S was found in ~A"
                     (document-404-id e)
                     (document-404-db e)))))

(define-condition document-conflict (document-error)
  ((conflicting-doc :initarg :doc :reader conflicting-document)
   (conflicting-doc-id :initarg :id :reader conflicting-document-id))
  (:report (lambda (e s)
             (format s "Revision for ~A conflicts with latest revision for~@
                        document with ID ~S"
                     (conflicting-document e)
                     (conflicting-document-id e)))))

;;;
;;; Server API
;;;
(defclass server ()
  ((host 
    :initarg :host 
    :initform "127.0.0.1")
   (port 
    :initarg :port
    :initform 5984)
   (username 
    :initarg :username
    :initform NIL)
   (password
    :initarg :password
    :initform NIL))
  (:documentation
   "A SERVER is an abstraction over a CouchDB server's host, port, and basic authentication."))

(defgeneric data->json (server data)
  (:documentation "Converts DATA to JSON suitable for sending to CouchDB.")
  (:method ((server server) data)
    data))
(defgeneric json->data (server json)
  (:documentation "Converts JSON to the desired data structure.")
  (:method ((server server) json)
    json))

(defclass hash-server (server)
  ()
  (:documentation
   "HASH-SERVERs are used to dispatch couch-request in a way that will make it automatically handle
   encoding/decoding of JSON to and from hash tables."))

(defmethod data->json ((server hash-server) data)
  (with-output-to-string (s)
    (json:encode data s)))

(defmethod json->data ((server hash-server) json)
  (json:parse json))

(defun server->url (server)
  "Returns a string representation of the URL SERVER represents."
  (with-slots (host port) server
    (format nil "http://~A:~A/" host port)))

(defparameter +utf-8+ (make-external-format :utf-8 :eol-style :lf))

(defgeneric couch-request (server uri &key &allow-other-keys)
  (:documentation
   "Sends an HTTP request to the CouchDB server represented by SERVER. Most of the keyword arguments
 for drakma:http-request are available as kwargs for this message.")
  (:method ((server server) uri &rest all-keys &key (content nil contentp) &allow-other-keys)
    (multiple-value-bind (response status-code)
        (apply #'http-request (strcat (server->url server) uri)
               :content-type "application/json"
               :external-format-out +utf-8+
               :basic-authorization (with-slots (username password) server
                                      (when username (list username password)))
               :content (if contentp (data->json server content) "")
               all-keys)
      (values (json->data server response)
              (or (cdr (assoc status-code +status-codes+ :test #'=))
                  ;; The code should never get here once we know all the
                  ;; status codes CouchDB might return.
                  (error "Unknown status code: ~A. HTTP Response: ~A"
                         status-code response))))))

(defun all-dbs (server)
  "Requests a list of all existing databases from SERVER."
  (couch-request server "_all_dbs"))

(defun config-info (server)
  "Requests the current configuration from SERVER."
  (couch-request server "_config"))

(defun replicate (server source target &key create-target-p continuousp
                  &aux (to-json `("source" ,source "target" ,target)))
  "Replicates the database in SOURCE to TARGET. SOURCE and TARGET can both be either database names
in the local server, or full URLs to local or remote databases. If CREATE-TARGET-P is true, the
target database will automatically be created if it does not exist. If CONTINUOUSP is true, CouchDB
will continue propagating any changes in SOURCE to TARGET."
  ;; There are some caveats to the keyword arguments -
  ;; create-target-p: doesn't actually seem to work at all in CouchDB 0.10
  ;; continuousp: The CouchDB documentation warns that this continuous replication
  ;;              will only last as long as the CouchDB daemon is running. If the
  ;;              daemon is restarted, replication must be restarted as well.
  ;;              Note that there are plans to add 'persistent' replication.
  (when create-target-p (setf to-json (append `("create_target" "true") to-json)))
  (when continuousp (setf to-json (append `("continuous" "true") to-json)))
  (couch-request server "_replicate" :method :post :content (format nil "{~{~s:~s~^,~}}" to-json)))

(defun stats (server)
  "Requests general statistics from SERVER."
  (couch-request server "_stats"))

(defun active-tasks (server)
  "Lists all the currently active tasks on SERVER."
  (couch-request server "_active_tasks"))

(defun get-uuids (server &key (number 10))
  "Returns a list of NUMBER unique IDs requested from SERVER. The UUIDs generated by the server are
reasonably unique, but are not checked against existing UUIDs, so conflicts may still happen."
  (couch-request server (format nil "_uuids?count=~A" number)))

;;;
;;; Basic database API
;;;
(defclass database ()
  ((server 
    :initarg :server 
    :initform (make-instance 'server)
    :accessor server) 
   (name
    :initarg :name
    :accessor name))
  (:documentation
   "Base database class. These objects represent the information required in order to communicate
   with a particular CouchDB database."))

(defun db-namestring (db)
  (with-slots (server name) db
    (strcat (server->url server) name)))

(defmethod print-object ((db database) stream)
  (print-unreadable-object (db stream :type t :identity t)
    (format stream "~A" (db-namestring db))))

;; TODO - CouchDB places restrictions on what sort of URLs are accepted, such as everything having
;;        to be downcase, and only certain characters being accepted. There is also special meaning
;;        behing the use of /, so a mechanism to escape it in certain situations would be good.

(defgeneric db-request (db uri &key &allow-other-keys)
  (:documentation "Sends a CouchDB request to DB.")
  (:method ((db database) uri &rest all-keys &key &allow-other-keys)
    (apply #'couch-request (server db) (strcat (name db) "/" uri) all-keys)))

(defmacro handle-request ((result-var db uri &rest db-request-keys &key &allow-other-keys) &body expected-responses)
  "Provides a nice interface to the relatively manual, low-level status-code checking that Chillax
uses to understand CouchDB's responses. The format for EXPECTED-RESPONSES is the same as the CASE
macro: The keys should be either keywords, or lists o keywords (not evaluated), which correspond to
translated HTTP status code names. See +status-codes+ for all the currently-recognized keywords."
  (let ((status-code (gensym "STATUS-CODE-")))
    `(multiple-value-bind (,result-var ,status-code)
         (db-request ,db ,uri ,@db-request-keys)
       (case ,status-code
         ,@expected-responses
         (otherwise (error 'unexpected-response :status-code ,status-code :response ,result-var))))))

(defgeneric db-info (db)
  (:documentation "Fetches info about a given database from the CouchDB server.")
  (:method ((db database))
    (handle-request (response db "")
      (:ok response)
      (:internal-server-error (error "Illegal database name: ~A" (name db)))
      (:not-found (error 'db-not-found :uri (db-namestring db))))))

(defun db-connect (name &key (db-class 'database) server (server-class 'hash-server))
  "Confirms that a particular CouchDB database exists. If so, returns a new database object that can
be used to perform operations on it."
  (let ((db (make-instance db-class 
                           :server (or server (make-instance server-class)) 
                           :name name)))
    (when (db-info db) 
      db)))

(defun db-create (name &key (db-class 'database) server (server-class 'hash-server))
  "Creates a new CouchDB database. Returns a database object that can be used to operate on it."
  (let ((db (make-instance db-class
                           :name name
                           :server (or server (make-instance server-class)))))
    (handle-request (response db "" :method :put)
      (:created db)
      (:internal-server-error (error "Illegal database name: ~A" name))
      (:precondition-failed (error 'db-already-exists :uri (db-namestring db))))))

(defun ensure-db (name &rest all-keys &key &allow-other-keys)
  "Either connects to an existing database, or creates a new one. Returns two values: If a new
database was created, (DB-OBJECT T) is returned. Otherwise, (DB-OBJECT NIL)"
  (handler-case (values (apply #'db-create name all-keys) t)
    (db-already-exists () (values (apply #'db-connect name all-keys) nil))))

(defgeneric db-delete (db &key)
  (:documentation "Deletes a CouchDB database.")
  (:method ((db database) &key)
    (handle-request (response db "" :method :delete)
      (:ok response)
      (:not-found (error 'db-not-found :uri (db-namestring db))))))

(defgeneric db-compact (db)
  (:documentation "Triggers a database compaction.")
  (:method ((db database))
    (handle-request (response db "_compact" :method :post :content "")
      (:accepted response))))

(defgeneric db-changes (db)
  (:documentation "Returns the changes feed for DB")
  (:method ((db database))
    (handle-request (response db "_changes")
      (:ok response))))

;;;
;;; Documents
;;;
(defgeneric get-document (db id &key)
  (:documentation "Returns an CouchDB document from DB as an alist.")
  (:method ((db database) id &key key startkey endkey limit include-docs &aux params)
    (flet ((add-param (key value)
             (push (cons key (prin1-to-string value)) params)))
      (when key (add-param "key" key))
      (when startkey (add-param "startkey" startkey))
      (when endkey (add-param "endkey" endkey))
      (when limit (add-param "limit" limit))
      (when include-docs (add-param "include_docs" "true"))
      (handle-request (response db (princ-to-string id) :parameters params)
        (:ok response)
        (:not-found (error 'document-not-found :db db :id id))))))

(defgeneric all-documents (db &key)
  (:documentation "Returns all CouchDB documents in DB, in alist form.")
  (:method ((db database) &rest all-keys)
    (apply #'get-document db "_all_docs" all-keys)))

(defgeneric batch-get-documents (db &rest doc-ids)
  (:documentation "Uses _all_docs to quickly fetch the given DOC-IDs in a single request.")
  (:method ((db database) &rest doc-ids)
    (handle-request (response db "_all_docs" :method :post
                              :parameters '(("include_docs" . "true"))
                              :content (format nil "{\"keys\":[~{~S~^,~}]}" doc-ids))
      (:ok response))))

(defgeneric put-document (db id doc &key)
  (:documentation "Puts a document into DB, using ID.")
  (:method ((db database) id doc &key batch-ok-p)
    (handle-request (response db (princ-to-string id) :method :put :content doc
                              :parameters (when batch-ok-p '(("batch" . "ok"))))
      ((:created :accepted) response)
      (:conflict (error 'document-conflict :id id :doc doc)))))

(defgeneric post-document (db doc)
  (:documentation
   "POSTs a document into DB. CouchDB will automatically assign a UUID if the document does not
   already exist. Note that using this function is discouraged in the CouchDB documentation, since
   it may result in duplicate documents because of proxies and other network intermediaries.")
  (:method ((db database) doc)
    (handle-request (response db "" :method :post :content doc)
      ((:created :accepted) response)
      (:conflict (error 'document-conflict :doc doc)))))

(defgeneric delete-document (db id revision)
  (:documentation "Deletes an existing document.")
  (:method ((db database) id revision)
    (handle-request (response db (format nil "~A?rev=~A" id revision) :method :delete)
      (:ok response))))

(defgeneric copy-document (db from-id to-id &key)
  (:documentation "Copies a document's content in-database.")
  (:method ((db database) from-id to-id &key revision)
    (handle-request (response db (princ-to-string from-id) :method :copy
                              :additional-headers `(("Destination" . ,(princ-to-string to-id)))
                              :parameters `(,(when revision `("rev" . ,revision))))
      (:created response))))
