(in-package :chillax.core)

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
;;; Basic database API
;;;

;; Database protocol
(defgeneric database-server (database)
  (:documentation "Returns the server object with which DATABASE is associated."))
(defgeneric database-name (database)
  (:documentation
   "Returns the URL-encoded name of the database, a string. Note that CouchDB accepts certain
   characters in database names -only- if they are URL-encoded (such as #\/). It is up to individual
   implementations of DATABASE-NAME to implement this encoding."))

;; Database functions
(defun print-database (db stream)
  "Objects implementing the database protocol may use this function in their PRINT-OBJECT method."
  (print-unreadable-object (db stream :type t :identity t)
    (format stream "~A" (db-uri db))))

(defun db-request (db uri &rest all-keys)
  "Sends a CouchDB request to DB."
  (apply #'couch-request (database-server db) (strcat (database-name db) "/" uri) all-keys))

(defmacro handle-request ((result-var db uri &rest db-request-keys &key &allow-other-keys)
                          &body expected-responses)
  "Provides a nice interface to the relatively manual, low-level status-code checking that Chillax
uses to understand CouchDB's responses. The format for EXPECTED-RESPONSES is the same as the CASE
macro: The keys should be either keywords, or lists o keywords (not evaluated), which correspond to
translated HTTP status code names. See +status-codes+ for all the currently-recognized keywords."
  (let ((status-code (gensym "STATUS-CODE-")))
    `(multiple-value-bind (,result-var ,status-code)
         (db-request ,db ,uri ,@db-request-keys)
       (case ,status-code
         ,@expected-responses
         (:bad-request (error "Bad request: ~A" ,result-var))
         (otherwise (error 'unexpected-response :status-code ,status-code :response ,result-var))))))

(defun db-info (db)
  "Fetches info about a given database from the CouchDB server."
  (handle-request (response db "")
    (:ok response)
    (:internal-server-error (error "Illegal database name: ~A" (database-name db)))
    (:not-found (error 'db-not-found :uri (db-uri db)))))

(defun db-connect (server name)
  "Confirms that a particular CouchDB database exists. If so, returns a new database object that can
be used to perform operations on it. Will signal a DB-NOT-FOUND error if the database does not
already exist."
  (let ((db (make-db-object server name)))
    (when (db-info db)
      db)))

(defun db-create (server name)
  "Creates a new CouchDB database. Returns a database object that can be used to operate on it. Will
signal a DB-ALREADY-EXISTS error if there is already a database with the same NAME in SERVER."
  (let ((db (make-db-object server name)))
    (handle-request (response db "" :method :put)
      (:created db)
      (:internal-server-error (error "Illegal database name: ~A" name))
      (:precondition-failed (error 'db-already-exists :uri (db-uri db))))))

(defun ensure-db (server name)
  "Either connects to an existing database, or creates a new one. Returns two values: If a new
database was created, (DB-OBJECT T) is returned. Otherwise, (DB-OBJECT NIL)"
  (handler-case (values (db-create server name) t)
    (db-already-exists () (values (db-connect server name) nil))))

(defun db-delete (db)
  "Deletes a CouchDB database."
  (handle-request (response db "" :method :delete)
    (:ok response)
    (:not-found (error 'db-not-found :uri (db-uri db)))))

(defun db-compact (db)
  "Triggers a database compaction."
  (handle-request (response db "_compact" :method :post :content "")
    (:accepted response)))

(defun db-changes (db)
  "Returns the changes feed for DB"
  (handle-request (response db "_changes")
    (:ok response)))

(defun db-uri (db)
  "Returns a string representing the full URI for DB."
  (strcat (server-uri (database-server db)) (database-name db)))

;;;
;;; Sample protocol implementation
;;;
(defclass standard-database ()
  ((server :reader database-server :initarg :server)
   (name :reader database-name))
  (:documentation
   "Minimal, class-based implementation of the database protocol."))

(defun url-encode (string)
  (drakma:url-encode string :utf-8))

(defmethod initialize-instance :after ((db standard-database) &key name)
  (setf (slot-value db 'name) (url-encode name)))

(defmethod print-object ((db standard-database) stream)
  (print-database db stream))

(defmethod make-db-object ((server standard-server) name)
  (make-instance 'standard-database :server server :name name))
