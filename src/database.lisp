(in-package :chillax)

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

;; TODO - CouchDB places restrictions on what sort of URLs are accepted, such as everything having
;;        to be downcase, and only certain characters being accepted. There is also special meaning
;;        behing the use of /, so a mechanism to escape it in certain situations would be good.

;; Database protocol
(defgeneric database-server (database))
(defgeneric database-name (database))

(defun print-database (db stream)
  (print-unreadable-object (db stream :type t :identity t)
    (format stream "~A" (db-namestring db))))

(defgeneric db-request (db uri &key &allow-other-keys)
  (:documentation "Sends a CouchDB request to DB.")
  (:method (db uri &rest all-keys &key &allow-other-keys)
    (apply #'couch-request (database-server db) (strcat (database-name db) "/" uri) all-keys)))

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
  (:method (db)
    (handle-request (response db "")
      (:ok response)
      (:internal-server-error (error "Illegal database name: ~A" (database-name db)))
      (:not-found (error 'db-not-found :uri (db-namestring db))))))

(defun db-connect (name &key (db-class 'standard-database) server (server-class 'hash-server))
  "Confirms that a particular CouchDB database exists. If so, returns a new database object that can
be used to perform operations on it."
  (let ((db (make-instance db-class
                           :server (or server (make-instance server-class))
                           :name name)))
    (when (db-info db)
      db)))

(defun db-create (name &key (db-class 'standard-database) server (server-class 'hash-server))
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
  (:method (db &key)
    (handle-request (response db "" :method :delete)
      (:ok response)
      (:not-found (error 'db-not-found :uri (db-namestring db))))))

(defgeneric db-compact (db)
  (:documentation "Triggers a database compaction.")
  (:method (db)
    (handle-request (response db "_compact" :method :post :content "")
      (:accepted response))))

(defgeneric db-changes (db)
  (:documentation "Returns the changes feed for DB")
  (:method (db)
    (handle-request (response db "_changes")
      (:ok response))))

(defun db-namestring (db)
  (strcat (server->url (database-server db)) (database-name db)))

;;;
;;; Convenience implementation
;;;
(defclass standard-database ()
  ((server
    :initarg :server
    :initform (make-instance 'server)
    :reader database-server)
   (name
    :initarg :name
    :reader database-name))
  (:documentation
   "Base database class. These objects represent the information required in order to communicate
   with a particular CouchDB database."))

(defmethod print-object ((db standard-database) stream)
  (print-database db stream))
