(in-package :chillax.core)

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

(defparameter +utf-8+ (make-external-format :utf-8 :eol-style :lf))

;;;
;;; Conditions
;;;
(define-condition couchdb-error () ())

(define-condition unexpected-response (couchdb-error)
  ((status-code :initarg :status-code :reader error-status-code)
   (response :initarg :response :reader error-response))
  (:report (lambda (condition stream)
             (format stream "Unexpected response with status code: ~A~@
                             HTTP Response: ~A~@
                             Please report this to Chillax's maintainer(s)"
                     (error-status-code condition)
                     (error-response condition)))))

;;;
;;; Server API
;;;

;; Server Protocol
(defgeneric server-host (server))
(defgeneric server-port (server))
(defgeneric server-username (server))
(defgeneric server-password (server))
(defgeneric server-secure-p (server))
(defgeneric data->json (server data &key)
  (:documentation "Converts DATA to JSON suitable for sending to CouchDB."))
(defgeneric json->data (server json &key)
  (:documentation "Converts JSON to the desired data structure."))
(defgeneric make-db-object (server name)
  (:documentation
"Creates an object which represents a database connection in SERVER. The object must conform to the
database protocol."))

(defun couch-request (server uri &rest all-keys
                      &key (content nil contentp) (convert-data-p t)
                      &allow-other-keys)
  "Sends an HTTP request to the CouchDB server represented by SERVER. Most of the keyword arguments
for drakma:http-request are available as kwargs for this message."
  (let* ((content (cond ((and contentp convert-data-p)
                        (data->json server content))
                       ((and contentp (not convert-data-p))
                        content)
                       (t "")))
         (content-length (or (getf all-keys :content-length)
                             (flex:octet-length content :external-format +utf-8+))))
    (remf all-keys :content-length)
    (remf all-keys :convert-data-p)
    (multiple-value-bind (response status-code)
        (apply #'http-request (strcat (server-uri server) uri)
               :content-type "application/json"
               :external-format-out +utf-8+
               :external-format-in +utf-8+
               :basic-authorization (when (server-username server)
                                      (list (server-username server)
                                            (server-password server)))
               :content content
               :content-length content-length
               all-keys)
      (values (json->data server response)
              (or (cdr (assoc status-code +status-codes+ :test #'=))
                  ;; The code should never get here once we know all the
                  ;; status codes CouchDB might return.
                  (error "Unknown status code: ~A. HTTP Response: ~A"
                         status-code response))))))

;; Server functions
(defun server-uri (server)
  "Returns a string representation of the URL SERVER represents."
  (format nil "~A://~A:~A/"
          (if (server-secure-p server)
              "https"
              "http")
          (server-host server)
          (server-port server)))

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
;;; Sample protocol implementation
;;;
(defclass standard-server ()
  ((host
    :reader server-host
    :initarg :host)
   (port
    :reader server-port
    :initarg :port)
   (username
    :reader server-username
    :initarg :username)
   (password
    :reader server-password
    :initarg :password)
   (securep
    :reader server-secure-p
    :initarg :securep))
  (:documentation
   "Default implementation of the server protocol.")
  (:default-initargs
      :host "127.0.0.1"
    :port 5984
    :username nil
    :password nil
    :securep nil))

(defmethod data->json ((server standard-server) data &key)
  data)
(defmethod json->data ((server standard-server) json &key)
  json)
