(in-package :chillax)

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
