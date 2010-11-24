(in-package :chillax.core)

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
;;; Direct Document API
;;;
(defun get-document (db id &key key startkey endkey limit include-docs &aux params)
  "Finds a CouchDB document in DB, named by ID."
  (flet ((add-param (key value)
           (push (cons key (prin1-to-string value)) params)))
    (when key (add-param "key" key))
    (when startkey (add-param "startkey" startkey))
    (when endkey (add-param "endkey" endkey))
    (when limit (add-param "limit" limit))
    (when include-docs (add-param "include_docs" "true"))
    (handle-request (response db (princ-to-string id) :parameters params)
      (:ok response)
      (:not-found (error 'document-not-found :db db :id id)))))

(defun all-documents (db &rest all-keys)
  "Requests the \_all\_docs document. ALL-KEYS correspond to GET-DOCUMENT's keyword arguments."
  (apply #'get-document db "_all_docs" all-keys))

(defun batch-get-documents (db &rest doc-ids)
  "Uses _all_docs to quickly fetch the given DOC-IDs in a single request. Note that this function
will NOT signal a DOCUMENT-NOT-FOUND error when one or more DOC-IDs are not found. Instead, the
results will be returned, and it's the user's responsibility to deal with any missing docs."
  (handle-request (response db "_all_docs" :method :post
                            :parameters '(("include_docs" . "true"))
                            :content (format nil "{\"keys\":[~{~S~^,~}]}" doc-ids)
                            :convert-data-p nil)
    (:ok response)))


(defun put-document (db id doc &key batch-ok-p)
  "Puts a document into DB, using ID."
  (handle-request (response db (princ-to-string id) :method :put :content doc
                            :parameters (when batch-ok-p '(("batch" . "ok"))))
    ((:created :accepted) response)
    (:conflict (error 'document-conflict :id id :doc doc))))

(defun post-document (db doc)
  "POSTs a document into DB. CouchDB will automatically assign a UUID if the document does not
already exist. Note that using this function is discouraged in the CouchDB documentation, since it
may result in duplicate documents because of proxies and other network intermediaries."
  (handle-request (response db "" :method :post :content doc)
    ((:created :accepted) response)
    (:conflict (error 'document-conflict :doc doc))))

(defun delete-document (db id revision)
  "Deletes an existing document."
  (handle-request (response db (format nil "~A?rev=~A" id revision) :method :delete)
    (:ok response)
    (:not-found (error 'document-not-found :db db :id id))))

(defun copy-document (db from-id to-id &key revision)
  "Copies a document's content in-database."
  (handle-request (response db (princ-to-string from-id) :method :copy
                            :additional-headers `(("Destination" . ,(princ-to-string to-id)))
                            :parameters `(,(when revision `("rev" . ,revision))))
    (:created response)
    (:not-found (error 'document-not-found :db db :id from-id))))
