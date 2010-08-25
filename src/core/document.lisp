(in-package :chillax)

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
(defgeneric get-document (db id &key)
  (:documentation "Returns an CouchDB document from DB as an alist.")
  (:method (db id &key key startkey endkey limit include-docs &aux params)
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
  (:method (db &rest all-keys)
    (apply #'get-document db "_all_docs" all-keys)))

(defgeneric batch-get-documents (db &rest doc-ids)
  (:documentation
   "Uses _all_docs to quickly fetch the given DOC-IDs in a single request. Note that this function
   will NOT signal a DOCUMENT-NOT-FOUND error when one or more DOC-IDs are not found. Instead, the
   results will be returned, and it's the user's responsibility to deal with any missing docs.")
  (:method (db &rest doc-ids)
    (handle-request (response db "_all_docs" :method :post
                              :parameters '(("include_docs" . "true"))
                              :content (format nil "{\"keys\":[~{~S~^,~}]}" doc-ids)
                              :convert-data-p nil)
      (:ok response))))

(defgeneric put-document (db id doc &key batch-ok-p)
  (:documentation "Puts a document into DB, using ID.")
  (:method (db id doc &key batch-ok-p)
    (handle-request (response db (princ-to-string id) :method :put :content doc
                              :parameters (when batch-ok-p '(("batch" . "ok"))))
      ((:created :accepted) response)
      (:conflict (error 'document-conflict :id id :doc doc)))))

(defgeneric post-document (db doc)
  (:documentation
   "POSTs a document into DB. CouchDB will automatically assign a UUID if the document does not
   already exist. Note that using this function is discouraged in the CouchDB documentation, since
   it may result in duplicate documents because of proxies and other network intermediaries.")
  (:method (db doc)
    (handle-request (response db "" :method :post :content doc)
      ((:created :accepted) response)
      (:conflict (error 'document-conflict :doc doc)))))

(defgeneric delete-document (db id revision)
  (:documentation "Deletes an existing document.")
  (:method (db id revision)
    (handle-request (response db (format nil "~A?rev=~A" id revision) :method :delete)
      (:ok response))))

(defgeneric copy-document (db from-id to-id &key)
  (:documentation "Copies a document's content in-database.")
  (:method (db from-id to-id &key revision)
    (handle-request (response db (princ-to-string from-id) :method :copy
                              :additional-headers `(("Destination" . ,(princ-to-string to-id)))
                              :parameters `(,(when revision `("rev" . ,revision))))
      (:created response))))

;;;
;;; Document object API
;;;
(defun save-document (doc &key batch-ok-p)
  (let* ((response (put-document (at doc "_database") (at doc "_id")
                                 doc :batch-ok-p batch-ok-p))
         (revision (at response "rev")))
    (setf (at doc "_rev") revision)
    doc))

(defun update-document (doc)
  "Updates DOC with the most recent data from the database."
  (let* ((db (at doc "_database"))
         (id (at doc "_id"))
         (new-doc (get-document db id)))
    (if (equal (at doc "_rev") (at new-doc "_rev"))
        doc
        (%update-document doc new-doc))))

(defgeneric %update-document (old-doc new-doc)
  (:method ((old-doc hash-table) new-doc)
    (let ((db (at old-doc "_database")))
      (clrhash old-doc)
      (maphash (lambda (key value)
                 (setf (at new-doc key) value))
               old-doc)
      (setf (at old-doc "_database") db)
      old-doc))
  (:method ((old-doc list) new-doc)
    (let ((db (at old-doc "_database")))
      (setf (car old-doc) (car new-doc)
            (cdr old-doc) (cdr new-doc)
            (at old-doc "_database") db)
      old-doc)))
