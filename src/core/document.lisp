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
(defun get-document-revision (db doc-id &key (errorp t))
  "Quickly fetches the latest revision for DOC-ID. If ERRORP is NIL, this can be used to quickly
test the existence of a document."
  (multiple-value-bind (x status-code headers)
      (http-request (strcat (db-uri db) "/" (url-encode (princ-to-string doc-id)))
                    :method :head)
    (declare (ignore x))
    (case (or (cdr (assoc status-code +status-codes+ :test #'=))
              (error "Unknown status code: ~A." status-code))
      (:ok (dequote (cdr (assoc :etag headers))))
      (:not-found (when errorp (error 'document-not-found :db db :id doc-id))))))

(defun get-document (db id &key attachmentsp (errorp t) params)
  "Finds a CouchDB document in DB, named by ID. PARAMS should be an alist containing the parameters
for the HTTP GET request. If ATTACHMENTSP is TRUE, the document's attachments will be included in
their entirety in their base64-encoded version. It is not recommended you use this unless you really
know what you're doing. If ERRORP is NIL, GET-DOCUMENT will simply return NIL on 404."
  (handle-request (response db (url-encode (princ-to-string id))
                            :parameters (if attachmentsp
                                            (cons (cons "attachments" "true") params)
                                            params))
    (:ok response)
    (:not-found (when errorp (error 'document-not-found :db db :id id)))))

(defun put-document (db id doc &key batch-ok-p)
  "Puts a document into DB, using ID."
  (handle-request (response db (url-encode (princ-to-string id)) :method :put :content doc
                            :parameters (when batch-ok-p '(("batch" . "ok"))))
    ((:created :accepted) response)
    (:conflict (error 'document-conflict :id id :doc doc))))

(defun post-document (db doc)
  "POSTs a document into DB. CouchDB will automatically assign a UUID if the document does not
already exist. Note that using this function is discouraged in the CouchDB documentation, since it
may result in duplicate documents because of proxies and other network intermediaries. If what you
need is to create a new document with a generated id, consider using GET-UUIDS with PUT-DOCUMENT."
  (handle-request (response db "" :method :post :content doc)
    ((:created :accepted) response)
    (:conflict (error 'document-conflict :doc doc))))

(defun delete-document (db id revision)
  "Deletes an existing document."
  (handle-request (response db
                            (strcat (url-encode (princ-to-string id))
                                    "?rev="
                                    (url-encode (princ-to-string revision)))
                            :method :delete)
    (:ok response)
    (:not-found (error 'document-not-found :db db :id id))))

(defun copy-document (db from-id to-id &key revision)
  "Copies a document's content in-database."
  (handle-request (response db (url-encode (princ-to-string from-id)) :method :copy
                            :additional-headers `(("Destination" . ,(princ-to-string to-id)))
                            :parameters `(,(when revision `("rev" . ,revision))))
    (:created response)
    (:not-found (error 'document-not-found :db db :id from-id))))

;;;
;;; Bulk Document API
;;;
(defun all-documents (db &rest all-keys)
  "Requests the _all_docs document. ALL-KEYS correspond to GET-DOCUMENT's keyword arguments."
  (apply #'get-document db "_all_docs" all-keys))

(defun batch-get-documents (db doc-ids)
  "Uses _all_docs to quickly fetch the given DOC-IDs in a single request. Note that this function
will NOT signal a DOCUMENT-NOT-FOUND error when one or more DOC-IDs are not found. Instead, the
results will be returned, and it's the user's responsibility to deal with any missing docs."
  (handle-request (response db "_all_docs" :method :post
                            :parameters '(("include_docs" . "true"))
                            :content (format nil "{\"keys\":~S}"
                                             (data->json (database-server db) doc-ids))
                            :convert-data-p nil)
    (:ok response)))

(defun bulk-post-documents (db documents &key all-or-nothing-p)
  "Allows you to update or submit multiple documents at the same time, using CouchDB's _bulk_docs
API. In order to delete a document through this API, the document must have a _document attribute
with JSON 'true' as its value (note that what gets translated into 'true' depends on the server).

DOCUMENTS must be a sequence or sequence-like (depending on what DATA->JSON will do to it).

If ALL-OR-NOTHING-P is true, the entire submission will fail if a single one fails."
  (let ((as-json (data->json (database-server db) documents)))
    (handle-request (response db "_bulk_docs" :method :post
                              :content (with-output-to-string (s)
                                         (princ "{\"docs\":" s)
                                         (princ as-json s)
                                         (when all-or-nothing-p
                                           (princ ",\"all_or_nothing\":true" s))
                                         (princ "}" s))
                              :convert-data-p nil)
      ((:ok :accepted :created) response))))

;;;
;;; Standalone Attachments
;;;
(defun put-attachment (db doc-id attachment-name data &key rev (content-type "application/octet-stream"))
  "Adds DATA as an attachment. DATA can be a number of things:

  * String or sequence of octets - DATA will be sent as-is directly to the server (using
    EXTERNAL-FORMAT-OUT for strings).
  * Stream - The stream will be read until EOF is reached.
  * Pathname - The file the pathname denotes will be opened and its data uploaded.
  * Function designator - The corresponding function will be called with one argument, the
    stream to the server, to which it should send data.

If the document already exists, REV is required. This function can be used on non-existent
documents. If so, REV is not needed, and a document will be created automatically, and the
attachment associated with it.

The CONTENT-TYPE should be a string specifying the content type for DATA."
  (multiple-value-bind (response status-code)
      (http-request (strcat (server-uri (database-server db))
                            "/"
                            (database-name db)
                            "/"
                            (url-encode (princ-to-string doc-id))
                            "/"
                            attachment-name)
                    :method :put
                    :parameters (when rev
                                  `(("rev" . ,rev)))
                    :content data
                    :content-type content-type)
    (case (or (cdr (assoc status-code +status-codes+ :test #'=))
              (error "Unknown status code: ~A. HTTP Response: ~A"
                     status-code response))
      (:ok (json->data (database-server db) response))
      (:created (json->data (database-server db) response))
      (:not-found (error 'document-not-found :db db :id doc-id))
      (otherwise (error 'unexpected-response :status-code status-code :response response)))))

(defun get-attachment (db doc-id attachment-name)
  "Returns 3 values:

  1. STREAM - An open flexi-stream that can be READ. In order to read straight binary data, you must
              first fetch the underlying stream with FLEXI-STREAMS:FLEXI-STREAM-STREAM.
  2. MUST-CLOSE-P - A boolean. If TRUE, the user must CLOSE this stream themselves
     once reading is done.
  3. CONTENT-LENGTH - Declared content length for the incoming data."
  (multiple-value-bind (response status-code headers fourth fifth must-close-p)
      (http-request (strcat (server-uri (database-server db))
                            "/"
                            (database-name db)
                            "/"
                            (url-encode (princ-to-string doc-id))
                            "/"
                            attachment-name)
                    :want-stream t)
    (declare (ignore fourth fifth))
    (case (or (cdr (assoc status-code +status-codes+ :test #'=))
              (error "Unknown status code: ~A. HTTP Response: ~A"
                     status-code response))
      (:ok (values response must-close-p
                   (let ((content-length (cdr (assoc :content-length headers))))
                     (when content-length (parse-integer content-length)))))
      (:not-found (error 'document-not-found :db db :id doc-id))
      (otherwise (when (streamp response) (close response))
                 (error 'unexpected-response :status-code status-code :response response)))))

(defun delete-attachment (db doc-id attachment-name doc-revision)
  "Deletes an attachment from a document. DOC-REVISION must be the latest revision for the document."
  (multiple-value-bind (response status-code)
      (http-request (strcat (server-uri (database-server db))
                            "/"
                            (database-name db)
                            "/"
                            (url-encode (princ-to-string doc-id))
                            "/"
                            attachment-name)
                    :method :delete
                    :parameters `(("rev" . ,doc-revision)))
    (case (or (cdr (assoc status-code +status-codes+ :test #'=))
              (error "Unknown status code: ~A. HTTP Response: ~A"
                     status-code response))
      (:ok (json->data (database-server db) response))
      (:not-found (error 'document-not-found :db db :id doc-id))
      (otherwise (error 'unexpected-response :status-code status-code :response response)))))

(defun copy-attachment (db doc-id attachment-name output-stream &key (max-buffer-size 4096))
  "Copies data from the named attachment to OUTPUT-STREAM. Returns the number of bytes copied."
  (multiple-value-bind (attachment-stream must-close-p content-length)
      (get-attachment db doc-id attachment-name)
    (unwind-protect
         (if (plusp content-length)
             (copy-stream (flex:flexi-stream-stream attachment-stream) output-stream
                          :buffer-size (mod content-length max-buffer-size))
             0)
      (when must-close-p
        (close attachment-stream)))))
