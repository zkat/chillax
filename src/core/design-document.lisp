(in-package :chillax.core)

;;;
;;; Design Doc errors
;;;
(define-condition view-not-found (couchdb-error)
  ((view       :initarg :view :reader view-404-view)
   (design-doc :initarg :ddoc :reader view-404-design-document)
   (db         :initarg :db   :reader view-404-db))
  (:report (lambda (e s)
             (format s "No view \"_design/~A/_view/~A\" was found in ~A"
                     (view-404-design-document e)
                     (view-404-view e)
                     (view-404-db e)))))

;;;
;;; Design Doc basics
;;;
(defun view-cleanup (db)
  "Invokes _view_cleanup on DB. Old view output will remain on disk until this is invoked."
  (handle-request (response db "_view_cleanup" :method :post)
    (:accepted response)))

(defun compact-design-doc (db design-doc-name)
  "Compaction can really help when you have very large views, very little space, or both."
  (handle-request (response db (strcat "_compact/" design-doc-name) :method :post)
    (:accepted response)
    (:not-found (error 'document-not-found :db db :id (strcat "_design/" design-doc-name)))))

(defun design-doc-info (db design-doc-name)
  "Returns an object with various bits of status information. Refer to CouchDB documentation for
specifics on each value."
  (let ((ddoc (strcat "_design/" design-doc-name)))
    (handle-request (response db (strcat ddoc "/_info"))
      (:ok response)
      (:not-found (error 'document-not-found :db db :id ddoc)))))

(defun build-view-params (database &key
                          (key nil key-given)
                          (startkey nil startkey-given)
                          (endkey nil endkey-given)
                          startkey-docid endkey-docid limit skip
                          (descendingp nil descendingpp)
                          (groupp nil grouppp) group-level
                          (reducep t reducepp) stalep
                          (include-docs-p nil include-docs-p-p)
                          (inclusive-end-p t inclusive-end-p-p)
                          &allow-other-keys)
  (let ((params ()))
    (labels ((%param (key value)
               (push (cons key (princ-to-string value)) params))
             (maybe-param (test name value)
               (when test (%param name value)))
             (param (name value)
               (maybe-param value name value))
             (encode (value)
               (data->json (database-server database) value)))
      (maybe-param key-given "key" (encode key))
      (maybe-param startkey-given "startkey" (encode startkey))
      (maybe-param endkey-given "endkey" (encode endkey))
      (maybe-param inclusive-end-p-p "inclusive_end" (if inclusive-end-p "true" "false"))
      (param "startkey_docid" startkey-docid)
      (param "endkey_docid" endkey-docid)
      (param "limit" limit)
      (maybe-param stalep "stale" "ok")
      (maybe-param descendingpp "descending" (if descendingp "true" "false"))
      (param "skip" skip)
      (maybe-param grouppp "group" (if groupp "true" "false"))
      (param "group_level" group-level)
      (maybe-param reducepp "reduce" (if reducep "true" "false"))
      (maybe-param include-docs-p-p "include_docs" (if include-docs-p "true" "false")))
    params))

(defun query-view (db design-doc-name view-name &rest all-keys
                   &key key startkey startkey-docid endkey
                   multi-keys endkey-docid limit skip
                   descendingp groupp group-level
                   reducep stalep include-docs-p
                   inclusive-end-p)
  "Queries view named by VIEW-NAME in DESIGN-DOC-NAME. Keyword arguments correspond to CouchDB view
query arguments.

  * key - Single key to search for.
  * multi-keys - Multiple keys to search for.
  * startkey - When searching for a range of keys, the key to start from.
  * endkey - When searching for a range of keys, the key to end at. Whether this is inclusive or not
    depends on inclusive-end-p (default: true)
  * inclusive-end-p - If TRUE, endkey is included in the result. (default: true)
  * startkey-docid - Like startkey, but keyed on the result documents' doc-ids.
  * endkey-docid - Like endkey, but keyed on the result documents' doc-ids.
  * limit - Maximum number of results to return.
  * stalep - If TRUE, CouchDB will not refresh the view, even if it is stalled. (default: false)
  * descendingp - If TRUE, will return reversed results. (default: false)
  * skip - Number of documents to skip while querying.
  * groupp - Controls whether the reduce function reduces to a set of distinct keys, or to a single
    result row.
  * group-level - It's complicated. Google it!
  * reducep - If FALSE, return the view without applying its reduce function (if any). (default: true)
  * include-docs-p - If TRUE, includes the entire document with the result of the query. (default: false)"
  (declare (ignore key startkey startkey-docid endkey endkey-docid limit skip descendingp
                   groupp group-level reducep stalep include-docs-p inclusive-end-p))
  (let ((params (apply #'build-view-params db all-keys))
        (doc-name (strcat "_design/" design-doc-name "/_view/" view-name)))
    (if multi-keys
        (let* ((server (database-server db))
               (content (with-output-to-string (s)
                          (write-string "{\"keys\":[" s)
                          (mapl (lambda (kl)
                                  (write-string (data->json server (car kl)) s)
                                  (unless (null (cdr kl))
                                    (write-string "," s)))
                                multi-keys)
                          (write-string "]}" s))))
          ;; If we receive the MULTI-KEYS argument, we have to do a POST instead.
          (handle-request (response db doc-name :method :post
                                    :parameters params
                                    :content content
                                    :convert-data-p nil)
            (:ok response)
            (:not-found (error 'view-not-found :db db :view view-name :ddoc design-doc-name))))
        (handle-request (response db doc-name :parameters params)
          (:ok response)
          (:not-found (error 'view-not-found :db db :view view-name :ddoc design-doc-name))))))

;;;
;;; Views
;;;
(defun query-temporary-view (db &rest all-keys &key (language "javascript") reduce
                             (map (error "Must provide a map function for temporary views."))
                             key startkey startkey-docid endkey
                             endkey-docid limit skip
                             descendingp groupp group-level
                             reducep stalep include-docs-p
                             inclusive-end-p)
  "Queries a temporary view. These views are meant to be for testing and development purposes, and
should _not_ be used in actual code."
  ;; I'm not sure CouchDB actually accepts all the view parameters for temporary views...
  (declare (ignore key startkey startkey-docid endkey endkey-docid limit skip descendingp
                   groupp group-level reducep stalep include-docs-p inclusive-end-p))
  (let ((json (with-output-to-string (s)
                (format s "{")
                (format s "\"language\":~S" language)
                (format s ",\"map\":~S" map)
                (when reduce
                  (format s ",\"reduce\":~S" reduce))
                (format s "}")))
        (params (apply #'build-view-params db all-keys)))
    (handle-request (response db "_temp_view" :method :post
                              :parameters params
                              :content json
                              :convert-data-p nil)
      (:ok response))))
