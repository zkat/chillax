(in-package :chillax.core)

;;;
;;; Design Doc basics
;;;
(defun view-cleanup (db)
  (handle-request (response db "_view_cleanup" :method :post)
    (:accepted response)))

(defun compact-design-doc (db design-doc-name)
  (handle-request (response db (strcat "_compact/" design-doc-name) :method :post)
    (:accepted response)
    (:not-found (error 'document-not-found :db db :id design-doc-name))))

(defun design-doc-info (db design-doc-name)
  (handle-request (response db (strcat "_design/" design-doc-name "/_info"))
    (:ok response)
    (:not-found (error 'document-not-found :db db :id design-doc-name))))

(defun invoke-view (db design-doc-name view-name &key errorp
                    key startkey startkey-docid endkey
                    endkey-docid limit skip
                    (descendingp nil descendingpp)
                    (groupp nil grouppp) group-level
                    (reducep t reducepp) stalep
                    (include-docs-p nil include-docs-p-p)
                    (inclusive-end-p t inclusive-end-p-p)
                    &aux params)
  "Invokes view named by VIEW-NAME in DESIGN-DOC-NAME. Keyword arguments correspond to CouchDB view
query arguments.

  * key - Single key to search for.
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
  (let ((server (database-server db)))
    (labels ((%param (key value)
               (push (cons key value) params))
             (maybe-param (test key value)
               (when test (%param key value)))
             (param (name value)
               (when value (%param name (data->json server value)))))
      (param "key" key)
      (param "startkey" startkey)
      (param "endkey" endkey)
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
    (get-document db (strcat "_design/" design-doc-name "_view/" view-name)
                  :errorp errorp :params params)))

;;;
;;; Views
;;;
(defun invoke-temporary-view (db &key
                              (language "common-lisp")
                              (map (error "Must provide a map function for temporary views."))
                              reduce)
  (let ((json (with-output-to-string (s)
                (format s "{")
                (format s ",\"language\":~S" language)
                (format s "\"map\":~S" map)
                (when reduce
                  (format s ",\"reduce\":~S" reduce))
                (format s "}"))))
    (handle-request (response db "_temp_view" :method :post
                              :content json
                              :convert-data-p nil)
      (:ok response))))
