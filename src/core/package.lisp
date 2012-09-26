(cl:defpackage #:chillax.core
  (:use :cl :flexi-streams :drakma :alexandria :chillax.utils)
  (:shadow :url-encode)
  (:export

   ;; Conditions
   :couchdb-error :unexpected-response :database-error
   :db-not-found :db-already-exists :document-error
   :document-not-found :document-conflict

   ;; Server API
   :server-uri
   :all-dbs
   :config-info
   :replicate
   :stats
   :active-tasks
   :get-uuids

   ;; Database API
   :db-uri
   :db-info
   :db-connect
   :db-create
   :ensure-db
   :db-delete
   :db-compact
   :db-changes

   ;; Document API
   :get-document
   :get-document-revision
   :put-document
   :post-document
   :delete-document
   :copy-document
   :all-documents
   :batch-get-documents
   :bulk-post-documents

   ;; Standalone Attachment API
   :put-attachment
   :get-attachment
   :delete-attachment
   :copy-attachment

   ;; Design Document API
   :view-cleanup
   :compact-design-doc
   :design-doc-info
   :query-view
   :query-temporary-view

   ;;;
   ;;; Protocols
   ;;;

   ;; Server Protocol
   :server-host
   :server-port
   :server-username
   :server-password
   :server-secure-p
   :data->json
   :json->data
   :couch-request

   ;; Database Protocol
   :make-db-object
   :database-server
   :database-name

   ;;;
   ;;; Sample implementations
   ;;;
   :standard-server
   :standard-database
   ))
