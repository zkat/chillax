(cl:defpackage #:chillax
  (:use :cl :flexi-streams :drakma :alexandria :chillax.utils)
  (:export

   ;; Handy utils
   :mkhash
   :hashget
   :fun
   :strcat

   ;; Conditions
   :couchdb-error :unexpected-response :database-error
   :db-not-found :db-already-exists :document-error
   :document-not-found :document-conflict

   ;; Server API
   :server
   :host :port :username :password
   :json-server
   :couch-request
   :all-dbs
   :config-info
   :replicate
   :stats
   :active-tasks
   :get-uuids

   ;; Database API
   :database
   :server :name
   :db-request
   :db-info
   :db-connect
   :db-create
   :ensure-db
   :db-delete
   :db-compact
   :db-changes

   ;; Documents API
   :get-document
   :all-documents
   :batch-get-documents
   :put-document
   :post-document
   :delete-document
   :copy-document
   ))
