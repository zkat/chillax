(asdf:defsystem chillax-server
  :version "0"
  :description "View server for CouchDB"
  :maintainer "Josh Marchán <sykopomp@sykosomatic.org>"
  :author "Josh Marchán <sykopomp@sykosomatic.org>"
  :licence "MIT"
  :depends-on (yason)
  :serial t
  :components
  ((:module src
            :serial t
            :components
            ((:file "string-case")
             (:file "view-server")))))
