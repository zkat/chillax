(asdf:defsystem chillax.view-server
  :version "0.1"
  :description "View server for CouchDB"
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (yason alexandria)
  :serial t
  :components
  ((:module src
            :serial t
            :components
            ((:file "utils")
             (:module view-server
                      :components
                      ((:file "view-server")))))))
