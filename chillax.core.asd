(asdf:defsystem chillax.core
  :version "0.2"
  :description "CouchDB abstraction layer - core API and protocols."
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (flexi-streams drakma cl-ppcre)
  :serial t
  :components
  ((:module src
            :serial t
            :components
            ((:file "utils")
             (:module core
                      :serial t
                      :components
                      ((:file "package")
                       (:file "server")
                       (:file "database")
                       (:file "document")
                       (:file "design-document")))))))
