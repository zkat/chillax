(asdf:defsystem chillax.core
  :version "0.1"
  :description "CouchDB abstraction layer - core API and protocols."
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (flexi-streams drakma)
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
                       (:file "base")
                       (:file "server")
                       (:file "database")
                       (:file "document")
                       (:file "design-document")))))))
