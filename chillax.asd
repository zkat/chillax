(asdf:defsystem chillax
  :version "0"
  :description "CouchDB abstraction layer"
  :maintainer "Josh Marchán <sykopomp@sykosomatic.org>"
  :author "Josh Marchán <sykopomp@sykosomatic.org>"
  :licence "MIT"
  :depends-on (flexi-streams drakma yason)
  :serial t
  :components
  ((:module src
            :serial t
            :components
            ((:file "utils")
             (:file "package")
             (:file "base")
             (:file "server")
             (:file "database")
             (:file "document")))))
