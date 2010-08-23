(asdf:defsystem chillax.protocols
  :version "0"
  :description "CouchDB abstraction layer"
  :maintainer "Josh Marchán <sykopomp@sykosomatic.org>"
  :author "Josh Marchán <sykopomp@sykosomatic.org>"
  :licence "MIT"
  :depends-on (flexi-streams drakma)
  :serial t
  :components
  ((:module src
            :serial t
            :components
            ((:file "utils")
             (:file "package")
             (:module protocols
                      :serial t
                      :components
                      ((:file "base")
                       (:file "server")
                       (:file "database")
                       (:file "document")))))))
