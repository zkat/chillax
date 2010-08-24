(asdf:defsystem chillax
  :version "0.2"
  :description "CouchDB abstraction layer - Primary implementation of protocols."
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (chillax.core yason)
  :serial t
  :components
  ((:module src
            :components
            ((:file "chillax")))))

