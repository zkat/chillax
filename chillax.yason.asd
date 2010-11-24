(asdf:defsystem chillax.yason
  :description "CouchDB abstraction layer - Implementation of protocols using Yason."
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (chillax.core yason)
  :serial t
  :components
  ((:module src
            :components
            ((:file "yason")))))