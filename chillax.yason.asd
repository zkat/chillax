(asdf:defsystem chillax.yason
  :description "CouchDB abstraction layer - Implementation of protocols using Yason."
  :maintainer "Josh Marchán <sykopomp@sykosomatic.org>"
  :author "Josh Marchán <sykopomp@sykosomatic.org>"
  :licence "MIT"
  :depends-on (chillax.core yason)
  :serial t
  :components
  ((:module src
            :components
            ((:file "yason")))))