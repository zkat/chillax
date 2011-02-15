(asdf:defsystem chillax
  :version "0.4.1"
  :description "CouchDB abstraction layer - Easy-load system with sane defaults"
  :maintainer "Josh Marchán <sykopomp@sykosomatic.org>"
  :author "Josh Marchán <sykopomp@sykosomatic.org>"
  :licence "MIT"
  :depends-on (chillax.core chillax.yason)
  :components
  ((:module src
            :components
            ((:file "chillax")))))
