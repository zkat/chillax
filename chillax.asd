(asdf:defsystem chillax
  :version "0.2"
  :description "CouchDB abstraction layer - Easy-load system with sane defaults"
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (chillax.core chillax.yason)
  :components
  ((:module src
            :components
            ((:file "chillax")))))
