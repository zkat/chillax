(asdf:defsystem chillax.jsown
  :description "CouchDB abstraction layer - Implementation of protocols using JSOWN."
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (chillax.core jsown)
  :serial t
  :components
  ((:module src
            :components
            ((:file "jsown")))))