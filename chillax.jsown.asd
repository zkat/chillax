(asdf:defsystem chillax.jsown
  :description "CouchDB abstraction layer - Implementation of protocols using JSOWN."
  :maintainer "Josh Marchán <sykopomp@sykosomatic.org>"
  :author "Josh Marchán <sykopomp@sykosomatic.org>"
  :licence "MIT"
  :depends-on (chillax.core jsown)
  :serial t
  :components
  ((:module src
            :components
            ((:file "jsown")))))