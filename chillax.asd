(asdf:defsystem chillax
  :version "0"
  :description "CouchDB abstraction layer"
  :maintainer "Josh Marchán <sykopomp@sykosomatic.org>"
  :author "Josh Marchán <sykopomp@sykosomatic.org>"
  :licence "MIT"
  :depends-on (flexi-streams sheeple drakma cl-json)
  :serial t
  :components
  ((:module src
            :serial t
            :components
            ((:file "package")
             (:file "db")))))

