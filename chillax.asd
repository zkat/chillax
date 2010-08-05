(asdf:defsystem chillax
  :version "0"
  :description "CouchDB abstraction layer"
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (flexi-streams drakma yason)
  :serial t
  :components
  ((:module src
            :serial t
            :components
            ((:file "utils")
             (:file "package")
             (:file "db")))))

