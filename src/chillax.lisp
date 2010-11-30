(cl:defpackage #:chillax
  (:use :cl :chillax.core :chillax.yason))
(cl:in-package :chillax)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-external-symbols (s (find-package "CHILLAX.CORE"))
    (export s (find-package "CHILLAX")))
  (do-external-symbols (s (find-package "CHILLAX.YASON"))
    (export s (find-package "CHILLAX"))))

(defparameter *chillax-version* "0.5")
(export '*chillax-version*)