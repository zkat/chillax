(cl:defpackage #:chillax
  (:use :cl :chillax.core :chillax.yason))
(cl:in-package :chillax)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-external-symbols (s (find-package :chillax.core))
    (export s (find-package :chillax)))
  (do-external-symbols (s (find-package :chillax.yason))
    (export s (find-package :chillax))))

(defparameter *chillax-version* '(0 4 1))
(export '*chillax-version*)
