(cl:in-package :cl-user)
;; (require 'asdf)
;; (asdf:oos 'asdf:load-op 'chillax.view-server)
(ql:quickload 'chillax.view-server)
#+sbcl
(sb-ext:save-lisp-and-die "chillax-server" :executable t :toplevel #'chillax-server::run-server)
#+ccl
(ccl:save-application "chillax-server" :prepend-kernel t
                      :toplevel-function #'chillax-server::run-server)
