(in-package :gdl-user)

(glisp:without-package-variance-warnings
  #+nil
  (gdl:define-package :gwl
      (:use :net.aserve :net.aserve.client :net.uri :net.html.generator :cl-who)
    (:shadow #:define-package))
  (gwl:define-package :gwl)
  (gwl:define-package :gwl-user))


