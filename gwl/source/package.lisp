(in-package :common-lisp-user)

;;
;; FLAG -- clean out symbol dependencies of (:net.aserve :net.aserve.client :net.uri :net.html.generator :cl-who)
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package (list :gwl :net.aserve :net.aserve.client :net.uri :net.html.generator :cl-who) :gwl))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro gwl:define-package (name &rest args)
    `(gdl:define-package ,name 
	 (:shadow #:define-package)
       (:use :gwl :net.aserve :net.aserve.client :net.uri :net.html.generator :cl-who)
       ,@args)))

(gwl:define-package :gwl-user)

