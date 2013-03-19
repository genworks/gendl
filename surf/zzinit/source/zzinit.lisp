(in-package :surf)

(defparameter *these-features* (list :surf))
(glisp:set-features *these-features*)
(setq gdl::*features-to-initialize* (append gdl::*features-to-initialize* *these-features*))

