(gwl:define-package :www.genworks.com (:export #:assembly))

#+allegro (eval-when (:compile-toplevel :load-toplevel :execute)
	    (require :smtp))
