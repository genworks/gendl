(gwl:define-package :www.genworks.com (:export #:assembly))

#+(and linux allegro) (eval-when (:compile-toplevel :load-toplevel :execute)
			(require :smtp))
