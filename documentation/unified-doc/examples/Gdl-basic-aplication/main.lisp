(in-package :my-new-packege)

(define-object new-object (base-object)
  :input-slots ()
  :trickle-down-slots ()
  :computed-slots ()
  :objects (
	    (my-cylinder :type 'cylinder
			:length 10
			:radius 3)
	    )
  :hidden-objects ()
  :functions ()
  :methods ())
