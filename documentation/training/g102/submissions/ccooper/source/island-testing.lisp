(in-package :gdl-user)

(define-object island-testing ()

  :input-slots
  ()
  
  :objects
  ((surface1 :type 'planar-surface
	     :p00 (make-point 0 0 0)
	     :p01 (make-point 1 0 0)
	     :p11 (make-point 1 1 0)
	     :p10 (make-point 0 1 0))
   
   (surface2 :type 'planar-surface
	     :p00 (make-point 0 0 0)
	     :p01 (make-point 0 1 0)
	     :p11 (make-point 1 1 0)
	     :p10 (make-point 1 0 0))
   
   (surface3 :type 'planar-surface
	     :p00 (make-point 0 1 0)
	     :p01 (make-point 1 1 0)
	     :p11 (make-point 1 0 0)
	     :p10 (make-point 0 0 0))
   
   (surface4 :type 'planar-surface
	     :p00 (make-point 1 0 0)
	     :p01 (make-point 1 1 0)
	     :p11 (make-point 0 1 0)
	     :p10 (make-point 0 0 0))
   
   
   
   
   (curve :type 'b-spline-curve
	  :control-points (list (make-point 0.2 0.2 .001)
				(make-point 0.4 0.5 .001)
				(make-point 0.7 0.6 .001)
				(make-point 0.5 0.8 .001)
				(make-point 0.3 0.4 .001)
				(make-point 0.2 0.2 .001)))
   
   (drop-curves :type 'dropped-curve
		:sequence (:size 4)
		:surface (nth (the-child index)(list (the surface1) 
						     (the surface2)
						     (the surface3)
						     (the surface4)))
		:curve-in (the curve))
   
   ))
