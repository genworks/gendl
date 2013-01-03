(in-package :gdl-surf-user)

(define-object section-curve (base-object)
  
  :objects
  ((surface :type 'surf::test-b-spline-surface)
   
   (intersection-curve :type 'planar-section-curve
		       :plane-normal (the (face-normal-vector :top))
		       :plane-point (translate (the center)
					       :up 2))))

(define-object test-box (box-solid)
  :input-slots
  ((length 10)
   (width 10)
   (height 20)
   
   (display-controls (list :color :green
			   :transparency 0.5))
   )
  
  :hidden-objects
  ((hole :type 'cylinder-solid
	 :length (* (the length) 1.1)
	 :radius (half (half (the width)))))
  
  :objects
  (
   (subtraction :type 'subtracted-solid
		:brep self
		:other-brep (the hole))
   
   (vertex-spheres :type 'sphere
		   :display-controls (list :color :red)
		   :sequence (:size (the subtraction vertices
					 number-of-elements))
		   :center (the subtraction 
			     (vertices (the-child index))
			     center)
		   :radius (/ (the length) 50))))

