(in-package :bracket) 

(define-object simple-bracket (base-object)
  :input-slots
  (
   (thickness 2 :settable)
   (width 100 :settable)
   (length-A 10 :settable)
   (length-B 30 :settable)
   (length-C 10 :settable)
   (angle-AB 45 :settable)
   (angle-BC 90 :settable)
   (radius-AB 2 :settable)
   (radius-BC 2 :settable)
   
   (number-of-holes 3 :settable)
   (hole-radius 5 :settable)
   (hide-holes? nil :settable)

   
   )
  :computed-slots
  (
   (length-XA (- (the length-A) (* (+ (the radius-AB) (the thickness)) (tan (degrees-to-radians (* 0.5 (the angle-AB)))))))
   (length-XB (- (the length-B) (+ (* (+ (the radius-AB) (the thickness)) (tan (degrees-to-radians (* 0.5 (the angle-AB)))))
				   (* (+ (the radius-BC) (the thickness)) (tan (degrees-to-radians (* 0.5 (the angle-BC))))))))
   (length-XC (- (the length-C) (* (+ (the radius-BC) (the thickness)) (tan (degrees-to-radians (* 0.5 (the angle-BC)))))))


   )
  :objects
  (
   (part-A
    :type 'box-solid
    :width (the length-XA)
    :length (the width)
    :height (the thickness)
    :display-controls (list :color :blue :transparency 0.5)
    :center (translate
	     (translate-along-vector
	      (translate-along-vector (the bend-AB reference-point)
				      (the bend-AB reference-top)
				      (* -0.5 (the thickness)))
	      (the bend-AB reference-right)
	      (* 0.5 (the-child width))) :rear (* 0.5 (the width)))
    :orientation (alignment :top (the bend-AB reference-top)
			    :right (the bend-AB reference-right)))


   (part-B
    :type 'box-solid-with-holes
    :width (the length-XB)
    :length (the width)
    :height (the thickness)
    :display-controls (list :color :blue :transparency 0.5)
    :center (translate (the center) :bottom (* 0.5 (the thickness)))
    :hole-brep-list (mapcar #'(lambda (x) (the-object x cylinder)) (list-elements (the hole))))

   (part-C
    :type 'box-solid
    :width (the length-XC)
    :length (the width)
    :height (the thickness)
    :display-controls (list :color :blue :transparency 0.5)
    :center (translate
	     (translate-along-vector
	      (translate-along-vector (the bend-BC reference-point)
				      (the bend-BC reference-top)
				      (* -0.5 (the thickness)))
	      (the bend-BC reference-right)
	      (* 0.5 (the-child width))) :front (* 0.5 (the width)))
    :orientation (alignment :top (the bend-BC reference-top)
			    :right (the bend-BC reference-right)))
   (bend-BC
    :type 'bend
    :display-controls (list :color :green :transparency 0.5)
    :width (the width)
    :radius (the radius-BC)
    :angle (the angle-BC)
    :thickness (the thickness)
    :center (translate (the center) :right (* 0.5 (the length-XB))
		       :rear (* 0.5 (the width))))

   (bend-AB
    :type 'bend
    :display-controls (list :color :green :transparency 0.5)
    :width (the width)
    :radius (the radius-AB)
    :angle (the angle-AB)
    :thickness (the thickness)
    :center (translate (the center) :left (* 0.5 (the length-XB))
		       :front (* 0.5 (the width)))
    :orientation (alignment :top (the (face-normal-vector :top))
			    :front (the (face-normal-vector :rear))))
   

   (hole
    :type 'hole
    :hidden? (the hide-holes?)
    :sequence (:size (the number-of-holes))
    :radius (the hole-radius)
    :line-length (* 3 (the thickness))
    :center (translate 
	     (translate (the center) :rear (* (/ (- (the number-of-holes) 1) 2)
					      (/ (the width) (the number-of-holes))))
	     :front (* (the-child index) (/ (the width) (the number-of-holes)))))



   
   (center-point
    :type 'sphere :hidden? t
    :radius 5
    :display-controls (list :color :red))
   (x-axis-left
    :type 'c-cylinder :hidden? t
    :end (make-point 35 0 0)
    :start (make-point 0 0 0)
    :display-controls (list :color :red)
    :radius 0.5)
   (y-axis-front
    :type 'c-cylinder :hidden? t
    :end (make-point 0 35 0)
    :start (make-point 0 0 0)
    :display-controls (list :color :blue)
    :radius 0.5)
    (z-axis-bottom
    :type 'c-cylinder :hidden? t
    :end (make-point 0 0 35)
    :start (make-point 0 0 0)
    :display-controls (list :color :green)
    :radius 0.5)
   

   ))

(define-object bend (extruded-solid)
:input-slots
((flat? nil :settable)
 (radius)
 (angle)
 (thickness)
 (width)

 (axis-vector (the (face-normal-vector :front)))
)
:computed-slots
(
 (arc-center (translate (the center) :top (the radius)))
 (bend-allowance (* (+ (the radius) (* 0.5 (the thickness)))
		    (degrees-to-radians (the angle))))

 (distance (the width))
 (curve1 (if (the flat?) (the upper-line) (the inner-arc)))
 (curve2 (if (the flat?) (the lower-line) (the outer-arc)))
 (side1 (make-object 'b-spline-curve :degree 1 :control-points
		     (list (the curve1 start) (the curve2 start))))
 (side2 (make-object 'b-spline-curve :degree 1 :control-points
		     (list (the curve1 end) (the curve2 end))))
 (profile (make-object 'composed-curve :curves (list (the curve1) 
						     (the side2) (the curve2) (the side1))))

 (reference-top (rotate-vector-d (the (face-normal-vector :top))
				 (the angle)
				 (the (face-normal-vector :front))))
 (reference-right (rotate-vector-d (the (face-normal-vector :right))
				 (the angle)
				 (the (face-normal-vector :front))))
 (reference-point (the curve1 end))



)
:objects
(
 (inner-arc
  :type 'arc-curve
  :radius (the radius) :hidden? t
  :center (the arc-center)
  :orientation (alignment :top (the (face-normal-vector :front))
			  :right (the (face-normal-vector :bottom)))
  :start-angle 0
  :end-angle (degrees-to-radians (the angle)))

 (outer-arc
  :type 'arc-curve
  :radius (+ (the radius) (the thickness)) :hidden? t
  :center (the arc-center)
  :orientation (alignment :top (the (face-normal-vector :front))
			  :right (the (face-normal-vector :bottom)))
  :start-angle 0
  :end-angle (degrees-to-radians (the angle)))

 (upper-line
  :type 'b-spline-curve :hidden? t
  :degree 1
  :control-points (list (the center) (translate (the center) :right (the bend-allowance))))
 (lower-line
  :type 'boxed-curve :hidden? t
  :curve-in (the upper-line)
  :center (translate (the center) :bottom (the thickness)))
  

 
))


(define-object hole (base-object)
  :input-slots
  ((line-length 0.5)
   (radius)
   )
  :objects
  ((line
    :type 'b-spline-curve
    :degree 1 
    :control-points (list (translate (the center) :top (* 0.5 (the line-length)))
			  (translate (the center) :bottom (* 0.5 (the line-length)))))
   (pt
    :type 'point
    :center (the center))
   
   (cylinder
    :type 'cylinder-solid
    :display-controls (list :color :grey :transparency 0.8)
    :radius (the radius)
    :length (the line-length)
    :orientation (alignment :top (the (face-normal-vector :right))
			    :front (the (face-normal-vector :top))))))

(define-object box-solid-with-holes (transformed-solid)
  :input-slots
  ((width) 
   (height)
   (length)
   (hole-brep-list))
  :computed-slots
  ((brep (the (subtract-holes-from-box)))
   )
  :objects 
  ((box 
    :type 'box-solid :hidden? t
    :pass-down (width length height))
   #+nil   (subtracted
	    :type 'subtracted-solid
    :brep (the box)
    :other-brep (first (the hole-brep-list)))
)
  :functions
  (
   (subtract-holes-from-box
    (&key (brep (the box))
	  (brep-list (the hole-brep-list)))
   (let* ((result)
	  (new-brep))
     (setq new-brep
	   (make-object 'subtracted-solid
			:brep brep
			:other-brep (first brep-list)))
     (if (eql 1 (length brep-list))
	 (setq result new-brep)
	 (setq result (the (subtract-holes-from-box :brep new-brep
						    :brep-list (cdr brep-list)))))
     result))))


     
  

  