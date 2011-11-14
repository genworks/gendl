;;-- main.lisp--
;; this is a new version V0.3 of the file main.lisp

(in-package :shock-absorber)

(define-object  assembly (base-object)
  
  :input-slots 
  ((piston-radius 12 :settable)
   (piston-y-position 125 :settable)
   (piston-length 10 :settable)
   (rot-length 90 :settable)
   (pressure-a 1.5e5 :settable);;[SI]
   (pressure-b 2.2e5 :settable);;[SI]
   (loaded-mass 10000 :settable);;[SI]
   )
  
  :computed-slots 
  ((volume-a 
    (* 2 pi (- (* (the piston-radius) 
		  (- (- (get-y (the rod-piston-assembly piston center)) 
			(get-y (the rod-piston-assembly floating-piston center))) 
		     (/ (+ (the rod-piston-assembly  floating-piston length) 
			   (the rod-piston-assembly  piston length)) 2))) 
	       (*  (the blocking-ring  length) 
		   (-  (the piston-radius)
		       (the blocking-ring  inner-radius-1))))))
   
   (volume-b 
    (* (- (- (get-y (the rod-piston-assembly floating-piston center)) 
	     (/ (the  rod-piston-assembly floating-piston length)2))
	  (+ (get-y (the tube-cap center)) (/ (the tube-cap length)2)))
       (* 2 pi (the  rod-piston-assembly  floating-piston radius )))))	    
  
  :objects 
  ((pressure-tube :type 'cone
		  :center (make-point 0 70 0)
		  :length 120
		  :radius-1 (+ 1 (the piston-radius))
		  :inner-radius-1 (the piston-radius)
		  :radius-2 (+ 1 (the piston-radius))
		  :inner-radius-2 (the piston-radius))
   
   (tube-cap :type 'cone
	     :center (make-point 0 5 0)
	     :length 10
	     :radius-1 5
	     :inner-radius-1 0
	     :radius-2 (+ 1 (the piston-radius))
	     :inner-radius-2 0)
   
   (seal-cap :type 'cone
	     :center (make-point 0 135 0)
	     :length 10
	     :radius-1 (+ 1 (the piston-radius))
	     :inner-radius-1 2.5
	     :radius-2 5
	     :inner-radius-2 2.5)
   
   (floating-piston :type 'cylinder
		    :center (make-point 0 35 0)
		    :radius (the piston-radius)
		    :length 10)
   
   (blocking-ring :type 'cone
		  :center (make-point 0 42.5 0)
		  :length 5
		  :radius-1 (the piston-radius)
		  :inner-radius-1 (* 0.8 (the piston-radius))
		  :radius-2 (the piston-radius)
		  :inner-radius-2 (* 0.8 (the piston-radius)))
   
   (rod-piston-assembly :type 'piston-assembly
			:pass-down (piston-radius piston-y-position piston-length rot-length)))
  
  :hidden-objects ()
  :functions ()
  :methods ())
