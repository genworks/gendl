;;; Airfoil exercise AE4-232
;;; 9 March 09
;;; Cory Cooper
;;; builds a symmetric airfoil from a leading-edge radius and two joined lines
;;; takes in the chord and 'NACA last two' (percent chord) for thickness
;;; however, resulting airfoil does not represent true NACA 00xx shape

(in-package :gdl-user)

(define-object airfoil-basic (composed-curve)
  
  :input-slots
  ((chord 2)
   (NACA-last-two 12))
  
  :computed-slots
  ((curves
    (list (the leading-edge)
	  (the top)
	  (the bottom))))
   
  :hidden-objects
  ((leading-edge 
    :type 'arc-curve
    :center (make-point 0 0 0)
    :radius (half (* (* .01 (the NACA-last-two)) (the chord)))
    :start-angle (* 0.5 pi)
    :end-angle (* -0.5 pi))
    
   (top 
    :type 'linear-curve
    :start (the leading-edge end)
    :end (make-point (- (the chord) 
			(the leading-edge radius)) 
		     0 0))
    
   (bottom 
    :type 'linear-curve
    :start (the leading-edge start)
    :end (the top end))))
