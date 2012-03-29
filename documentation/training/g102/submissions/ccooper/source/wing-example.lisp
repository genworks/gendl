;;; Wing exercise AE4-232
;;; 9 March 09
;;; Cory Cooper
;;; 
;;; Help from Dave Cooper on 11 Mar 09
;;;

(in-package :gdl-user)

(define-object wing-basic (lofted-surface)
  
  :input-slots
  ((root-chord 3)
   (root-NACA-last-two 12)
   (tip-NACA-last-two 12)
   (wing-length 6)
   (taper 0.5)				;ratio of tip chord/ root chord
   (twist -10))				;in degrees, negative indicates washout
  
  :computed-slots 
  ;;give the parent object the curves to loft between
  ((curves (list (the box-root-airfoil) (the box-tip-airfoil)))
   
   (display-controls (list :color "#0000FF"
			   :isos (list :n-u 25 :n-v 10))))
  
  :objects
  ((root-airfoil			;creates the root airfoil drawing aide
    :type 'airfoil-basic
    :chord (the root-chord)
    :NACA-last-two (the root-NACA-last-two)
    :center (the root center)
    :orientation (the root orientation))
   
   (tip-airfoil				;creates the tip airfoil drawing aide
    :type 'airfoil-basic
    :chord (* (the taper) (the root-chord)) 
    :NACA-last-two (the tip-NACA-last-two)
    :center (the root center)
    :orientation (the root orientation))
   
   
   (box-root-airfoil			;creates the boxed curve of the root airfoil
    :type 'boxed-curve
    :curve-in (the root-airfoil))	;"made of this curve"

   
   #+nil
   (check-point :type 'point
		:center (translate (the tip-airfoil top end)
				   :top (the wing-length)))
   
   (box-tip-airfoil			;creates the boxed curve of the tip aifoil
    :type 'boxed-curve
    :curve-in (the tip-airfoil)		;"made of this curve"
    ;; move the curve out the length of the wing to loft to
    
    :center 
    (translate (the center) 
	       :top 
	       (the wing-length)
		       
	       )
    :show-box? nil
    ;;move the box so that the twist angle rotates about the trailing edge axis
    :orientation-center (translate (the center) 
				   :right (- (the tip-airfoil chord) 
					     (half (* (the tip-NACA-last-two) 
						      0.01 
						      (the tip-airfoil chord)))))
    ;;give the tip airfoil a twist
    :orientation (alignment :rear 
			    (rotate-vector-d (the (face-normal-vector :rear))
					     (the twist)
					     (the (face-normal-vector :top)))))))

#+nil
(define-object wing-reader (iges-reader)
  
  :computed-slots ((file-name "/tmp/try.igs")))
