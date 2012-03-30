;;; Aircraft Exercise AE4-232
;;; 11 March 09
;;; Cory Cooper
;;; 
;;; 
;;;

(in-package :gdl-user)

(define-object aircraft-basic (base-object)
  
  :input-slots
  ((length 30)
   (fuselage-diameter 3)
   (wing-root-chord 3)
   (horz-tail-root-chord 2)
   (vert-tail-root-chord 2)
   (wing-root-NACA-last-two 12)
   (wing-tip-NACA-last-two 12)
   (horz-tail-root-NACA-last-two 12)
   (horz-tail-tip-NACA-last-two 12)
   (vert-tail-root-NACA-last-two 12)
   (vert-tail-wing-tip-NACA-last-two 12)
   (wing-taper 0.6)
   (horz-tail-taper 0.8)
   (vert-tail-taper 0.8)
   (wing-twist -10)
   (horz-tail-twist 0)
   (wing-span 15)
   (horz-tail-span 5)
   (wing-sweep 10)			;in degrees
   (horz-tail-sweep 10)			;in degrees
   )					;include aspect ratio in lieu of span-chord??
  
  
  :computed-slots
  (display-controls (list :color "#0000FF"
			   :isos (list :n-u 25 :n-v 10)))
  
  :objects
  ((fuselage				;creates the fuselage
    :type 'fuselage
    :length (the length)
    :diameter (the fuselage-diameter))
    
   (right-wing				;creates the right wing
    :type 'wing-basic
    :root-chord (the wing-root-chord)
    :root-NACA-last-two (the wing-root-NACA-last-two)  
    :tip-NACA-last-two (the wing-tip-NACA-last-two)
    :wing-length (half (the wing-span))
    :taper (the wing-taper)
    :twist (the wing-twist)
    :center (the root center)
    :orientation (alignment :front (rotate-vector-d (the (face-normal-vector :top))
						    -90
						    (the (face-normal-vector :front)))))
   
   #+nil
   (left-wing				;creates the left wing
    :type 'wing-basic
    :curve-in (the root-airfoil))	
   
   #+nil
   (vert-tail				;creates the vertical tail
    :type 'wing-basic
    :curve-in (the root-airfoil))	
   
   #+nil
   (rt-horz-tail			;creates the right horizontal tail
    :type 'wing-basic
    :curve-in (the tip-airfoil)		
    :orientation (alignment :rear 
			    (rotate-vector-d (the (face-normal-vector :rear))
					     (the twist)
					     (the (face-normal-vector :top)))))
   #+nil
   (lt-horz-tail			;creates the vertical tail
    :type 'wing-basic
    :curve-in (the root-airfoil))))
