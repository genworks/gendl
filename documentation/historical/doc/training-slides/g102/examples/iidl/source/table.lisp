(in-package :gdl-user)


(define-object table (box)
  
  :input-slots 
  ((leg-type 't-support-1)
   (length 50) 
   (width 70)
   (height 20 :settable)
   (top-thickness 1/2)
   
   (number 2 :settable)
   
   )

  
  :objects
  ((table-top :type 'box
	      :length (the length)
	      :width (the width)
	      :height (the top-thickness))
   
   (legs :type (the leg-type)
	 :length (the height)
	 :width 2
	 :height 3
	 :orientation (alignment (ecase (first (the-child index))
				   (1 :bottom)
				   (0 :top))
				 (the (face-normal-vector :rear)))
	 :center (translate (the center)
			    (ecase (second (the-child index))
			      (0  :right)
			      (1  :left))
			    (- (half (the width)) (half (the-child width)))
			    (ecase (first (the-child index))
			      (0  :rear)
			      (1  :front))
			    (- (half (the length))
			       (half (the-child height)))
			    
			    :down (+ (half (the top-thickness))
				     (half (the-child length))))
			      
	 :sequence (:matrix :longitudinal (the number) :lateral (the number)))))
   
   
