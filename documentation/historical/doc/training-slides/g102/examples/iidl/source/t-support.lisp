(in-package :gdl-user)

(define-object t-support-1 (box)
  
  :input-slots
  (cost-per-inch
   (length 120)
   
   )
  
  :computed-slots
  ((height (/ (the width) 2))
   (width 12)
   (support-thickness 2)
     
   (cost (* (the length) (the cost-per-inch))))

  
  :objects
  ((vertical-support :type 'box
		     :center (translate (the center)
					:down
					(half (the-child height)))
		     :width (the support-thickness)
		     :height (the height)
		     :length (the length))
   
   (horizontal-support :type 'box
		       :center (translate (the center)
					    :up
					    (half (the-child height)))
		       :width (the width)
		       :length (the length)
		       :height (the support-thickness))))
    
