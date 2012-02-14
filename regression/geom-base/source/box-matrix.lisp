(in-package :gdl-user)

#+nil
(define-object box-matrix (base-object)
  
  :input-slots 
  ((rows 50)
   (columns 50))
  
  :objects
  ((box-container :type 'box-container
		  :box-length 10 
		  :box-width 10
		  :box-height 1
		  :pass-down (rows columns))
   
   (box-solid-container :type 'box-solid-container
			:box-length 10 
			:box-width 10
			:box-height 1
			:pass-down (rows columns))

   (planar-surface-container :type 'planar-surface-container
			     :box-length 10 
			     :box-width 10
			     :box-height 1
			     :pass-down (rows columns))
   
   ))

#+nil
(define-object planar-surface-container (base-object)
  :input-slots (box-length box-width box-height rows columns)
  
  :computed-slots ((width (* (the box-width) (the columns)))
		   (length (* (the box-length) (the rows))))
  
  :objects
  ((planes :type 'planar-surface
	   :p00 (translate (the-child center)
			   :left (half (the-child width))
			   :front (half (the-child length)))
	   :p01 (translate (the-child p00)
			   :right (the-child width))
	   :p10 (translate (the-child p01)
			   :rear (the-child length))
	   :p11 (translate (the-child p10)
			   :left (the-child width))
	   :length (the box-length)
	   :width (the box-width)
	   :height (the box-height)
	   :sequence (:matrix :longitudinal (the rows) 
			      :lateral (the columns)))))

#+nil
(define-object box-container (base-object)
  
  :input-slots (box-length box-width box-height rows columns)
  
  :computed-slots ((width (* (the box-width) (the columns)))
		   (length (* (the box-length) (the rows))))
  
  :objects
  ((boxes :type 'box
	  :length (the box-length)
	  :width (the box-width)
	  :height (the box-height)
	  :sequence (:matrix :longitudinal (the rows) 
			     :lateral (the columns)))))


#+nil
(define-object box-solid-container (base-object)
  
  :input-slots (box-length box-width box-height rows columns)
  
  :computed-slots ((width (* (the box-width) (the columns)))
		   (length (* (the box-length) (the rows))))
  
  :objects
  ((boxes :type 'box-solid
	  :length (the box-length)
	  :width (the box-width)
	  :height (the box-height)
	  :sequence (:matrix :longitudinal (the rows) 
			     :lateral (the columns)))))
