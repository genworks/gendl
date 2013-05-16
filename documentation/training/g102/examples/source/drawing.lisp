(in-package :gdl-user)


(define-object robot-drawing (base-drawing)
  
  :objects
  ((tri-view :type 'base-view
	     :length (half (the length))
	     :center (translate (the center) :rear (half (the-child length)))
	     :projection-vector (getf *standard-views* :trimetric)
	     :object-roots (list (the robot)))
   
   (front-view :type 'base-view
	       :length (half (the length))
	       :width (half (the width))
	       :center (translate (the center) :front (half (the-child length))
				  :left (half (the-child width)))
	       :projection-vector (getf *standard-views* :front)
	       :object-roots (list (the robot)))


   (width-dim :type 'horizontal-dimension
	      :character-size 15
	      :dim-scale (/ (the top-view view-scale))
	      :witness-line-length 108
	      :start-point (the top-view (view-point (the robot robot body (arms 0) (vertex :top :rear :left))))
	      :end-point (the top-view (view-point (the robot robot body (arms 1) (vertex :top :rear :right)))))
   

   (top-view :type 'base-view
	     :length (half (the length))
	     :width (half (the width))
	     :center (translate (the center) :front (half (the-child length))
				:right (half (the-child width)))
	     :projection-vector (getf *standard-views* :top)
	     :annotation-objects (list (the width-dim))
	     :object-roots (list (the robot)))
   
   

   (robot :type 'robot:assembly)))


