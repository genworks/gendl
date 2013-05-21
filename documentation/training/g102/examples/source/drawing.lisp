(in-package :gdl-user)

(define-object robot-drawing (base-drawing)

  :hidden-objects ((robot-assembly :type 'robot:assembly))

  :objects
  ((tri-view :type 'base-view
	     :border-box? t
	     :object-roots (list (the robot-assembly robot))
	     :length (half (the length))
	     :center (translate (the center) :rear (half (the-child length)))
	     :projection-vector (getf *standard-views* :trimetric))

   (top-view :type 'base-view
	     :border-box? t
	     :object-roots (list (the robot-assembly robot))
	     :annotation-objects (list (the top-width-dim)
				       (the top-length-dim))
	     :view-scale (* 0.80 (getf (the-child view-contents-data) :view-scale))
	     :length (half (the length))
	     :width (half (the width))
	     :center (translate (the center) :front (half (the-child length))
				:left (half (the-child width)))
	     :projection-vector (getf *standard-views* :top))


   (top-width-dim :type 'horizontal-dimension
		  :character-size 10
		  :dim-scale (/ (the top-view view-scale))
		  :witness-line-length 20
		  :witness-line-gap 70
		  :witness-line-ext 5
		  :start-point (the top-view (view-point (the robot-assembly robot body (arms 0) (vertex :rear :left :top))))
		  :end-point (the top-view (view-point (the robot-assembly robot body (arms 1) (vertex :rear :right :top)))))

   (top-length-dim :type 'vertical-dimension
		   :dim-scale (/ (the top-view view-scale))
		   :character-size 10
		   :witness-line-length 15
		   :witness-line-gap 25
		   :witness-line-ext 3
		   :flip-leaders? t
		   :start-point (the top-view (view-point (the robot-assembly robot body base foot (vertex :rear :left :top))))
		   :end-point (the top-view (view-point (the robot-assembly robot body base foot (vertex :front :left :top)))))



   (front-view :type 'base-view
	       :border-box? t
	       :object-roots (list (the robot-assembly robot))
	       :length (half (the length))
	       :width (half (the width))
	       :center (translate (the center) :front (half (the-child length))
				  :right (half (the-child width)))
	       :projection-vector (getf *standard-views* :front))




   ))

   




