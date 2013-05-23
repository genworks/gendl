(in-package :gdl-user)

(define-object robot-drawing (base-drawing)

  :hidden-objects ((robot-assembly :type 'robot:assembly)

		   (text-block :type 'robot-text-block
			       :width (the text-view width)
			       :length (the text-view length)
			       :robot-width (the robot-assembly height)
			       :robot-length (the robot-assembly length)
			       :arm-angle-left (the robot-assembly arm-angle-left)
			       :head-angle (the robot-assembly head-angle)
			       :body-angle (the robot-assembly body-angle)))

  :objects
  ((text-view :type 'base-view
	      :left-margin 0
	      :front-margin 0
	      :border-box? t
	      :objects (list (the text-block))
	      :length (half (the length))
	      :width (half (the width))
	      :projection-vector (getf *standard-views* :top)
	      :center (translate (the center) :rear (half (the-child length))
				 :right (half (the-child width))))

   (tri-view :type 'base-view
	     :border-box? t
	     :object-roots (list (the robot-assembly robot))
	     :length (half (the length))
	     :width (half (the width))
	     :center (translate (the center) :rear (half (the-child length))
				:left (half (the-child width)))
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




(define-object robot-text-block (typeset-block)
  
  :input-slots
  (robot-width robot-length body-angle arm-angle-left head-angle)

  :functions
  ((content
    ()
    (tt:compile-text (:font "Helvetica" :font-size 12.0)
      (tt:vspace 100)
      (tt:paragraph () "Robot Data")
      (tt:table (:col-widths (list 220 (- (the width) 220)))
	(dolist (slot (list :robot-width :robot-length :body-angle :arm-angle-left :head-angle))
	  (tt:row ()
	    (tt:cell (:background-color "#00FF00") (tt:put-string (format nil "~a" (string-capitalize slot))))
	    (tt:cell () 
	      (tt:paragraph (:h-align :center) (tt:put-string (format nil "~a" (the (evaluate slot)))))))))))))




