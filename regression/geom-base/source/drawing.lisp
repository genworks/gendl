(in-package :gdl-user)

(define-object box-with-annotated-drawing (base-object)

  :objects      
  ((drawing :type 'box-annotated-drawing
            :objects (list (the box)))
     
   (box :type 'box 
        :length 10 :width 20 :height 30)))


(define-object box-annotated-drawing (base-drawing)
  :input-slots (objects (character-size 15)
                (witness-line-gap 10)
                (witness-line-length 15)
                (witness-line-ext 5))
    
  :objects
  ((tri-view :type 'base-view 
              :projection-vector (getf *standard-views* :trimetric)
              :length (half (the length))
	      :width (half (the width))
              :center (translate (the center) :rear (half (the-child length))
				 :left (half (the-child width)))
              :objects (the objects)
              :annotation-objects (list (the main-length-dim)))
   

   (top-view :type 'base-view 
	     :view-scale (* (getf (the-child view-contents-data) :view-scale) 0.8)
	     :projection-vector (getf *standard-views* :top)
	     :length (half (the length))
	     :width (half (the width))
	     :center (translate (the center) :rear (half (the-child length))
				:right (half (the-child width)))
	     :objects (the objects)
	     :annotation-objects (list (the top-width-dim) (the top-length-dim)))
   
   
   (main-length-dim :type 'vertical-dimension
                    :pass-down (character-size witness-line-gap witness-line-length witness-line-ext)
                    :start-point (the tri-view (view-point (the box (vertex :rear :top :right))))
                    :end-point (the tri-view (view-point (the box (vertex :rear :bottom :right))))
                    :dim-value (3d-distance (the box (vertex :rear :top :right))
                                            (the box (vertex :rear :bottom :right)))
                    :text-above-leader? nil)
   
   (front-view :type 'base-view 
	       :view-scale (* (getf (the-child view-contents-data) :view-scale) 0.8)
	       :projection-vector (getf *standard-views* :front)
	       :length (half (the length))
	       :center (translate (the center) :front (half (the-child length)))
	       :objects (the objects)
	       :annotation-objects (list (the bottom-length-dim)))
   

   (top-width-dim :type 'horizontal-dimension
                   :pass-down (character-size witness-line-gap witness-line-length witness-line-ext)
                   :start-point (the top-view (view-point (the box (vertex :top :front :left))))
                   :end-point (the top-view (view-point (the box (vertex :top :front :right))))
                   :text-above-leader? nil
		   :flip-leaders? t
                   :dim-scale (/ (the top-view view-scale)))

   (top-length-dim :type 'vertical-dimension
                   :pass-down (character-size witness-line-gap witness-line-length witness-line-ext)
                   :start-point (the top-view (view-point (the box (vertex :top :rear :right))))
                   :end-point (the top-view (view-point (the box (vertex :top :front :right))))
		   :flip-leaders? nil
                   :dim-scale (/ (the top-view view-scale)))



   (bottom-length-dim :type 'horizontal-dimension
                   :pass-down (character-size witness-line-gap witness-line-length witness-line-ext)
                   :start-point (the front-view (view-point (the box (vertex :front :bottom :left))))
		   :end-point (the front-view (view-point (the box (edge-center :front :right))))
                   :text-above-leader? nil
		   :dim-value (the box width)
		   :dim-text-bias :start
		   :flip-leaders? t
		   :outside-leaders? t
		   )



   #+nil
   (bottom-length-dim :type 'horizontal-dimensions
                   :pass-down (character-size witness-line-gap witness-line-length witness-line-ext)
                   :start-point (the front-view (view-point (the box (vertex :front :bottom :left))))
                   :dim-scale (/ (the front-view view-scale))
                   :text-above-leader? nil
		   :flip-leaders? t
                   :end-point (the front-view (view-point (the box (vertex :front :bottom :right)))))

   ))

