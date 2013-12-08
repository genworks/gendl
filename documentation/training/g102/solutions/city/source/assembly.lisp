(in-package :city)

(define-object assembly (application-mixin)

  :input-slots
  ((height 10) 
   (width 2000 :settable)
   (length 1000 :settable)
   (number-of-buildings 100 :settable)
   (number-of-x-sections 5 :settable)
   (number-of-y-sections 3 :settable)
   (proximity-rules *proximity-rules* :settable))

  
  :computed-slots
  ((corner (translate (the base center) 
		      :up (half (the base height))
		      :left (half (the base width))
		      :front (half (the base length))))
   
   (buildings-data 
    (let (result)
      (dotimes (n (the number-of-buildings) (nreverse result))
	(push (the (generate-building-data :height-min 10
					   :height-max 50
					   :width-min 20 
					   :width-max 75
					   :length-min 20 
					   :length-max 75)) result))))
   
   (sections-ht 
    (let ((ht (make-hash-table)))
      (dolist (building (list-elements (the buildings)) ht)
	(push building 
	      (gethash (the-object building section) ht)))))
   
   
   (ui-display-list-objects (cons (the base)
				  (append (list-elements (the buildings))
					  (list-elements (the sections)))))
   
   
   (minimum-distance (let (result)
		       (dolist (building (list-elements (the buildings)) result)
			 (when (and (the-object building minimum-distance)
				    (or (null result)
					(< (first (the-object building minimum-distance))
					   (first result))))
			   (setq result (append (the-object building minimum-distance)
						(list building))))))))
				
  
  :objects
  ((base :type 'box
	 :center (translate (the center)
			    :down (half (the height)))
	 :display-controls (list :color :green :transparency 0.2))
   
   (buildings :type (:sequence (mapcar #'(lambda(plist) (getf plist :type)) (the buildings-data)))
	      :sequence (:size (length (the buildings-data)))
	      :data (nth (the-child index) (the buildings-data))
	      :section (the (compute-section-for-building (the-child)))
	      :closest? (member (the-child) (the minimum-distance))
	      :height (getf (the-child data) :height)
	      :width (getf (the-child data) :width)
	      :length (getf (the-child data) :length)
	      :center (getf (the-child data) :center)
	      :pass-down (proximity-rules))
   
   
   (minimum-distance-rule :type 'gwl-rule-object
			  :rule-title "Min. Dist."
			  :rule-description "Closest Pair of Buildings"
			  :rule-result (format nil "~a to ~a: ~2,1f"
					       (the-object (second (the minimum-distance)) index)
					       (the-object (third (the minimum-distance)) index)
					       (first (the minimum-distance)))
			  :violated? t)
   
   (sections :type 'grid-section
	     :sequence (:matrix :lateral (the number-of-x-sections)
				:longitudinal (the number-of-y-sections))
	     :width (/ (the width) (the number-of-x-sections))
	     :length (/ (the length) (the number-of-y-sections))
	     :buildings-list (gethash (the-child) (the sections-ht))
	     :center (translate (the base (vertex :top :left :front))
				:right (+ (half (the-child width))
					  (* (the-child width) (first (the-child index))))
				
				:rear (+ (half (the-child length))
					  (* (the-child length) (second (the-child index)))))))

  
  :functions
  ((compute-section-for-building
    (building)
    (let ((section-indices
	   (list
	    (floor (div (+ (half (the width))
			   (get-x (the-object building center)) )
			(the (sections 0 0) width)))
	    (floor (div (+ (half (the length))
			   (get-y (the-object building center)) )
			(the (sections 0 0) length))))))
      (the (sections (first section-indices) (second section-indices)))))
	    
   
   (generate-building-data
    (&key height-min height-max
	  width-min width-max
	  length-min length-max
	  (base-width (the base width))
	  (base-length (the base length)))
    
    (let* ((type (ecase (random 2) (0 'school) (1 'pub)))
	   (height (+ (random (1+ (- height-max height-min))) 
		      height-min))
	   (width (+ (random (1+ (- width-max width-min))) 
		     width-min))
	   (length (+ (random (1+ (- length-max length-min))) 
		      length-min))
	   (center (translate 
		    (the corner) 
		    :up (half height)
		    :right (+ (half width) 
			      (random (- (1+ base-width) width)))
		    :rear (+ (half length) 
			     (random (- (1+ base-length) 
					length))))))
      (list :type type :height height :width width 
	    :length length :center center)))))
	  

(define-lens (html-format assembly)()
  :output-functions
  ((model-inputs
    ()
    (html (:table 
	   (:tr ((:td :bgcolor :yellow) "Width")
		(:td ((:input :type :string :name :width :value (the width)))))
	   (:tr ((:td :bgcolor :yellow) "Length")
		(:td ((:input :type :string :name :length :value (the length)))))
	   (:tr ((:td :bgcolor :yellow) "# of Buildings")
		(:td ((:input :type :string :name :number-of-buildings :value (the number-of-buildings)))))
	   (:tr ((:td :bgcolor :yellow) "# of Sections")
		(:td ((:input :type :string :name :number-of-x-sections :value (the number-of-x-sections))))))
	  (:p (:center ((:input :type :submit :name :submit :value "OK"))))))))
	  
