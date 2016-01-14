(in-package :graph-plot)

(define-object assembly (base-object)

  :input-slots
  ((grid-increment 1)
   (function #'identity) (range-min -5) (range-max 5) (increment 0.1)
   (show-axes? t)
   (show-grid? (the show-axes?))

   (domain-min (apply  #'min (the y-samples)))
   (domain-max (apply #'max (the y-samples))))


  :computed-slots
  ((range-interval (- (the range-max) (the range-min)))

   (domain-interval (- (the domain-max) (the domain-min)))

   (x-samples (list-of-numbers (the range-min) (the range-max) (the increment)))
   (y-samples (mapcar #'(lambda(x) (funcall (the function) x)) (the x-samples)))

   (curve-sample-points (mapcar #'(lambda(x y) (make-point x y 0))
				   (the x-samples) (the y-samples))))

  
  :objects
  (
   (points-display :type 'points-display
		   
		   :hidden? t
		   :points (the curve-sample-points))

   (curve :type 'fitted-curve
	  :points (the curve-sample-points))

   (x-axis :type (if (the show-axes?) 'line 'null-object)
	   :start (make-point (the range-min) 0 0)
	   :end (make-point (the range-max) 0 0)
	   :display-controls (list :color :green :line-style :dashed))

   (x-grid :type (if (the show-grid?) 'line 'null-object)
	   :sequence (:size (floor (/ (the domain-interval) (the grid-increment))))
	   :display-controls (list :line-thickness 0.5 :color :grey)
	   :start (make-point (1+ (the range-min))
			      (+ (* (the-child index) (the grid-increment))
				 (floor (the domain-min))
				 (if (evenp (floor (the domain-min))) 0 1)) 0)
	   :end (make-point (1+ (the range-max))
			    (+ (* (the-child index) (the grid-increment))
			       (floor (the domain-min))
			       (if (evenp (floor (the domain-min))) 0 1)) 0))

   (y-axis :type (if (the show-axes?) 'line 'null-object)
	   :start (make-point 0 (the domain-min) 0)
	   :end (make-point 0 (the domain-max) 0)
	   :display-controls (list :color :green :line-style :dashed))

   (y-grid :type (if (the show-grid?) 'line 'null-object)
	   :sequence (:size (floor (/ (the range-interval) (the grid-increment))))
	   :display-controls (list :line-thickness 0.5 :color :grey)
	   :start (make-point (+ (* (the-child index) (the grid-increment))
				 (floor (the range-min)) 1)
			      (1- (the domain-min))
			      0)
	   :end (make-point (+ (* (the-child index) (the grid-increment))
			       (floor (the range-min)) 1)
			    (1- (the domain-max))
			    0))
   
   ))

		     
		     
