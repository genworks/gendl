(in-package :surf)


(define-object normally-projected-curve (curve)

    :input-slots (from-surface
		  curve to-surface
		  (number-of-points 200))


    :computed-slots ((sample-points (the curve uv-curve (equi-spaced-points (the number-of-points))))

		     (normals (mapcar #'(lambda(point) (the from-surface (normal (get-u point) (get-v point)))) (the sample-points)))

		     (projected-points (mapcar #'(lambda(point normal)
						   (the to-surface (projected-point (the from-surface (point (get-u point) (get-v point))) normal)))
					       (the sample-points) (the normals)))

		     (projected-points-3d (mapcar #'get-3d-point-of (the projected-points)))


		     (projected-points-uv (mapcar #'get-uv-point-of (the projected-points)))

		     (built-from (the fitted-3d)))

  
  
    :hidden-objects ((fitted :type 'fitted-curve
			     :points (the projected-points-3d))

		     (fitted-uv :type 'fitted-curve
				:points (mapcar #'(lambda(point) (make-point (get-x point) (get-y point) 0)) (the projected-points-uv)))

		     (approximated :type 'approximated-curve
				   :curve-in (the fitted-uv)
				   :tolerance 0.00001)
            
		     (fitted-3d :type 'b-spline-curve
				:knot-vector (the approximated knot-vector)
				:degree (the approximated degree)
				:weights (the approximated weights)
				:control-points (mapcar #'(lambda(point)
							    (the to-surface (point (get-u point) (get-v point))))
							(the approximated control-points)))))

  


#+nil
(define-object normally-projected-curve-test (normally-projected-curve)

  :computed-slots ((from-surface (the test-fitted-surface))
		   (curve (the projected))
		   (to-surface (the offset-surface)))


  :objects ((test-fitted-surface :type 'test-fitted-surface
				 :grid-height 1)

	    (offset-surface :type 'rectangular-surface
			    :length 5 :width 5 :center (make-point 2 2 1.5))

	    (projected :type 'projected-curve
		       :curve-in (the segment)
		       :surface (the test-fitted-surface)
		       :projection-vector (the (face-normal-vector :bottom)))

	    (segment :type 'linear-curve
		     :start (make-point 1 1 3)
		     :end (make-point 3 3 3))))
