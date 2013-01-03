(in-package :ackermann)

(define-object simple-linkage (box)

  :input-slots
  (tie-rod-length
   (kingpin-center (make-point 0 0 0))
   (kingpin-radius 15)
   (kingpin-inclination 10)
   (kingpin-length 100)
   (tie-rod-arm-length 300)
   (tie-rod-arm-width 30)
   (tie-rod-arm-height 30)
   (tie-rod-arm-offset 50)
   (current-rotation-angle 0)
   (side :left)
   (height (* (the kingpin-length) 1.25))
   (width (* (the kingpin-diameter) 1.25))
   (length (the width)))

  :computed-slots
  ((kingpin-diameter (twice (the kingpin-radius)))
   (kingpin-axis (the kingpin direction-vector))
   (tie-rod-arm-ball-center (the tie-rod-arm ball-center))
   (tie-rod-arm-vector (subtract-vectors (the tie-rod-arm-ball-center) (the tie-rod-arm-pivot-point)))
   (straight-ahead-tie-rod-arm-ball-center (the straight-ahead-tie-rod-arm ball-center))
   (tie-rod-arm-pivot-point (inter-line-plane 
			     (the kingpin center) (the kingpin-axis)
			     (the straight-ahead-tie-rod-arm-ball-center) (the kingpin-axis)))
   (straight-ahead-tie-rod-arm-vector (subtract-vectors (the straight-ahead-tie-rod-arm-ball-center)
							(the tie-rod-arm-pivot-point))))

  :objects
  ((kingpin :type 'cylinder
	    :radius (the kingpin-radius)
	    :center (the kingpin-center)
	    :length (the kingpin-length)
	    :orientation (alignment :front
				    (rotate-vector-d (the (face-normal-vector :top)) (the kingpin-inclination)
						     (the (face-normal-vector (the side))))))
   (tie-rod-arm :type 'tie-rod-arm
		:kingpin-center (the kingpin center)
		:kingpin-axis (the kingpin-axis)
		:front-vector (the (face-normal-vector :left))
		:length (the tie-rod-arm-length)
		:width (the tie-rod-arm-width)
		:height (the tie-rod-arm-height)
		:offset (the tie-rod-arm-offset)
		:current-rotation-angle (the current-rotation-angle)))

  :hidden-objects
  ((straight-ahead-tie-rod-arm :type 'tie-rod-arm
			       :kingpin-center (the kingpin center)
			       :kingpin-axis (the kingpin-axis)
			       :front-vector (the (face-normal-vector :left))
			       :length (the tie-rod-arm-length)
			       :width (the tie-rod-arm-width)
			       :height (the tie-rod-arm-height)
			       :offset (the tie-rod-arm-offset)
			       :current-rotation-angle 0)
   (tie-rod-ball-socket-sphere :type 'sphere
			       :center (the tie-rod-arm ball-center)
			       :radius (the tie-rod-length))
   
   (tie-rod-arm-ball-articulation-circle :type 'circle
					 :radius (half (the tie-rod-arm length))
					 :center (inter-line-plane (the kingpin center) 
								   (the kingpin-axis) 
								   (the straight-ahead-tie-rod-arm-ball-center)
								   (the kingpin-axis))
					 :orientation (alignment :top (the kingpin direction-vector)))))
