(in-package :ackermann)

(define-object tie-rod-arm (box)

  :input-slots
  (kingpin-center
   kingpin-axis
   front-vector
   offset
   current-rotation-angle
   (ball-radius 20))

  :computed-slots
  ((center (translate-along-vector (the kingpin-center) (the kingpin-axis) (the offset)))
   (orientation (alignment :top (the kingpin-axis) :front
			   (rotate-vector-d (the front-vector) (the current-rotation-angle) 
					    (the kingpin-axis))))
   (ball-center (the (face-center :rear))))

  :objects
  ((ball :type 'sphere
	 :radius (the ball-radius)
	 :center (the ball-center))
   (pipe :type 'c-cylinder
	 :radius (half (the width))
	 :start (the center)
	 :end (the ball center))))
