(in-package :ackermann)

(define-object assembly (box)
  
  :documentation (:description "Schematic of a typical truck steering system.")
  
  :input-slots
  (("List of 3D points. Enables direct placement of the kingpin axes."
    kingpin-centers (list :left (make-point 0 -200 0) :right (make-point 0 200 0)))
   ("List of keyword and number. Indicates current turn state of the mechanism."
    current-rotation-angle 30 :settable))


  :computed-slots
  ((self-center (midpoint (getf (the kingpin-centers) :left) (getf (the kingpin-centers) :right)))
   (length (3d-distance (getf (the kingpin-centers) :left) (getf (the kingpin-centers) :right)))
   (tie-rod-length (3d-distance (the known-linkage-object straight-ahead-tie-rod-arm-ball-center)
				(the other-linkage-object straight-ahead-tie-rod-arm-ball-center)))
   (width 100)
   (height 100)
   (known-linkage-object (the (linkages 0)))
   (known-linkage-object-sphere (the known-linkage-object tie-rod-ball-socket-sphere))
   (other-linkage-object (the (linkages (- 1 (the known-linkage-object :index)))))
   (other-linkage-object-circle (the other-linkage-object tie-rod-arm-ball-articulation-circle))
   
   (other-tie-rod-ball-location (inter-circle-sphere (the other-linkage-object-circle center)
						     (the other-linkage-object-circle radius)
						     (the other-linkage-object-circle (face-normal-vector :top))
						     (the known-linkage-object-sphere center)
						     (the known-linkage-object-sphere radius)
						     nil))
   
   (other-tie-rod-rotation (angle-between-vectors-d (the other-linkage-object straight-ahead-tie-rod-arm-vector)
						    (subtract-vectors (the other-tie-rod-ball-location)
								      (the other-linkage-object tie-rod-arm-pivot-point))
						    (the other-linkage-object kingpin-axis) t)))

  :objects
  (
   (ball :type 'sphere
	 :display-controls (list :color :red)
	 :center (the other-tie-rod-ball-location)
	 :radius 10)
   
   (tie-rod :type 'c-cylinder
	    :radius 1
	    :start (the (linkages 0) tie-rod-arm-ball-center)
	    :end (the (linkages 1) tie-rod-arm-ball-center))
   (linkages :type 'simple-linkage
	     :sequence (:size 2)
	     :side (ecase (the-child index) (0 :left) (1 :right))
	     :tie-rod-length (the tie-rod-length)
	     :kingpin-center (getf (the kingpin-centers) (the-child side))
	     :current-rotation-angle (if (eql (the-child side) :left) (the current-rotation-angle)
				       (the other-tie-rod-rotation)))))
