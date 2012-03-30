(in-package :gdl-user)

(define-object simple-curve (base-object)
  :input-slots
  ((control-points (list (make-point -2 0 0) 
                         (make-point -1 1 0) 
                         (make-point 1 1 0)
                         (make-point 2 0 0))))
  :objects
  ((curves
    :sequence (:size 3)
    :type 'b-spline-curve
    :degree (1+ (the-child index))
    :control-points (the control-points))
   
   (points
    :type 'points-display
    :points (the control-points))))
