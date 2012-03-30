(in-package :gdl-user)

(define-object simple-surface (base-object)
  :input-slots
  ((control-points 
    (list (list (make-point -2 0 -2) (make-point -1 1 -2)
                (make-point 1 1 -2) (make-point 2 0 -2))
          (list (make-point -2 0 -1) (make-point -1 2 -1)
                (make-point 1 2 -1) (make-point 2 0 -1))
          (list (make-point -2 0 1) (make-point -1 2 1)
                (make-point 1 2 1) (make-point 2 0 1))
          (list (make-point -2 0 2) (make-point -1 1 2)
                (make-point 1 1 2) (make-point 2 0 2)))))
  :objects
  ((surfaces
    :type 'b-spline-surface
    :sequence (:size 3)
    :u-degree (1+ (the-child index))
    :display-controls (list :color :blue 
                            :line-thickness 2 
                            :bezier-points t)
    :control-points (the control-points))))
