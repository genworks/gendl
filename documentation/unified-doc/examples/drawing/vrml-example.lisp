(in-package :gdl-user)

(define-object vrml-example (base-object)

  :computed-slots
  
  ((points-data '((1.5 0.0 0.0) (2.5 0.0 0.0) 
                  (3.0 0.0 0.0) (4.0 0.0 0.0) 
                  (5.0 0.5 0.0) (7.0 3.0 0.0) 
                  (5.0 6.5 0.0) (3.5 8.5 0.0) 
                  (4.5 10.0 0.0) (6.0 10.0 0.0)))
   
   (control-points (mapcar #'(lambda(list) (apply-make-point list)) 
                           (the points-data))))
  :objects 

   ((conture :type 'b-spline-curve
            :hidden? t
            :control-points (the control-points))
   
   (vase :type 'revolved-surfaces
         :display-controls (list :color :periwinkle )
         :axis-point (make-point 1.5 0 0)
         :axis-vector (make-vector 0 1 0)
         :curves (list (the conture)))))
