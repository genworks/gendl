(in-package :wind)

(define-object edge-blend-surface-wind (surface)
  
  :input-slots 
  (surface-1 surface-2 curve-1 curve-2

   ;;
   ;; FLAG -- replace with maximum-deviation
   ;;
   (number-of-sample-points 10)
   
   (curve-side-1 nil)
   (curve-side-2 nil)
   (angle-1 0)
   (angle-2 0)
   (ratio-1 0.5)
   (ratio-2 0.5)
   (reverse-1? nil)
   (untwist? nil)
   
   (hide-curve-computations? nil :settable)
   (hide-offset-points? nil :settable)
   (hide-iso-curves? nil :settable))


  
  :computed-slots 
  
  ((built-from (the extended))
   
   
   
   (control-points                      ;(gdl-user::transpose 
    (list (the curve-1-computation equi-spaced-points)
          (the curve-1-computation offset-points)
          (the curve-2-computation offset-points)
          (the curve-2-computation equi-spaced-points)))
   
   
   (u-degree 3)
   
   (3d-distance (3d-distance (the curve-1 start) (the curve-2 start))))

  
  :objects  (
            
             (splines :type 'b-spline-curve
                      :hidden? t
                      :sequence (:size (length (the curve-1-computation equi-spaced-points)))
                      :control-points (list 
                                       (car (nthcdr (the-child index) (the curve-2-computation equi-spaced-points)))
                                       (car (nthcdr (the-child index) (the curve-2-computation offset-points)))
                                       (car (nthcdr (the-child index) (the curve-1-computation offset-points)))
                                       (car (nthcdr (the-child index) (the curve-1-computation equi-spaced-points))))))
  
                                                
  :objects 
            
  ((curve-see-1 :type 'curve
                :hidden? t
                :built-from (the curve-1))
   #+nil
   (curve-see-2 :type 'curve
                :built-from (the curve-2))
   #+nil
   (splines-see-first :type 'curve
                      :built-from (the splines first))
   #+nil
   (splines-see-last :type 'curve
                     :built-from (the splines last))
   #+nil
   (splines-see-all :type 'curve
                    :hidden? nil
                    :sequence (:size (length (the curve-1-computation equi-spaced-points)))
                    :built-from (the (splines (the-child index))))

   (extended :type 'extended-surface
               :hidden? t
              :surface (the extended-4)
              :curve (the  curve-1)
              :direction :v
              :deformation-param 0.25
              :continuity :cmax
              :which-end :end) 
   
   
   
   (extended-4 :type 'extended-surface
                 :hidden? t
               :surface (the loft-blend)
               :curve (the  curve-2)
               :direction :v
               :deformation-param 0.25
               :continuity :cmax
               :which-end :start) 
     
   (loft-blend :type 'lofted-surface
               :hidden? t
               :synchronized? nil
               :display-controls (list :color :red)
               :tolerance 0.0001
               :curves (list-elements (the splines))))
  
  :hidden-objects 
  
  ((curve-1-computation :type 'edge-curve-computation
                        :hidden? (the hide-curve-computations?)
                        :3d-distance (the 3d-distance)
                        :number-of-sample-points (the number-of-sample-points)
                        :reverse? (the reverse-1?)
                        :curve (the curve-1)
                        :surface (the surface-1)
                        :ratio (the ratio-1)
                        :angle (the angle-1)
                        :curve-side (the curve-side-1)
                        :pass-down (hide-offset-points? hide-iso-curves?))
            
   (curve-2-computation :type 'edge-curve-computation
                        :hidden? (the hide-curve-computations?)
                        :3d-distance (the 3d-distance)
                        :number-of-sample-points (the number-of-sample-points)
                        :curve (the curve-2)
                        :reverse? nil
                        :surface (the surface-2)
                        :ratio (the ratio-2)
                        :angle (the angle-2)
                        :curve-side (the curve-side-2)
                        :pass-down (hide-offset-points? hide-iso-curves?))))



(define-object edge-curve-computation ()
  :input-slots 
  (hide-offset-points? hide-iso-curves? 
   3d-distance number-of-sample-points reverse? curve surface ratio angle curve-side)
  
  :computed-slots
  ((effective-curve (if (the reverse?) (the curve reverse) (the curve)))
   
   (uv-curve (let ((curve (the curve uv-curve)))
               (if (the reverse?) (the-object curve reverse) curve)))

   (equi-spaced-parameters (the effective-curve (equi-spaced-parameters (the number-of-sample-points))))
   
   (equi-spaced-points (the effective-curve (equi-spaced-points (the number-of-sample-points))))
   
   #+nil
   (equi-spaced-points (mapcar #'(lambda(param) (the effective-curve (point param)))
                               (the equi-spaced-parameters)))
   
   (iso-tangents (mapcar #'(lambda(iso) 
                             (let ((nominal (the-object iso (tangent (the curve parameter)))))
                               (if (eql (the curve-side) :left-side)
                                   (reverse-vector nominal)
                                 nominal)))
                         (list-elements (the isos))))
   
   (distance (* (the ratio) (the 3d-distance)))
   
   (offset-points (mapcar #'(lambda(point tangent)
                              (translate-along-vector point tangent (the distance)))
                          (the equi-spaced-points) (the iso-tangents))))
  
  :objects
  (
   (offset-spots :type 'points-display
                 :hidden? (the hide-offset-points?)
                 :points (the offset-points))
   
   (equi-spaced-spots :type 'points-display
                      :hidden? (the hide-offset-points?)
                      :points (the equi-spaced-points))
    
    
   (isos :type 'iso-curve
         :hidden? (the hide-iso-curves?)
         :sequence (:size (the number-of-sample-points))
         :surface (the surface)
         :u-or-v (ecase (the curve u-or-v) (:u :v) (:v :u))
         :parameter (let ((uv-point (the uv-curve (point (nth (the-child index) (the equi-spaced-parameters))))))
                      (ecase (the-child u-or-v) (:u (get-x uv-point)) (:v (get-y uv-point)))))))
   
   
   


  
  
