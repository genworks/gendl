;;
;; Copyright 2002-2011 Genworks International 
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;
;; This source file contains free software: you can redistribute it
;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This source file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with this source file.  If not, see
;; <http://www.gnu.org/licenses/>.
;;

(in-package :surf)


(define-object general-dual-blend-surface (b-spline-surface)
  
  :documentation (:description "Creates a smooth blend-surface between curve-1, lying on surface-1 and curve-2 on surface-2. The local start and end directions of this surface along curve-1 and curve-2 are specified by the user with parametric functions."
                  
                  :examples "<pre>

 (in-package :surf)

 (define-object test-g-d-b-s (base-object)
  
  :computed-slots
  ((f-tangent-1 #'(lambda(param)
                    (rotate-vector-d 
                     (the (face-normal-vector :top))
                     -10
                     (the curve-1 (tangent param)))))
                    
   
   (f-tangent-2 #'(lambda(param)
                    (rotate-vector-d 
                     (the (face-normal-vector :bottom))
                     10
                     (the curve-2 (tangent param)))))
   
   (f-ratio-1 #'(lambda(param)
                  (declare (ignore param))
                  1/3))
   
   (f-ratio-2 #'(lambda(param)
                  (declare (ignore param))
                  1/3)))
   
  :objects
  ((g-d-b-s :type 'general-dual-blend-surface
            :display-controls (list :color :green)
            :pass-down (f-tangent-1 
                        f-tangent-2 f-ratio-1 f-ratio-2
                        curve-1 surface-1 curve-2 surface-2))
                                    
    (surf-1-top :type 'linear-curve
               :hidden? t
               :start (make-point -5 -5 0)
               :end (make-point 5 -5 0))
    
   (surf-1-bottom :type 'linear-curve
                  :hidden? t
                  :start (make-point -7 -10 -2)
                  :end (make-point 7 -10 -2))
    
   (surface-1 :type 'ruled-surface
              :curve-1 (the surf-1-top)
              :curve-2 (the surf-1-bottom))
    
   (curve-1 :type 'iso-curve
            :display-controls (list :color :red :line-thickness 4)
            :surface (the surface-1)
            :parameter 0
            :u-or-v :v)
    
   (surf-2-bottom :type 'linear-curve
                  :hidden? t
                  :start (make-point -5 5 0)
                  :end (make-point 5 5 0))
    
   (surf-2-top :type 'linear-curve
               :hidden? t
               :start (make-point -7 10 2)
               :end (make-point 7 10 2))
    
   (surface-2 :type 'ruled-surface
              :curve-1 (the surf-2-bottom)
              :curve-2 (the surf-2-top))
    
   (curve-2 :type 'iso-curve
            :display-controls (list :color :blue :line-thickness 4)
            :surface (the surface-2)
            :parameter 0
            :u-or-v :v)))

 (generate-sample-drawing :object-roots (list (make-object 'test-g-d-b-s))
                           :projection-direction (getf *standard-views* :trimetric))

 </pre>" )
  
  :input-slots
  ("Input-function. Parametric function defined from 0 to 1 that outputs the blend-surface's local direction vector along curve-1. The input value of 0 corresponds to the start of curve-1, 1 to the end of curve-1."  f-tangent-1

   "Input-function. Parametric function defined from 0 to 1 that outputs the blend-surface's local direction vector along curve-2. The input value of 0 corresponds to the start of curve-2, 1 to the end of curve-2."  f-tangent-2
   
   ("Input-function. Parametric function defined from 0 to 1 that outputs the blend-surface's local blend-ratio along curve-1. The input value of 0 corresponds to the start of curve-1, 1 to the end of curve-1. The output local blend-ratio can be any number greater than 0, but the resulting blend surface can become erratic for values greater than 1. Defaults to a constant value of 1/3."  f-ratio-1 #'(lambda(param) (declare (ignore param)) 1/3))
   
   ("Input-function. Parametric function defined from 0 to 1 that outputs the blend-surface's local blend-ratio along curve-2. The input value of 0 corresponds to the start of curve-2, 1 to the end of curve-2. The output local blend-ratio can be any number greater than 0, but the resulting blend surface can become erratic for values greater than 1. Defaults to a constant value of 1/3."  f-ratio-2 #'(lambda(param) (declare (ignore param)) 1/3))
   
   "GDL curve. Curve lying on surface-1 that forms the starting-edge of the blend-surface" curve-1 
   
   "GDL surface. Surface that is to be connected to surface-2 by the blend-surface" surface-1
   
   "GDL curve. Curve lying on surface-2 that forms the ending-edge of the blend-surface"   curve-2 
   
   "GDL surface. Surface that is to be connected to surface-1 by the blend-surface"  surface-2
   
   ("Number. Number of b-spline-segments of the resulting blend-surface in the parametric direction along curve-1 and curve-2. Defaults to 50." n-segments 50))
   
  
  :computed-slots 
  ((control-point-distances (mapcar #'3d-distance 
                                    (the (fits 0) control-points)
                                    (the (fits 1) control-points)))
 
 
   (control-points (list (the (fits 0) control-points)
                         (the (fits 0) new-control-points)
                         (the (fits 1) new-control-points)
                         (the (fits 1) control-points)))
   
   
   ;;
   ;; FLAG -- ensure compatible knot vector between (the fits).
   ;;
   (u-knot-vector (the (fits 0) knot-vector))
   
   ;;(v-knot-vector)
   (u-degree (the (fits 0) degree))
   
   (v-degree 3)

   
   
   )
                         
  
  :hidden-objects
  ((fits :type 'blending-fitted-curve
         :sequence (:size 2)
         ;;
         ;; FLAG -- duplicate call to equi-spaced-paramters (happens again in equi-spaced-points)
         ;;

         :equi-parameters 

         (ecase (the-child index)
           (0 (the curve-1 (equi-spaced-parameters (the n-segments))))
           (1 (the curve-2 (equi-spaced-parameters (the n-segments)))))
         
         
         :points 
         (ecase (the-child index)
           (0 (the curve-1 (equi-spaced-points (the n-segments))))
           (1 (the curve-2 (equi-spaced-points (the n-segments)))))
         
         :vector-function 

         (ecase (the-child index)
           (0 (the f-tangent-1))
           (1 (the f-tangent-2)))
         
         :ratio-function 

         (ecase (the-child index)
           (0 (the f-ratio-1))
           (1 (the f-ratio-2)))
         
         :pass-down (control-point-distances))))
   

(define-object blending-fitted-curve (fitted-curve)
  
  :input-slots (equi-parameters vector-function ratio-function control-point-distances)
  
  :computed-slots
  ((take-off-vectors (mapcar (the vector-function) (the equi-parameters)))
   
   (distances (mapcar #'(lambda(distance parameter)
                          (* distance (funcall (the ratio-function) parameter)))
                      (the control-point-distances)
                      (the equi-parameters)))
   
   (new-on-curve-points (the (translate-points (the points))))
   
   
   (new-control-points (the (translate-points (the control-points))))
   
   )

  
  :objects ((control-grid :type 'points-display
                          :points (the control-points)))
  
  :functions
  (

   (translate-points 
    (points)
    (mapcar #'(lambda(point vector distance)
                (translate-along-vector point vector distance))
            points (the take-off-vectors) (the distances)))))




