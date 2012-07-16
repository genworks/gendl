;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
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

(define-object b-spline-curve (curve)
  
  :documentation (:description "A general NURBS (potentially non-Uniform, potentially Rational, b-spline) 
curve specified with control points, weights, knots, and degree.

If the knot-vector is different from the default, it is non-Uniform.

If any of the weights are different from 1.0, it is Rational."

                  
                  :examples "<pre>

 (in-package :surf)

 (define-object test-b-spline-curves (base-object)

  :input-slots
  ((control-points (list (make-point 0 0 0)
                         (make-point 2 3.0 0.0) 
                         (make-point 4 2.0 0.0) 
                         (make-point 5 0.0 0.0) 
                         (make-point 4 -2.0 0.0) 
                         (make-point 2 -3.0 0.0) 
                         (make-point 0 0 0))))
  
  :objects
  ((curves :type 'b-spline-curve
           :sequence (:size 6)
           :control-points (the control-points)
           :degree (1+ (the-child :index))
           :display-controls (list :line-thickness (* 0.3 (the-child index))
                                   :color (ecase (the-child index)
                                            (0 :red) (1 :orange) 
                                            (2 :yellow) (3 :green)
                                            (4 :blue) (5 :red-violet))))

   (points :type 'point 
           :sequence (:size (length (rest (the control-points))))
           :center (nth (the-child index) (rest (the control-points)))
           :display-controls (list :color :green))))


  (generate-sample-drawing :object-roots (make-object 'test-b-spline-curves))


  ;; Here is another example which shows the difference between a
  ;; simple bezier-curve from the :geom-base package, and a NURBS.
  ;; 
  (define-object bezier-and-nurbs (base-object)
  
    :input-slots ((control-points (list (make-point 0 0 0)
                                        (make-point 1 1 0)
                                        (make-point 2 1 0)
                                        (make-point 3 0 0))))
  
  
    :objects ((points :type 'points-display 
                      :points (the control-points))
            
            
              (bezier :type 'bezier-curve  
                      ;; This will be a geom-base:bezier-curve
                      :display-controls (list :color :green)
                      :control-points (the control-points))
            
              (b-spline :type 'b-spline-curve 
                        ;; This will be an equivalent surf:b-spline-curve.
                        :display-controls (list :color :red :bezier-points t)
                        :control-points (the bezier control-points))

              ;;
              ;; The b-spline-curve is a full NURBS curve and so has
              ;; more inputs than a simple bezier-curve: degree,
              ;; weights, and knot-vector, so we can do things like:
              ;;
              (b-spline-weighted :type 'b-spline-curve
                                 :display-controls (list :color :purple)
                                 :control-points (the bezier control-points)
                                 :weights (list 1.0 1.2 1.2 1.0))
            
              (b-spline-degree-2 :type 'b-spline-curve
                                 :display-controls (list :color :orange)
                                 :control-points (the bezier control-points)
                                 :degree 2)))

 </pre>")


  
  
  
  
  
  
  :input-slots
  ("List of 3D Points. The control points."
   control-points 
   
   ("Integer. Degree of the curve. Defaults to 3 (cubic)."
    degree 3) 
   
   ("List of Numbers. Knots of the curve. Default is NIL, which indicates a uniform knot vector."
    knot-vector (make-uniform-knot-vector (length (the control-points)) (the degree)))
   
   ("List of numbers. A weight to match each control point. Should be same length as control-points.
 Default is a value of 1.0 for each weight, resulting in a nonrational curve."
    weights (make-list (length (the control-points)) :initial-element 1.0))
   
   
   (surface nil)
   )
  
  :computed-slots
  ((native-curve (make-b-spline-curve *geometry-kernel* (the control-points) 
                                      (mapcar #'to-double-float (the weights)) (the degree) (the knot-vector) ))
   
   
   (%renderer-info% (list :vrml? t :view-default :top))
   ))


(define-object test-b-spline-curves (base-object)

  :input-slots
  ((control-points (list (make-point 0 0 0)
                         (make-point 2 3.0 0.0) 
                         (make-point 4 2.0 0.0) 
                         (make-point 5 0.0 0.0) 
                         (make-point 4 -2.0 0.0) 
                         (make-point 2 -3.0 0.0) 
                         (make-point 0 0 0))))
  
  :hidden-objects
  ((view :type 'base-view
         :page-length (* 5 72) :page-width (* 5 72)
	 :pseudo-inputs (page-length page-width)
         :objects (the children)))

  :objects
  ((note :type 'general-note
         :strings "Hey Now")
   
   (curves :type 'b-spline-curve
           :sequence (:size 6)
           :control-points (the control-points)
           :degree (1+ (the-child :index))
           :display-controls (list :line-thickness 3 ;;(* 0.3 (the-child index))
                                   :dash-pattern (list 10 10)
                                   :fill-color (ecase (the-child index)
                                                 (0 :red) (1 :orange) (2 :yellow) (3 :green)
                                                 (4 :blue) (5 :red-violet))
                                   :color 
                                   (ecase (the-child index)
                                            (0 :red) (1 :orange) (2 :yellow) (3 :green)
                                            (4 :blue) (5 :red-violet))))

   (points :type 'point
           :sequence (:size (length (rest (the control-points))))
           :center (nth (the-child index) (rest (the control-points)))
           :display-controls (list :color :green))))




   
   

            
         
  
