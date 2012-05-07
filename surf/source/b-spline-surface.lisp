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


(define-object b-spline-surface (surface)
  
  :documentation (:description "A general b-spline surface specified with control points, weights, knots, and degree."

                  :examples "<pre>

 (in-package :surf)

 (define-object test-b-spline-surface (b-spline-surface)
  
   :computed-slots
   ((points-data '(((0 0 0)(4 1 0)(8 1 0)(10 0 0)(8 -1 0)(4 -1 0)(0 0 0))
                   ((0 0 2) (4 2 2) (8 2 2) (10 0 2) (8 -2 2) (4 -2 2) (0 0 2))
                   ((0 0 4) (4 2 4) (8 2 4) (10 0 4) (8 -2 4) (4 -2 4) (0 0 4))
                   ((0 0 7) (4 1 7) (8 1 7) (10 0 7) (8 -1 7) (4 -1 7) (0 0 7))))
    (control-points (mapcar #'(lambda(list) 
                                      (mapcar #'apply-make-point list)) 
                                  (the points-data)))

    (display-controls (list :color :periwinkle :line-thickness 2))))
  
 (generate-sample-drawing :object-roots (make-object 'test-b-spline-surface)
                          :projection-direction :trimetric)

 </pre>")
  
  :input-slots
  ("List of lists of 3D Points. The control net."
   control-points
   
   ("Integer. Degree of surface in U direction. Defaults to 3." u-degree 3)
   
   ("List of Numbers. Knots in U direction. Default is NIL, which indicates a uniform knot vector in U direction."
    u-knot-vector (make-uniform-knot-vector (length (first (the control-points))) (the u-degree)))
   
   ("Integer. Degree of surface in V direction. Defaults to 3."
    v-degree 3)
   
   ("List of Numbers. Knots in V direction. Default is NIL, which indicates a uniform knot vector in V direction."
    v-knot-vector (make-uniform-knot-vector (length (the control-points)) (the v-degree)))
   
   ("List of lists of numbers. A weight to match each control point. Should be congruent with control-points 
 (i.e. same number of rows and columns). Default is a value of 1.0 for each weight, resulting in a nonrational surface."
    weights (make-list (length (the control-points)) 
                       :initial-element (make-list (length (first (the control-points))) :initial-element 1.0))))
  
  
  :computed-slots
  ((native-surface (make-b-spline-surface *geometry-kernel* (the control-points) 
                                          (the weights) 
                                          (the u-degree) (the u-knot-vector) 
                                          (the v-degree) (the v-knot-vector)))))


(define-object test-b-spline-surface (b-spline-surface)
  
  :input-slots ((display-controls (list :color :periwinkle :line-thickness 2))

		(z-stretch 1))
  
  :computed-slots
  ((points-data '(((0 0 0)(4 1 0)(8 1 0)(10 0 0)(8 -1 0)(4 -1 0)(0 0 0))
                  ((0 0 2) (4 2 2) (8 2 2) (10 0 2) (8 -2 2) (4 -2 2) (0 0 2))
                  ((0 0 4) (4 2 4) (8 2 4) (10 0 4) (8 -2 4) (4 -2 4) (0 0 4))
                  ((0 0 7) (4 1 7) (8 1 7) (10 0 7) (8 -1 7) (4 -1 7) (0 0 7))))
   
   (control-points (mapcar #'(lambda(list) (mapcar #'apply-make-point 
						   (mapcar #'(lambda(list)
							       (list (first list)
								     (second list)
								     (* (third list) (the z-stretch))))
							   list)))
                           (the points-data))))

  
   :hidden-objects ((view :type 'base-view
			  :pseudo-inputs (page-width page-length)
                          :projection-vector (getf *standard-views* :trimetric)
                          :page-width (* 5 72) :page-length (* 5 72)
                          :objects (list self))))


   
   
