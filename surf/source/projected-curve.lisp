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

(define-object projected-curve (curve)
  
  :documentation (:description "Creates a 3D curve which is the <tt>curve-in</tt> 
projected onto the <tt>surface.</tt> according to the <tt>projection-vector</tt>. 
The resulting curve contains a uv-curve which is typically useful for trimming.

NOTE: Change from 1577p027 and forward -- the projected curve now is a 3D curve which 
can be drawn. It contains its uv representation on the surface. Previously, the uv-curve
was the actual projected-curve.

"

			       #+nil
			       :examples
			       #+nil "<pre>

 (in-package :surf)


 (define-object test-trimmed-from-projected-2 (trimmed-surface)
  :computed-slots
  ((uv-inputs t)
   (island (the island-3d uv-curve))
   (holes (list (the hole uv-curve)))
   (display-controls (list :color :blue :line-thickness 2)))
  
  :objects
  ((basis-surface :type 'test-fitted-surface
                  :display-controls (list :color :pink)
                  :grid-length 10 :grid-width 10 :grid-height 5
                  )
   
   (raised-hole :type 'b-spline-curve
                :display-controls (list :color :grey-light-very)
                :control-points (list (make-point 3.5 4.5 7)
                                      (make-point 4.5 6 7) 
                                      (make-point 5.5 7 7) 
                                      (make-point 6 4.5 7) 
                                      (make-point 5.5 2 7) 
                                      (make-point 4.5 2 7) 
                                      (make-point 3.5 4.5 7)))

   (raised-island :type 'b-spline-curve
                  :display-controls (list :color :grey-light-very)
                  :control-points (list (make-point 3 5 7)
                                        (make-point 5 8 7) 
                                        (make-point 7 10 7) 
                                        (make-point 8 5 7) 
                                        (make-point 7 0 7) 
                                        (make-point 5 0 7) 
                                        (make-point 3 5 7))))
  :hidden-objects
  ((island-3d :type 'projected-curve
              :curve-in (the raised-island)
              :surface (the basis-surface)
              :projection-vector (make-vector 0 0 -1))
   

   (hole :type 'projected-curve
         :curve-in (the raised-hole)
         :surface (the basis-surface)
         :projection-vector (make-vector 0 0 -1))))

#+nil
 (generate-sample-drawing 
  :objects (let ((self (make-object 'test-trimmed-from-projected-2)))
             (list (the basis-surface) self (the raised-island) 
                   (the raised-hole)))
  :projection-direction :trimetric)
 

 </pre>")
  
  :input-slots
  ("GDL NURBS Curve. The curve to be projected to the surface."
   curve-in 
   
   "GDL NURBS Surface. The surface on which the <tt>curve-in</tt> is to be projected."
   surface 
   
   "3D Vector. The direction of projection."
   projection-vector


   ("Number or nil. The tolerance used when projecting and creating new curves.
Defaults to nil, which uses the default of the geometry kernel."
    approximation-tolerance nil)

   ("Number or nil. The angle tolerance used when projecting and creating new curves.
Defaults to nil, which uses the default of the geometry kernel."
    angle-tolerance-radians nil)

   )
  
  :computed-slots
  (
   (native-curves-uv (let ((projected-curves 
                            (project-curve *geometry-kernel* (the curve-in) 
                                           (the surface) (the projection-vector)
                                           (when (the approximation-tolerance)
                                             (the approximation-tolerance))
                                           (when (the angle-tolerance-radians)
                                             (the angle-tolerance-radians)))))
                       (mapcar #'(lambda(curve)
                                   (multiple-value-bind (control-points weights knots degree)
                                       (get-b-spline-curve*-data *geometry-kernel* curve)
                                     (make-b-spline-curve *geometry-kernel* control-points weights degree knots)))
                               projected-curves)))
   
   (native-curve-uv  (if (= (length (the native-curves-uv)) 1) 
                         (first (the native-curves-uv))
                       (the composed native-curve)))
     
   (built-from (the 3d-curve)))

    
  :hidden-objects
  (("GDL UV curve object. The resultant projected-curve in the UV space of the surface."
    uv-curve :type 'curve
             :native-curve (the native-curve-uv))

     
   (3d-curve :type 'b-spline-curve
             :knot-vector (the uv-curve knot-vector)
             :weights (the uv-curve weights)
             :degree (the uv-curve degree)
             :control-points (mapcar #'(lambda(point)
                                         (the surface (point (get-x point) (get-y point))))
                                     (the uv-curve control-points))))
  
  :hidden-objects ((result-curves :type 'curve
                                  :sequence (:size (length (the native-curves-uv)))
                                  :native-curve (nth (the-child index) (the native-curves-uv)))
                    
                   (composed :type 'composed-curve
                             :curves (the result-curves))))












