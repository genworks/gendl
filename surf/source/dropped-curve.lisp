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


(define-object dropped-curve (curve)
  
  :documentation (:description "Creates a 3D curve which is the <tt>curve-in</tt> dropped normally to the <tt>surface.</tt>.
The resulting 3D curve contains a uv-curve which is typically useful for trimming.


NOTE: Change from 1577p027 and forward -- the dropped curve now is a 3D curve which 
can be drawn. It contains its uv representation on the surface. Previously, the uv-curve
was the actual dropped-curve.


"
                  :examples "<pre>
                  
 (in-package :surf)

 (define-object test-trimmed-from-dropped-2 (trimmed-surface)

   :computed-slots
   ((uv-inputs t)
    (holes (list (the hole uv-curve)))
    (island (the island-3d uv-curve))
    (reverse-island? t) (reverse-holes? t)
    (display-controls (list :color :blue :line-thickness 2)))
  
   :hidden-objects
   ((basis-surface :type 'test-fitted-surface
                   :display-controls (list :color :grey-light-very)
                   :grid-length 10 :grid-width 10 :grid-height 5)
   
    (island-3d :type 'dropped-curve
               :curve-in (the raised-island)
               :surface (the basis-surface))
   
    (hole :type 'dropped-curve
          :curve-in (the raised-hole)
          :surface (the basis-surface))
   
    (raised-hole :type 'b-spline-curve
                 :display-controls (list :color :grey-light-very)
                 :control-points (list (make-point 3.5 4.5 7) (make-point 4.5 6 7) (make-point 5.5 7 7) 
                                       (make-point 6 4.5 7) (make-point 5.5 2 7) (make-point 4.5 2 7) 
                                       (make-point 3.5 4.5 7)))
   
    (raised-island :type 'b-spline-curve
                   :display-controls (list :color :grey-light-very)
                   :control-points (list (make-point 3 5 7) (make-point 5 8 7) (make-point 7 10 7) 
                                         (make-point 8 5 7) (make-point 7 0 7) (make-point 5 0 7) 
                                         (make-point 3 5 7)))))



  (generate-sample-drawing :objects (let ((self (make-object 'test-trimmed-from-dropped-2)))
                                       (list (the basis-surface) self (the raised-hole) (the raised-island)))

                           :projection-direction :trimetric)

 
</pre>")
  
  :input-slots
  ("GDL NURBS Curve. The curve to be dropped to the surface."
   curve-in 
   
   "GDL NURBS Surface. The surface on which the <tt>curve-in</tt> is to be dropped."
   surface)
  
  :computed-slots
  ((native-curve-uv (multiple-value-bind (control-points weights knots degree)
                        (get-b-spline-curve*-data 
                         *geometry-kernel* 
                         (let ((dropped-curves (drop-curve *geometry-kernel* (the curve-in) (the surface))))
                           (if (consp (rest dropped-curves))
                               (error "Dropping resulted in multiple curve segments. Dropped-curves is not yet implemented.
Please contact Genworks so we can expedite implementing this and provide a patch.")
                             (first dropped-curves))))
                      (make-b-spline-curve *geometry-kernel* control-points weights degree knots)))

   (built-from (the 3d-curve)))
  
  
  :hidden-objects
  ((uv-curve :type 'curve
             :native-curve (the native-curve-uv))
   
   (3d-curve :type 'b-spline-curve
             :knot-vector (the uv-curve knot-vector)
             :weights (the uv-curve weights)
             :degree (the uv-curve degree)
             :control-points (mapcar #'(lambda(point)
                                         (the surface (point (get-x point) (get-y point))))
                                     (the uv-curve control-points)))))



(define-object test-dropped-curve (dropped-curve)
  :hidden-objects
  ((curve-in :type 'b-spline-curve
             :control-points (list (make-point 3 5 1)
                                   (make-point 5 8.0 1) 
                                   (make-point 7 10.0 1) 
                                   (make-point 8 5.0 1) 
                                   (make-point 7 0.0 1) 
                                   (make-point 5 0.0 1) 
                                   (make-point 3 5 1)))
   (surface :type 'planar-surface
            :p00 (make-point 0 0 0)
            :p01 (make-point 0 10 0)
            :p10 (make-point 10 0 0)
            :p11 (make-point 15 15 0))))


(define-object test-dropped-curve-2 (dropped-curve)
  :hidden-objects
  ((curve-in :type 'linear-curve
             :start (the surface (point 0 0)) :end (the surface (point 1 1)))
   
   (surface :type 'planar-surface
            :p00 (make-point 0 0 0)
            :p01 (make-point 0 10 0)
            :p10 (make-point 10 0 0)
            :p11 (make-point 15 15 0))))
