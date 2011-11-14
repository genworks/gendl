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

(define-object transformed-surface (transformed-bspline-mixin b-spline-surface)

  :input-slots
  (surface-in
   (from-location (the surface-in center))
   (from-orientation (the surface-in orientation)))
  
  :computed-slots
  (
   (orientation (the to-orientation))
   
   (b-spline-list (multiple-value-bind (control-points weights u-knots v-knots  u-degree v-degree)
                      (the surface-in b-spline-data)
                    (list :control-points control-points  
                          :weights weights :u-knots u-knots :v-knots v-knots 
                          :u-degree u-degree :v-degree v-degree)))
   
   
   (u-knot-vector (getf (the b-spline-list) :u-knots))
   (v-knot-vector (getf (the b-spline-list) :v-knots))
   (weights (getf (the b-spline-list) :weights))
   (u-degree (getf (the b-spline-list) :u-degree))
   (v-degree (getf (the b-spline-list) :v-degree))

   
   (transformed-control-points (if (the surface-in rational?)
                                   (mapcar #'(lambda(point-row weight-row)
                                               (mapcar #'(lambda(point weight)
                                                           (scalar*vector weight point))
                                                       point-row weight-row))
                                           (the %transformed-control-points)
                                           (the weights))
                                 (the %transformed-control-points)))

   
   (unweighted-points (if (the surface-in rational?)
                          (mapcar #'(lambda(point-row weight-row)
                                      (mapcar #'(lambda(point weight)
                                                  (scalar*vector (/ weight) point))
                                              point-row weight-row))
                                  (getf (the b-spline-list) :control-points)
                                  (the weights))
                        (getf (the b-spline-list) :control-points)))
   
   (%transformed-control-points 
    (mapcar #'(lambda(point-list)
                (mapcar #'(lambda(point)
                            (let ((transformed
                                   (the to-coord-space 
                                     (local-to-global 
                                      (the from-coord-space (global-to-local point))))))
                              (if (the scaling-needed?)
                                  (let ((local (the to-coord-space (global-to-local transformed))))
                                    (the to-coord-space (local-to-global 
                                                         (make-point (* (get-x local) (the scale-x))
                                                                     (* (get-y local) (the scale-y))
                                                                     (* (get-z local) (the scale-z))))))
                                transformed)))
                        point-list))
            (the unweighted-points)))))


(define-object test-transformed-surface (transformed-surface)
  
  :input-slots
  (surface-in (the trimmed surf))
   
   :objects
   ((trimmed :type 'surf::test-trimmed-from-projected)))
