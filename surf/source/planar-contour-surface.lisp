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

(define-object planar-contour-surface (trimmed-surface)
  
  :input-slots
  (island-curve

   (plane-normal nil))
  
  :computed-slots
  ((island (the island-curve))
   (island-curve-boundary (the island bounding-box)))

  
  :hidden-objects
  (

   (basis-surface :type 'rectangular-surface
                  :center (apply #'midpoint (the island-curve bounding-box))
                  :width (apply #'3d-distance (the island-curve bounding-box))
                  :length (apply #'3d-distance (the island-curve bounding-box))
                  :orientation 
                  (alignment :top (or (the plane-normal)
                                      (cross-vectors (the island-curve
                                                       (tangent (the island-curve u-min)))
                                                     (the island-curve
                                                       (normal (the island-curve u-min)))))))))

(define-object test-planar-contour-surface (planar-contour-surface)
  :hidden-objects
  (


   (island-curve :type 'b-spline-curve
                 :control-points (list (make-point 3 5 1)
                                       (make-point 5 8.0 1) 
                                       (make-point 7 10.0 1) 
                                       (make-point 8 5.0 1) 
                                       (make-point 7 0.0 1) 
                                       (make-point 5 0.0 1) 
                                       (make-point 3 5 1)))))
