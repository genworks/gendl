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

(in-package :geom-base)


(define-object fillet (arc)
  
  :input-slots
  (local-vertex direction-vectors)
  
  :computed-slots
  ((orientation (alignment :top (apply #'cross-vectors (the :direction-vectors))))
             
   (angle (apply #'angle-between-vectors (the :direction-vectors)))
   
   (other-angle (- pi (the :angle)))
             
   (distance-to-tangent (* (the :radius)
                           (tan (half (the :other-angle)))))
             
   (tangents (mapcar #'(lambda(vector)
                         (translate-along-vector (the :local-vertex) vector (the :distance-to-tangent)))
                     (the :direction-vectors)))
   
   
   (keep-vector (subtract-vectors (the :local-vertex) (the :center)))
   

   (start-to-end-angle (twice (angle-between-vectors 
                               (the :keep-vector)
                               (subtract-vectors (first (the :tangents)) (the :center)))))
   
   (start-tangent (if (minusp (angle-between-vectors 
                               (subtract-vectors (first (the :tangents)) (the :center))
                               (subtract-vectors (second (the :tangents)) (the :center))
                               (the (:face-normal-vector :top)) :-ve t))
                      (second (the :tangents)) (first (the :tangents))))
   
   (start-angle (angle-between-vectors (the (:face-normal-vector :right))
                                       (subtract-vectors (the :start-tangent) (the :center))
                                       (the (:face-normal-vector :top))))
   
   (start-angle-normalized (mod (the start-angle) 2pi))
   
   (end-angle (+ (the start-angle-normalized) (the start-to-end-angle)))
   
   
   (center (inter-line-plane
            (first (the :tangents))
            (rotate-vector (first (the :direction-vectors))
                           (half pi)
                           (the (:face-normal-vector :top)))
            (second (the :tangents)) (second (the :direction-vectors))))))





