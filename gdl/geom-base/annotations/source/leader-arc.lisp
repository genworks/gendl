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

(define-object leader-arc (leader-line)
  
  :input-slots (center-point radius witness-direction-vector-1 witness-direction-vector-2)
  
  :hidden-objects
  (
   
   

   (arc :type 'arc 
        :center (the center-point)
        :pass-down (radius)
        :start-angle (angle-between-vectors (the (face-normal-vector :right))
                                            (the witness-direction-vector-1)
                                            (the (face-normal-vector :top)) :-ve t)


        :end-angle   (angle-between-vectors (the (face-normal-vector :right))
                                            (the witness-direction-vector-2)
                                            (the (face-normal-vector :top)) :-ve t))

   
   (arrowhead :type 'arrowhead
              :center (translate-along-vector (the center-point)
                                              (the witness-direction-vector-1)
                                              (the radius))
              :length (the arrowhead-length)
              :width (the arrowhead-width)
              :style (the arrowhead-style)
              :orientation (alignment :top (the (face-normal-vector :top))
                                      :rear (cross-vectors (the witness-direction-vector-1)
                                                           (the (face-normal-vector :top)))))

   
   (arrowhead-2 :type (if (not (eql (the arrowhead-style-2) :none))
                          'arrowhead 'null-part)
                :center (translate-along-vector (the center-point)
                                                (the witness-direction-vector-2)
                                                (the radius))
                :length (the arrowhead-length)
                :width (the arrowhead-width)
                :style (the arrowhead-style-2)
                :orientation (alignment :top (the (face-normal-vector :top))
                                        :rear (cross-vectors (the (face-normal-vector :top))
                                                             (the witness-direction-vector-2))))
                                                             
   
   
   (polyline :type 'null-part)))


;;
;; FLAG -- get rid of this specialized view for both leader-line and
;;         leader-arc, and use outline-specialization-mixin instead.
;;
;;
;; FLAG -- update for auto-scaling outside base-view
;;

(define-lens (2d-output leader-arc)()
  :output-functions
  ((cad-output
    ()
    (write-the arc (cad-output))
    (write-the arrowhead (cad-output))
    (write-the arrowhead-2 (cad-output)))))
