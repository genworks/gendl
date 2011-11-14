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


(define-object horizontal-dimension (linear-dimension)
  
  :documentation (:description "Creates a dimension annotation along the horizontal axis."
                  
                  
                        :examples "<pre>          
 (in-package :gdl-user)

 (define-object  box-view (base-object)
   
   :objects
   ((box :type 'box
         :length 10 :width (* (the-child length) +phi+)
         :height (* (the-child :width) +phi+))
   
    (width-dimension :type 'horizontal-dimension
                     :character-size (/ (the box length) 20)
                     :arrowhead-width (/ (the-child character-size) 3)
                     :start-point (the box (vertex :top :left :rear))
                     :end-point (the box (vertex :top :right :rear)))))

 (generate-sample-drawing :object-roots (make-object 'box-view)) </pre>")

  
  
  :input-slots
  (
   
   ("3D Point. Determines where the text will start. Defaults to reasonable location for 
horizontal-dimension."
    dim-text-start (let ((mid-point (translate-along-vector 
                                     (the leader-start)
                                     (subtract-vectors (the leader-end) (the leader-start))
                                     (half (3d-distance (the leader-start) (the leader-end))))))
                     (let ((center
                            (cond ((and (not (the outside-leaders?)) (the text-above-leader?))
                                   (translate mid-point :rear (* (half (the dimension-text length))
                                                                 (the dim-text-above-gap-factor))))
                                  ((not (the outside-leaders?))
                                   ;;(translate mid-point :front (half (the dimension-text length)))
                                   mid-point
                                   )      
                                  (t (ecase (the dim-text-bias)
                                       (:center mid-point)
                                       (:start (translate (the leader-start) 
                                                          (if (the start-end-swapped?) :right :left)
                                                          (+ (the leader-line-length)
                                                             (half (the dimension-text width)))))
                                       (:end (translate (the leader-end) 
                                                        (if (the start-end-swapped?) :right :left)
                                                        (+ (half (the dimension-text width))
                                                           (the leader-line-length-2)))))))))
                       (translate center :left (half (the dimension-text width))
                                  :front (half (the dimension-text length)))))))

                  
  :computed-slots
  (
   
   
   (sample-point-direction :right)
   
   (witness-direction-vector  (the (face-normal-vector (if (the flip-leaders?) :front :rear))))
   
   ;;
   ;; FLAG -- adjust for start-end-swapped? t
   ;;
   (leader-direction-1-vector (the (face-normal-vector
                                    (if (the start-end-swapped?) :right :left))))
   
   (leader-direction-2-vector (the (face-normal-vector
                                    (if (the start-end-swapped?) :left :right))))
   
   (base-plane-normal (the (face-normal-vector :rear)))))




