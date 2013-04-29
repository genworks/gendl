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


(in-package :geom-base)

(define-object vertical-dimension (linear-dimension)

  :documentation (:description "Creates a dimension annotation along the vertical axis."
                  
                  
                  :examples "<pre>        

 (in-package :gdl-user)
                   
 (define-object  vertical-dimension-sample (base-object)
  
  
   :objects
   ((box :type 'box
         :length 10 :width (* (the-child length) +phi+)
         :height (* (the-child :width) +phi+))
   
    (length-dimension :type 'vertical-dimension
                      :character-size (/ (the box length) 20)
                      :flip-leaders? t
                      :start-point (the box (vertex :top :left :front))
                      :end-point (the box (vertex :top :left :rear)))))

 (generate-sample-drawing :object-roots (make-object 'vertical-dimension-sample))

</pre>")

  :input-slots

  (("3D Point. Determines where the text will start. Defaults to reasonable location for 
horizontal-dimension."
    dim-text-start (let ((mid-point (translate-along-vector 
                                     (the leader-start)
                                     (subtract-vectors (the leader-end) (the leader-start))
                                     (half (3d-distance (the leader-start) (the leader-end))))))
                     (let ((center
                            (cond ((and (not (the outside-leaders?))
                                        (the text-above-leader?)
                                        (the text-along-axis?))
                                   (translate mid-point :left (* (half (the dimension-text length))
                                                                 (the dim-text-above-gap-factor))))
                                  
                                  ((and (not (the outside-leaders?)) 
                                        (not (the text-above-leader?))
                                        (the text-along-axis?))
                                   
                                   mid-point)
                                  
                                  ((and (not (the outside-leaders?)) 
                                        (the text-above-leader?)
                                        (not (the text-along-axis?)))
                                   ;;(translate mid-point :left (half (the dimension-text width)))
				   mid-point

				   )

                                  ((and (not (the outside-leaders?)) 
                                        (not (the text-above-leader?))
                                        (not (the text-along-axis?)))
                                   mid-point)

                                  (t (ecase (the dim-text-bias)
                                       (:center mid-point)
                                       (:start (translate (the leader-start) 
                                                          (if (the start-end-swapped?) :rear :front)
                                                          (+ (the leader-line-length)
                                                             (half (the dimension-text width)))))
                                       (:end (translate (the leader-end) 
                                                        (if (the start-end-swapped?) :rear :front)
                                                        (+ (half (the dimension-text width))
                                                           (the leader-line-length-2)))))))))
                       (translate-along-vector
                        (translate-along-vector center 
                                                (the dimension-text (face-normal-vector :left))
                                                (half (the dimension-text width)))
                        (the dimension-text (face-normal-vector :front))
                        (half (the dimension-text length)))))))
  
  
  :computed-slots
  ((sample-point-direction :rear)   
   
   (witness-direction-vector (the (face-normal-vector (if (the flip-leaders?) :left :right))))
   
   ;;
   ;; FLAG -- adjust for start-end-swapped? t
   ;;
   (leader-direction-1-vector (the (face-normal-vector
                                    (if (the start-end-swapped?) :rear :front))))
   
   (leader-direction-2-vector (the (face-normal-vector
                                    (if (the start-end-swapped?) :front :rear))))

   (base-plane-normal (the (face-normal-vector :right)))))

