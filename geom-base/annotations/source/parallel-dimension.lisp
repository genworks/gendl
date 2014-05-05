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

(define-object parallel-dimension (linear-dimension)

  :documentation (:description "Creates a dimension annotation along an axis from a start point to an end point."
                  
                  
                  :examples "<pre>        

 (in-package :gdl-user)
                   
 (define-object  parallel-dimension-sample (base-object)
  
  
   :objects
   ((box :type 'box
         :length 10 :width (* (the-child length) +phi+)
         :height (* (the-child :width) +phi+))
   
    (length-dimension :type 'parallel-dimension
                      :character-size (/ (the box length) 20)
                      :start-point (the box (vertex :top :left :front))
                      :end-point (the box (vertex :top :right :rear)))))

  (generate-sample-drawing :object-roots (make-object 'parallel-dimension-sample))

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
                                   (translate-along-vector mid-point
                                                           (the rear-vector)
                                                           (* (the dim-text-above-gap-factor)
                                                              (half (the dimension-text length)))))
                                  ((and (not (the outside-leaders?)) 
                                        (not (the text-above-leader?))
                                        (the text-along-axis?)) mid-point)
                                  ((and (not (the outside-leaders?)) 
                                        (the text-above-leader?)
                                        (not (the text-along-axis?)))
                                   (translate-along-vector mid-point (the rear-vector) (half (the dimension-text width))))
                                  ((and (not (the outside-leaders?)) 
                                        (not (the text-above-leader?))
                                        (not (the text-along-axis?)))
                                   (translate-along-vector mid-point (the front-vector) (half (the dimension-text width))))
                                  (t (ecase (the dim-text-bias)
                                       (:center mid-point)
                                       (:start (translate-along-vector (the leader-start) 
                                                                       (if (the start-end-swapped?) 
                                                                           (the rear-vector) 
									   (the front-vector))
								       (+ (the leader-line-length)
									  (half (the dimension-text width)))))
                                       (:end (translate-along-vector (the leader-end) 
                                                                     (if (the start-end-swapped?) 
                                                                         (the rear-vector)
									 (the front-vector))
                                                                     (+ (half (the dimension-text width))
                                                                        (the leader-line-length-2)))))))))
                       (translate-along-vector
                        (translate-along-vector center  (the left-vector)
                                                (half (the dimension-text width)))
                        (the front-vector)
                        (half (the dimension-text length)))))))

  :computed-slots
  ((left-vector (the dimension-text (face-normal-vector :left)))
   (right-vector (the dimension-text (face-normal-vector :right)))
   (front-vector (the dimension-text (face-normal-vector :front)))
   (rear-vector (the dimension-text (face-normal-vector :rear)))
   
   
   
   
   
   (full-leader-break-points 
    (when (and *break-leaders?*
               (not (the text-above-leader?)))
      (let ((start (the leader-start))
            (end (the leader-end))
            (overage (* .2 (the dimension-text width))))
        (let ((start-end-vector (subtract-vectors end start)))
          (let ((first (translate-along-vector start
                                               start-end-vector
                                               (- (3d-distance start 
                                                               (the dim-text-start))
                                                  overage))))
            (list first
                  (translate-along-vector first start-end-vector 
                                          (+ (twice overage) 
                                             (the dimension-text width)))))))))
   
   
   (witness-direction-vector (cross-vectors (if (the flip-leaders?) 
                                                (the leader-direction-1-vector) 
                                              (the leader-direction-2-vector))
                                            (the top-vector)))

   (top-vector (if (parallel-vectors? (subtract-vectors (the start-point) (the end-point))
                                      (the (face-normal-vector :top)))
                   (the (face-normal-vector :rear))
                 (the (face-normal-vector :top))))
                     
   
   (leader-direction-1-vector (orthogonal-component  (subtract-vectors (the start-point) (the end-point))
                                                     (the top-vector)))
   
   (leader-direction-2-vector (reverse-vector (the leader-direction-1-vector)))
   
   (base-plane-normal (the witness-direction-vector))))
