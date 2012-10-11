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

#|

 (Yes, the curves need to be planar )
 
   1.  offset each curve by the given radius R
   2.  intersect the two offset curves for the center point C of the circle
   3.  project the center point back to each curve to get
        P1 at t1 on the first curve and P2 at t2 on the second curve
   4.  split each curve at t1 and t2
   5.  delete the ends of each curve
   6.  build the circular arc, center C, radius r from P1 to P2.
   
   |#
                  





(define-object filleted-curve (curve)
  :input-slots (segments default-radius (auto-chain? t) (plane-normal (the (face-normal-vector :top))))
  
  :computed-slots ((chained-segments (if (the auto-chain?) 
                                         (let ((segments (order-curves (the segments))))
                                           (unless segments (error "In ~s (~s), segments were unable to be chained." self (the strings-for-display)))
                                           segments)
                                       (assert-ordered-curves (the segments))))
                   
                   (closed? (coincident-point? (the-object (first (the chained-segments)) start)
                                               (the-object (lastcar (the chained-segments)) end)))
                   
                   (sharp-corners (let ((index -1) result)
                                    (mapcar #'(lambda(curve-1 curve-2)
                                                (incf index)
                                                (when (not (same-direction-vectors?
                                                            (the-object curve-1 (tangent (the-object curve-1 u-max)))
                                                            (the-object curve-2 (tangent (the-object curve-2 u-min)))))
                                                  (push index result)))
                                            (the chained-segments) 
                                            (if (the closed?) (append (rest (the chained-segments))
                                                                      (list (first (the chained-segments))))
                                              (rest (the chained-segments)))) (nreverse result)))
                   
                   (built-from (the fillets last composed))
                   
                   
                   (apparently-ok? (dolist (fillet (list-elements (the fillets)) t)
                                     (unless (the-object fillet apparently-ok?)
                                       (return nil)))))
  
  :objects
  ((fillets :type 'curve-fillet
            :sequence (:size (length (the sharp-corners)))
            :pass-down (plane-normal)
            :parent-curve self
            :radius (the default-radius)
            :segment-1 (cond ((the-child first?) (first (the chained-segments)))
                             ((and (the closed?) (the-child last?)) 
                              (make-object 'trimmed-curve 
                                           :built-from (the-child previous composed)
                                           :u1 (+ (the-child previous composed u1)
                                                  (* 0.5 (- (the-child previous composed u2)
                                                            (the-child previous composed u1))))))
                             (t (the-child previous composed)))
            :segment-2 (if (and (the (closed?)) (the-child last?))
                           (make-object 'trimmed-curve 
                                        :built-from (the-child previous composed)
                                        :u2 (- (the-child previous composed u2)
                                               (* 0.5 (- (the-child previous composed u2)
                                                         (the-child previous composed u1)))))
                         (nth (if (< (the-child index) (length (the sharp-corners)))
                                  (1+ (nth (the-child index) (the sharp-corners))) 0)
                              (the chained-segments))))))




(define-object curve-fillet (base-object)
  :input-slots (parent-curve radius segment-1 segment-2 plane-normal )
  
  
  :computed-slots ((center 
                    (let ((intersects (the segment-1-offset (curve-intersection-points (the segment-2-offset)))))
                      (get-3d-point-of (lastcar intersects))))

                   (apparently-ok? (and (the center)
                                        (> (get-parameter-of (the segment-1-drop))
                                           (the segment-1 u1))
                                        (< (get-parameter-of (the segment-2-drop))
                                           (the segment-2 u2))))

                   (segment-1-drop (when (the center) (the segment-1 (dropped-point (the center)))))
                   (segment-2-drop (when (the center) (the segment-2 (dropped-point (the center)))))
                   
                   (fillet-p1 (get-3d-point-of (the segment-1-drop)))
                   (fillet-p2 (get-3d-point-of (the segment-2-drop)))
                   
                   (offset-direction (let ((angle (angle-between-vectors-d (reverse-vector (the segment-1 (tangent (the segment-1 u-max))))
                                                                           (the segment-2 (tangent (the segment-2 u-min)))
                                                                           (the plane-normal))))
                                       (if (< angle 180) :left :right))))

  
  :objects
  ((fillet-arc :type 'arc-curve 
               :pass-down (radius))

   (segment-1-offset :type 'planar-offset-curve
                     :curve-in (the segment-1)
                     :distance (ecase (the offset-direction) (:right (the radius)) (:left (- (the radius))))
                     :pass-down (plane-normal))
   
   (segment-2-offset :type 'planar-offset-curve
                     :curve-in (the segment-2)
                     :distance (the segment-1-offset distance)
                     :pass-down (plane-normal))
  
   (composed :type 'composed-curve
             :curves (list (the segment-1-trimmed) (the fillet) (the segment-2-trimmed)))
   
   (fillet :type 'arc-curve
	   :pseudo-inputs (angle-1 angle-2 swap?)
           :pass-down (radius)
           :angle-1 (angle-between-vectors (the (face-normal-vector :right)) (subtract-vectors (the fillet-p1) (the center))
                                           (the (face-normal-vector :top)) :-ve t)
         
           :angle-2 (angle-between-vectors (the (face-normal-vector :right)) (subtract-vectors (the fillet-p2) (the center))
                                           (the (face-normal-vector :top)) :-ve t)
         
           :swap? (minusp (angle-between-vectors (subtract-vectors (the fillet-p2) (the center))
                                                 (subtract-vectors (the fillet-p1) (the center))
                                                 (the (:face-normal-vector :top)) :-ve t))
           :start-angle (if (the-child swap?) (the-child angle-1) (the-child angle-2))
           :end-angle (if (the-child swap?) (the-child angle-2) (the-child angle-1)))
   
   
   (segment-1-trimmed :type 'trimmed-curve
                      :built-from (the segment-1)
                      :u2 (let ((apparently-ok? (the apparently-ok?)))
                            (unless apparently-ok?
                              (error "Bummer! Filleting failed for ~s in ~s (~s).
Center is ~s, segment-1 drop parameter is ~s, and segment-1 u1 is ~s."
                                     self (the parent-curve) (the parent-curve strings-for-display)
                                     (the center) (get-parameter-of (the segment-1-drop))
                                     (the segment-1 u1)))
                            (get-parameter-of (the segment-1-drop))))
   
   (segment-2-trimmed :type 'trimmed-curve
                      :built-from (the segment-2)
                      :u1 (get-parameter-of (the segment-2-drop)))))


;;
;; The following is unsupported, for reference only for user code
;;
;; Derived from Corus filleted-curve-safe example.
;;

(define-object filleted-curve-safe (curve)
  
  :input-slots
  (segments
   default-radius
   (auto-chain? t)
   (find-closest-radius? t)
   (step-increment 1)
   (step-max 25)
   (debug? nil))
  
  
  :computed-slots
  ((built-from (the fillet))

   (closest-radius (find-closest-radius (the default-radius) (the fillet segments)
                                        :step-increment (the step-increment)
                                        :step-max (the step-max) 
                                        :debug? (the debug?)))

   (fillet-radius (if (the find-closest-radius?)
                      (the closest-radius)
                    (the default-radius))))
  
  :objects
  ((fillet :type 'filleted-curve
           :pass-down (segments auto-chain?)
           :default-radius (the fillet-radius))))



(defun find-closest-radius (radius curves &key (step-increment 1)
                                               (step-max 25)
                                               (debug? nil))
  ;; step-increment is percentage decrease in radius at each step
  ;; step-max is maximum percentage decrease in radius
  ;; debug? set to T prints debug output
  
  (when debug? (print-variables radius))
                
  (do* ((i 0 (incf i))
        (rad radius (* (div (- 100 (* i step-increment)) 100) radius))
        (filleted (make-object 'filleted-curve :segments curves :default-radius rad)
                  (make-object 'filleted-curve :segments curves :default-radius rad))
        (apparently-ok? (the-object filleted apparently-ok?)
                        (the-object filleted apparently-ok?)))
      ((or apparently-ok? (>= (* i step-increment) step-max)) 
       (progn (print-variables rad) rad))
    (when debug? (print-variables rad))))
  

