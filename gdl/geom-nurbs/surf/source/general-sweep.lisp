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


(define-object general-sweep (b-spline-surface)
  
  :input-slots
  (guide-curve 
   profile-curve
   v1-function
   v2-function
   
   (nsegments (1- (length (the guide-curve control-points))))
   
   (untwist? nil)
   
   (v-degree 3))
  
  
  :computed-slots
  (
   
   (control-points 
    (mapcar #'(lambda(profile)
                (the-object profile control-points))
            (cons (the start-profile)
                  (list-elements (the rest-profiles rest-profiles)))))

   (weights
    (mapcar #'(lambda(profile)
                (the-object profile weights))
            (cons (the start-profile)
                  (list-elements (the rest-profiles rest-profiles)))))

   
   ;;(u-knot-vector (the profile-curve knot-vector))
   (v-knot-vector (the guide-curve knot-vector))
   
   ;;(u-degree (the profile-curve degree))
   
   (u-degree 1)
   
   
   (guide-control-grid-length 
    (apply #'+ (the guide-control-distances)))

   
   (guide-control-distances (mapcar #'3d-distance
                                    (the  guide-curve control-points)
                                    (rest (the  guide-curve control-points))))
   
   
   (guide-control-cumulative-distances (let ((total 0))
                                         (mapcar #'(lambda(length)
                                                     (setq total 
                                                       (+ total length)))
                                                 (the guide-control-distances))))
   
   (guide-curve-params (make-array (length (the guide-control-cumulative-distances))
                                   :initial-contents 
                                   (mapcar #'(lambda(cumulative-length)
                                               (* 
                                                (/ cumulative-length 
                                                   (the guide-control-grid-length))
                                                (the guide-param-extent)))
                                           (the guide-control-cumulative-distances))))
   
   (guide-param-extent (- (the guide-curve u-max)
                          (the guide-curve u-min)))
                          

   (guide-curve-stations (make-array (length (the guide-curve-params))
                                     :initial-contents
                                     (mapcar #'(lambda(param) (the guide-curve (point param)))
                                             (coerce (the guide-curve-params) 'list)))
                         
                         ;;
                         ;; FLAG -- why doesn't it work this way?
                         ;;
                         #+nil
                         (map 'array #'(lambda(param) (the guide-curve (point param)))
                              (the guide-curve-params))))

  
  :objects
  (
   ;;
   ;; FLAG -- investigate using guide curve as spine
   ;;
   (lofted :type 'lofted-surface
           :synchronized? t
           :curves (cons (the start-profile)
                         (list-elements (the rest-profiles rest-profiles))))
   
   (start-profile :type 'boxed-curve
                  :curve-in (the profile-curve)
                  :center (the guide-curve start)
                  :orientation (alignment ;;:right
                                          ;;(funcall (the v1-function)
                                                ;;   (the guide-curve u1)
                                :top 
                                (funcall (the v2-function)
                                         (the guide-curve u1))))

   (rest-profiles :type 'profile-set
                  :curve-in (the start-profile)
                  :guide-curve-control-points (the guide-curve control-points)
                  :pass-down (guide-curve-params guide-curve-stations 
                              v1-function v2-function))))


(define-object normal-sweep (general-sweep)
  
  :computed-slots
  ((v2-function #'(lambda(param)
                     (the guide-curve (tangent param))
                     ))

   
   (v1-function #'(lambda(param)
                     (the (face-normal-vector :bottom))))))



(define-object profile-set (base-object)
  :input-slots (curve-in  guide-curve-params guide-curve-stations 
                v1-function v2-function guide-curve-control-points)
  
  :objects ((rest-profiles :type 'boxed-curve
                           :curve-in (the curve-in)
                           :sequence (:size (length (the guide-curve-params)))
                           
                           :center
                           (aref (the guide-curve-stations)
                                 (the-child index))

                           :orientation (let ((param (aref (the guide-curve-params)
                                                           (the-child index))))
                                          
                                          (alignment 
                                           
                                           :right
                                           (funcall (the v1-function) param)
                                           
                                           
                                           :top 
                                           
                                           (funcall (the v2-function) param)
                                            
                                           )))))




(define-object test-g-s (general-sweep)
  
  :computed-slots
  (
   ;;(display-controls (list :isos (list :n-u 80 :n-v 20)))
   
   (profile-curve (the test-data-set expected-surf-data1 (input-entities 1)))
   (guide-curve (the test-data-set expected-surf-data1 (input-entities 0)))
   
   (v1-function #'(lambda(param)
                     (the guide-curve (tangent param))
                     ))

                     
   
   (v2-function #'(lambda(param)
                    (cross-vectors
                     (the (face-normal-vector :right))
                     (the guide-curve (tangent param)))))
   
   
   (expected-result (the test-data-set expected-surf-data1 entity))
   
   )
  
  :objects
  ((test-data-set :type 'test-guide-normal-on-surface-sweep)
   
   (profile-view :type 'curve
                 :built-from (the profile-curve))
   
   (guide-view :type 'curve
               :built-from (the guide-curve))
   
   (guide-points :type 'points-display
                 :points (the guide-curve control-points))
   
   
   (dropped :type 'dropped-curve
            :curve-in (the guide-curve)
            :surface (the test-data-set expected-surf-data1 (input-entities 2)))
   
   
   ))
  

