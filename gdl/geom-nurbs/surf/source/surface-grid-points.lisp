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

;;
;; FLAG -- get rid of raw smlib calls in here
;;
#+nil
(define-object surface-grid-points (base-object)
  
  :computed-slots
  (( test-quad (smlib::approximated-surface-with-quads *geometry-kernel*  (the test-surf native-surface)  :tolerance 0.01 :tolerance-method 1))

   
   
   
   (points (surf-grid-points *geometry-kernel* 
                             (the test-surf
                                 
                              ;; test 
                               native-surface)
                             :u-parameters '(0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
                             :v-parameters '(0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0))))

  :objects
  ((test-surf  :type 'test-b-spline-surface)
   (grid-pints :type 'grid-points-sequence
               :sequence (:size (length (the points)))
               :points-list (nth (the-child :index) 
                                (the points)))
   
   (test :type 'planar-surface
         :p00 (make-point 0 0 0)
         :p01 (make-point 0 1 0)
         :p10 (make-point 1 0 0)
         :p11 (make-point 2 1 0))))
         

   
#+nil   
(define-object grid-points-sequence (base-object)
  
  :input-slots
  (points-list)
  :objects
  ((point :type 'point
                :sequence (:size (length (the points-list)))
                    :center (nth (the-child :index) 
                                 (the points-list)))))
