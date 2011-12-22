;;
;; Copyright 2002, 2009 Genworks International and Genworks BV 
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

(in-package :gdl-lift-tests)

(define-object extended-surface-test (base-object)
  
   :computed-slots
   ((regression-test-data (list (multiple-value-list (the extended b-spline-data))
                                (multiple-value-list (the extended-2 b-spline-data))
                                (multiple-value-list (the extended-3 b-spline-data))
                                (multiple-value-list (the extended-4 b-spline-data))))
   
    (display-list-objects (list (the loft)
                                (the extended)
                                (the extended-2))))
  
   :objects
   ((test3 :type 'linear-curve 
           :start (make-point 0 0 0) 
           :end (make-point 10 0 0))
   
    (test4 :type 'linear-curve
           :start (make-point 0 10 0) 
           :end (make-point 10 10 0))
   
   
    (mid-1 :type 'linear-curve
           :start (make-point 0 5.0 1)
           :end (make-point 10 5.0 1))
   
    (mid-2 :type 'linear-curve
           :start (make-point 0 8.0 1)
           :end (make-point 10 8.0 1))
   
    (bridge-1 :type 'b-spline-curve
              :control-points (list (make-point 0 0 0)
                                    (make-point -2 5.0 3) 
                                    (make-point -2 8.0 3) 
                                    (make-point 0 10 0)))       
    (bridge-2 :type 'b-spline-curve
              :control-points (list (make-point 10 0 0)
                                    (make-point 12 5.0 5) 
                                    (make-point 12 8.0 5) 
                                    (make-point 10 10 0)))      

    (bridge-3 :type 'b-spline-curve
              :control-points (list (make-point 0 -1 0)
                                    (make-point 3 -1 5)
                                    (make-point 7 -1 5)
                                    (make-point 10 -1 0)))

   
    (loft :type 'lofted-surface
          :curves (list (the test3) (the mid-1) 
                        (the mid-2) (the test4)))
   
    (extended :type 'extended-surface
              :display-controls (list :color :red :line-thickness 2)
              :surface (the loft)
              :curve (the bridge-1)
              :direction :v
              :which-end :start)

    (extended-2 :type 'extended-surface
                :display-controls (list :color :green :line-thickness 2)
                :surface (the loft)
                :curve (the bridge-1)
                :direction :v
                :which-end :start
                :deformation-param (+ (* 0.25 (- (the-child surface v-max)
                                                 (the-child surface v-min)))
                                      (the-child surface u-min)))
   
    (extended-3 :type 'extended-surface
                :display-controls (list :color :orange
                                        :isos (list :n-u 25 :n-v 25))
                :surface (the loft)
                :curve (the bridge-3)
                :direction :u
                :continuity :cmax
                :which-end :start)
   
    (extended-4 :type 'extended-surface
                :display-controls (list :color :blue 
                                        :isos (list :n-u 25 :n-v 25))
                :surface (the loft)
                :curve (the bridge-3)
                :direction :u
                :deformation-param (+ (* 0.25 (- (the-child surface u-max)
                                                 (the-child surface u-min)))
                                      (the-child surface u-min))
                :continuity :cmax
                :which-end :start)))

(register-test-definition 'extended-surface-test)
