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
;; FLAG -- get rid of raw smlib calls in here. 
;;
#+nil
(define-object  gordon-surface (surface)
  
  :documentation (:description "Creates a Gordon surface. It creates a Gordon surface 
interpolating two sets of compatible NON-RATIONAL curves. More precisely, given 
C_i(u) at v[i], i=0,...,k, and C_j(v) at u[j], j= 0,...,l."
                  :examples "<pre>
   </pre>" )
  
  :input-slots
  
  (u-curves u-degree u-degree-tensor u-parameters v-curves v-degree 
   v-degree-tensor v-parameters)
     
  :computed-slots ((native-surface 
                    (smlib::make-gordon-surface *geometry-kernel* 
                                                (mapsend (the  u-curves) :copy-new) 
                                                (mapsend (the  v-curves) :copy-new) 
                                                (the  u-parameters) 
                                                (the  v-parameters) 
                                                (the  u-degree) 
                                                (the  v-degree) 
                                                (the  u-degree-tensor) 
                                                (the  v-degree-tensor)))))

#+nil
(define-object  gordon-surface-with-direct-parameters (surface)
  
  :documentation (:description "Creates a Gordon surface. It creates a Gordon surface interpolating two sets of compatible NON-RATIONAL curves. More precisely, given C_i(u) at v[i], i=0,...,k, and C_j(v) at u[j], j= 0,...,l."
                  :examples "<pre>
   </pre>" )
  
  :input-slots
  
  (u-curves u-degree u-degree-tensor u-parameters v-curves v-degree v-degree-tensor v-parameters)
     
  :computed-slots ((native-surface (smlib::gordon-surf-with-direct-parameters *geometry-kernel* 
                                                               (the  u-curves) 
                                                               (the  v-curves) 
                                                               (the  u-parameters) 
                                                               (the  v-parameters) 
                                                               (the  u-degree) 
                                                               (the  v-degree) 
                                                               (the  u-degree-tensor) 
                                                               (the  v-degree-tensor)))))

#+nil
(define-object test-gordon-surface (base-object)
  
  :computed-slots ((u-curves (list (the curve-u-min ) (the curve-u-0.25 ) 
                                   (the curve-u-0.50 ) (the curve-u-max )))
                   (v-curves (list (the curve-v-min ) (the curve-v-0.25 ) 
                                   (the curve-v-0.50 ) (the curve-v-max )))
                   (u-parameters (list '(0.0 0.25 0.5 1.0) '(0.0 0.25 0.5 1.0) 
                                       '(0.0 0.25 0.5 1.0) '(0.0 0.25 0.5 1.0)))
                   (v-parameters (list '(0.0 0.25 0.5 1.0) '(0.0 0.25 0.5 1.0) 
                                       '(0.0 0.25 0.5 1.0) '(0.0 0.25 0.5 1.0)))
                   (u-degree (the surface u-degree))
                   (v-degree (the surface v-degree))
                   (u-degree-tensor (the surface u-degree))
                   (v-degree-tensor (the surface v-degree))
                   ;;
                   ;;for gordon-surface-with-direct-parameters 
                   ;;
                   (u '(0.0 0.25 0.5 1.0))
                   (v '(0.0 0.25 0.5 1.0)))
  :objects
  ((curve-1 :type 'b-spline-curve 
            :degree 3
            :control-points (list (make-point 0 0 0)
                                  (make-point 3 2 0)
                                  (make-point 5 1 0)
                                  (make-point 7 2 0)
                                  (make-point 9 0 0)))
   
   
   (curve-2 :type 'b-spline-curve
            :degree 3
            :control-points (list (make-point 0 0 1)
                                  (make-point 3 1 1)
                                  (make-point 5 1 1)
                                  (make-point 7 2 1)
                                  (make-point 9 0 1)))
   
   (curve-3 :type 'b-spline-curve
            :degree 3
            :control-points (list (make-point 0 0 2)
                                  (make-point 3 2 2)
                                  (make-point 5 1 2)
                                  (make-point 7 1 2)
                                  (make-point 9 0 2)))
   
   (curve-4 :type 'b-spline-curve
            :degree 3
            :control-points (list (make-point 0 0 4)
                                  (make-point 3 2 4)
                                  (make-point 5 1 4)
                                  (make-point 7 2 4)
                                  (make-point 9 0 4)))
   
   (surface :type 'lofted-surface
            :v-degree 3
            :curves (list (the curve-1)(the curve-2)(the curve-3)(the curve-4)))

   
   (curve-u-min :type 'iso-curve
                :parameter 0.0
                :surface (the surface)
                :u-or-v :u)
   
   (curve-u-0.25 :type 'iso-curve
                 :parameter 0.25
                 :surface (the surface)
                 :u-or-v :u)
   
   (curve-u-0.50 :type 'iso-curve
                 :parameter 0.5
                 :surface (the surface)
                 :u-or-v :u)
   
   (curve-u-max :type 'iso-curve
                :parameter 1.0
                :surface (the surface)
                :u-or-v :u)

   (curve-v-min :type 'iso-curve
                :parameter 0.0
                :surface (the surface)
                :u-or-v :v)

   (curve-v-0.25 :type 'iso-curve
                 :parameter 0.25
                 :surface (the surface)
                 :u-or-v :v)
   
   (curve-v-0.50 :type 'iso-curve
                 :parameter 0.5
                 :surface (the surface)
                 :u-or-v :v)
   
   (curve-v-max :type 'iso-curve
                :parameter 1.0
                :surface (the surface)
                :u-or-v :v)
;;!!!!!!!
   (test-gordon-s :type 'gordon-surface
                  :u-curves (the  u-curves) 
                  :v-curves (the  v-curves) 
                  :u-parameters (the  u-parameters) 
                  :v-parameters (the  v-parameters) 
                  :u-degree (the  u-degree) 
                  :v-degree (the  v-degree) 
                  :u-degree-tensor (the  u-degree-tensor) 
                  :v-degree-tensor (the  v-degree-tensor))
;;!!!!!!!
   (test-gordon-s-d :type 'gordon-surface-with-direct-parameters
                    :u-curves (the  u-curves) 
                    :v-curves (the  v-curves) 
                    :u-parameters (the  u) 
                    :v-parameters (the  v) 
                    :u-degree (the  u-degree) 
                    :v-degree (the  v-degree) 
                    :u-degree-tensor (the  u-degree-tensor) 
                    :v-degree-tensor (the  v-degree-tensor))))
   
   
   
