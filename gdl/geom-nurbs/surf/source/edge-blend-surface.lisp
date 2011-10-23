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

(define-object edge-blend-surface (general-dual-blend-surface)
  
  :documentation (:description "Creates a smooth blend-surface between curve-1, lying on surface-1 and curve-2 on surface-2. 

Note that curve-1 and curve-2 have to be so-called on-surface curves, which means they must answer a uv-curve message which is the UV representation of the curve on the given surface. The most common way to establish an on-surface curve is to use an iso-curve to begin with, or to use a projected-curve or dropped-curve to ensure that the curve is indeed an on-surface curve.

The local start and end directions of this surface at any point along curve-1 and curve-2 are determined from the cross-product between the tangent to the surface's u- or v-iso-curve (the one that is closest to being parallel to curve-1 or curve-2) at this point and the corresponding surface-normal at the same point. In this fashion a tangent blend is created between surface-1 and surface-2 that extends in a direction that follows and smoothly interpolates both surface's iso-curves. Takes the same inputs as general-dual-blend-surface, except for f-tangent-1 and f-tangent-2."
                  
                  :examples "<pre>

 (in-package :surf)

 (define-object test-e-b-s (base-object)
  
  
  :objects
  ((e-b-s :type 'edge-blend-surface
          :display-controls (list :color :green)
          :curve-side-1 :left-side
          :curve-side-2 :left-side
          :pass-down (curve-1 surface-1 curve-2 surface-2))
                                    
   
   (surf-1-top :type 'linear-curve
               :hidden? t
               :start (make-point -5 -5 0)
               :end (make-point 5 -5 0))
    
   (surf-1-bottom :type 'linear-curve
                  :hidden? t
                  :start (make-point -7 -10 -2)
                  :end (make-point 7 -10 -2))
    
   (surface-1 :type 'ruled-surface
              :curve-1 (the surf-1-top)
              :curve-2 (the surf-1-bottom))
    
   (curve-1 :type 'iso-curve
            :display-controls (list :color :red :line-thickness 4)
            :surface (the surface-1)
            :parameter 0
            :u-or-v :v)
    
   (surf-2-bottom :type 'linear-curve
                  :hidden? t
                  :start (make-point -5 5 0)
                  :end (make-point 5 5 0))
    
   (surf-2-top :type 'linear-curve
               :hidden? t
               :start (make-point -7 10 2)
               :end (make-point 7 10 2))
    
   (surface-2 :type 'ruled-surface
              :curve-1 (the surf-2-bottom)
              :curve-2 (the surf-2-top))
    
   (curve-2 :type 'iso-curve
            :display-controls (list :color :blue :line-thickness 4)
            :surface (the surface-2)
            :parameter 0
            :u-or-v :v)))

 (generate-sample-drawing :object-roots (list (make-object 'test-e-b-s))
                           :projection-direction (getf *standard-views* :top))

 </pre>")
  
  :input-slots (("Keyword. Used to specify the side w.r.t curve-1 in which the tangent blend-surface is to extend. Takes either :right-side or :left-side as input. Defaults to :right-side." curve-side-1 :right-side)
                
                
                ("Keyword. Used to specify the side w.r.t curve-2 in which the tangent blend-surface is to extend. Takes either :right-side or :left-side as input. Defaults to :right-side." curve-side-2 :right-side)
                
                
                ("GDL UV curve object. Defaults to the curve-1 uv-curve."
                 curve-1-uv (the curve-1 uv-curve))

                ("GDL UV curve object. Defaults to the curve-2 uv-curve."
                 curve-2-uv (the curve-2 uv-curve))
                
                (curve-1-u-or-v :u)
                
                (curve-1-accessor (ecase (the curve-1-u-or-v)
                                    (:u #'get-u)
                                    (:v #'get-v)))
                
                (curve-2-u-or-v :u)
                
                (curve-2-accessor (ecase (the curve-2-u-or-v)
                                    (:u #'get-u)
                                    (:v #'get-v))))
  
  :computed-slots
  (
   (f-tangent-1 #'(lambda(param)
                    (let* (#+nil
                           (surface-uv (the curve-1 (uv-curve (the surface-1)) 
                                            (point param)))
                           
                           (surface-uv (the curve-1-uv (point param)))
                           
                           (surface-iso (make-object 'iso-curve
                                                     :u-or-v (the curve-1-u-or-v)
                                                     :surface (the surface-1)
                                                     :parameter (funcall (the curve-1-accessor) surface-uv)))
                           (tangent-accessor (if (eql (the curve-1-accessor) #'get-y)
                                                 #'get-x
                                               #'get-y)))
                      (ecase (the curve-side-1)
                        (:right-side
                         (the-object surface-iso (tangent (funcall tangent-accessor surface-uv))))
                        (:left-side
                         (reverse-vector (the-object surface-iso (tangent (funcall tangent-accessor surface-uv)))))))))

   
   (f-tangent-2 #'(lambda(param)
                    (let* ((surface-uv (the curve-2-uv (point param)))
                           
                           (surface-iso (make-object 'iso-curve
                                                     :u-or-v (the curve-2-u-or-v)
                                                     :surface (the surface-2)
                                                     :parameter (funcall (the curve-2-accessor) surface-uv)))
                           (tangent-accessor (if (eql (the curve-1-accessor) #'get-y)
                                                 #'get-x
                                               #'get-y)))
                      
                      (ecase (the curve-side-2)
                        (:right-side
                         (the-object surface-iso (tangent (funcall tangent-accessor surface-uv))))
                        (:left-side
                         (reverse-vector (the-object surface-iso (tangent (funcall tangent-accessor surface-uv)))))))))))




