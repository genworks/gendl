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


(define-object dual-blend-surface (general-dual-blend-surface)
  
   :documentation (:description "Creates a smooth blend-surface between curve-1, lying on surface-1 and curve-2 on surface-2. The local start and end directions of this surface along curve-1 and curve-2 are determined from the cross-product between the tangent of these curves and the corresponding surface-normal at the same point. In this fashion a tangent blend is created between surface-1 and surface-2 that extends in a direction perpendicular to the input-curves. Takes the same inputs as general-dual-blend-surface, except for f-tangent-1 and f-tangent-2."
                  
                   :examples "<pre>

 (in-package :surf)

 (define-object test-d-b-s (base-object)
  
  :objects
  ((d-b-s :type 'dual-blend-surface
          :display-controls (list :color :green)
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

 (generate-sample-drawing :object-roots (list (make-object 'test-d-b-s))
                           :projection-direction (getf *standard-views* :top))
 </pre>")
  
  :computed-slots
  ((f-tangent-1 #'(lambda(param)
                    (let* ((surface-uv (the curve-1 uv-curve (point param)))
                           (surface-normal (the surface-1 (normal (get-x surface-uv) (get-y surface-uv))))
                           (curve-tangent (the curve-1 (tangent param))))
                      (cross-vectors surface-normal curve-tangent))))
   
   (f-tangent-2 #'(lambda(param)
                    (let* ((surface-uv (the curve-2 uv-curve (point param)))
                           (surface-normal (the surface-2 (normal (get-x surface-uv) (get-y surface-uv))))
                           (curve-tangent (the curve-2 (tangent param))))
                      (cross-vectors surface-normal curve-tangent))))))


;;
;; FLAG -- move to example and/or regression tests. 
;;
;;  Some lisps don't recognize the above dual-blend-surface definition while still
;;  in this file. 
;;
#+nil
(define-object test-d-b-s (base-object)
  
  
  :objects
  ((d-b-s :type 'dual-blend-surface
          :display-controls (list :color :green)
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
