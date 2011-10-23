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

(define-object planar-offset-curve (curve)
  
  :documentation (:description "Creates a curve which is the result of offsetting a curve by its normals along a plane."
                  :examples "<pre>
 
 (in-package :surf)

 (define-object test-planar-offset-curve ()
  :computed-slots
  ((curve-in (the b-spline-curves (curves 3)))
   
   (plane-normal (make-vector 0 0 -1))
   
   (distance 1) )
  
  :objects
  ((b-spline-curves :type 'test-b-spline-curves
                    :hidden? t)
   
   (curve-to-be-offset :type 'curve
                       :built-from (the b-spline-curves (curves 3)))
   
   
   (planar-offset-curve :type 'surf:planar-offset-curve
                        :curve-in (the curve-to-be-offset)
                        :plane-normal (make-vector 0 0 -1)
                        :distance 1)))

 (generate-sample-drawing :object-roots (list (make-object 'test-planar-offset-curve))
                          :projection-direction (getf *standard-views* :top))

 </pre>")
   
  :input-slots
  ("GDL Curve. The curve to be offset" curve-in 
   
   "3D Vector. The normal for the plane" plane-normal 
   
   "Number. The left-hand distance to offset with respect to curve direction. To get the opposite 
 direction, you can either negate this number or reverse the <tt>plane-normal</tt>." distance
   
   
   ("Number. The tolerance for approximating the resulting offset curve. 
Defaults to *3d-approximation-tolerance-default*." 
    tolerance *3d-approximation-tolerance-default*))

  
  :computed-slots
  ((native-curve-iw (make-offset-curve *geometry-kernel* 
                                       :curve (the curve-in) 
                                       :plane-normal (the plane-normal) 
                                       :offset-distance (- (the distance))
                                       :tolerance (the tolerance)))))


(define-object test-planar-offset-curve ()
  :computed-slots
  ((curve-in (the b-spline-curves (curves 3)))
   
   (plane-normal (make-vector 0 0 -1))
   
   (distance 1) 
   
   (tolerance 0.001)
   )
  
  :objects
  ((b-spline-curves :type 'test-b-spline-curves
                    :hidden? t)
   
   (curve-to-be-offset :type 'curve
                       :built-from (the b-spline-curves (curves 3)))
   
   
   (planar-offset-curve :type 'surf:planar-offset-curve
                        :curve-in (the curve-to-be-offset)
                        :plane-normal (make-vector 0 0 -1)
                        :tolerance (the tolerance)
                        :distance 1)))
  
  
