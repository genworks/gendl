;
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

(define-object revolved-surface (brep surface)

  :documentation 
  (:description "Creates a surface of revolution based on an arbitrary NURBS curve revolved by some angle about
a central axis and axis point."
                  
                  :examples "<pre>

 (in-package :surf)

 (define-object test-revolved-surface (revolved-surface)

   :computed-slots ((display-controls (list :color :green-yellow2)))

   :hidden-objects ((curve :type 'arc-curve
                           :center (translate (the center) :right 50)
                           :orientation (alignment :top (the (face-normal-vector :rear)))
                           :start-angle (half pi)
                           :end-angle (* 3/2 pi)
                           :radius 10)))

 (generate-sample-drawing :objects (make-object 'test-revolved-surface)
                          :projection-direction :trimetric)

 </pre>")

  
  :computed-slots (;;(%native-brep% (the sewn-brep %native-brep%))
                   
                   (%native-brep% (the components brep  %native-brep%))
                   
                   (native-surface (if (eql (the components number-of-elements) 1)
                                       (the (components 0) native-surface)
                                     (error "Revolved-surface, when given a multiple-component curve, is a brep 
which contains a sequence of faces. It is not an actual surface itself. If given a single-component curve,
revolved-surface will be a surface as well as a containing brep."))))
  
  :input-slots
  ((center (make-point 0 0 0) :defaulting)
   
   "GDL Curve object. The profile to be revolved."
   curve 
   
   ("3D Point. The center of revolution. Default value is the <tt>center</tt>."
    axis-point (the center))
   
   ("3D Vector. The direction of axis of revolution. Default is the top of the reference box."
    axis-vector (the (face-normal-vector :top)))
   
   ("Angle in radians. The amount to revolve. Default is twice pi (a full circle of revolution)."
    arc 2pi))

  :hidden-objects
  ((decomposed :type 'decomposed-curves
               :curve-in (the curve))
   
   (components :type 'single-revolved-surface
               :sequence (:size 1)
               :pass-down (curve axis-point axis-vector arc))))
   

(define-object single-revolved-surface (surface)
  :input-slots (curve (axis-point (the center)) (axis-vector (the (face-normal-vector :top))) (arc 2pi))
  
  :computed-slots
  ((native-surface (make-revolved-surface *geometry-kernel* 
                                          :curve (the curve native-curve) 
                                          :axis-point (the axis-point) 
                                          :axis-vector (the axis-vector)
                                          :arc (radians-to-degrees (the arc))
                                          :flag 2))))


(define-object revolved-surfaces (base-object)
  
  :documentation (:description "Creates a set of surfaces of revolution based on a list of arbitrary NURBS curves revolved by some angle about
a central axis and axis point."
                  
                  :examples  "<pre>

 (in-package :gdl-surf-user)

 (define-object test-revolved-surfaces (revolved-surfaces)

   :computed-slots ((curves (list (the curve-1) (the curve-2))))
   
   :hidden-objects ((curve-1 :type 'arc-curve
                             :center (translate (the center) :right 50)
                             :orientation (alignment :top (the (face-normal-vector :rear)))
                             :start-angle 0
                             :end-angle (/ pi 4)
                             :radius 10)
                    
                    (curve-2 :type 'arc-curve
                             :center (translate (the center) :right 50)
                             :orientation (alignment :top (the (face-normal-vector :rear)))
                             :start-angle pi
                             :end-angle (* 5/4 pi)
                             :radius 10)
                           
                    (view :type 'base-view
                          :projection-vector (getf *standard-views* :trimetric)
                          :page-width (* 5 72) :page-length (* 5 72)
                          :object-roots (list self))))

 (generate-sample-drawing :objects (list-elements (the-object (make-object 'test-revolved-surfaces)
                                                              surfaces))
                          :projection-direction :trimetric)
                 
  </pre>")
  
  :input-slots
  ("List of GDL Curve objects. The profiles to be revolved."
   curves
   
   ("3D Point. The center of revolution. Default value is the <tt>center</tt>."
    axis-point (the center))
   
   ("3D Vector. The direction of axis of revolution. Default is the top of the reference box."
    axis-vector (the (face-normal-vector :top)))
   
   ("Angle in radians. The amount to revolve. Default is twice pi (a full circle of revolution)."
    arc 2pi))
  
  :objects
  (("Sequence of GDL Surfaces. The resultant revolved surfaces."
    surfaces :type 'single-revolved-surface
             :sequence (:size (length (the curves)))
             :curve (nth (the-child index) (the curves))
             :pass-down (axis-point axis-vector arc))))



   
   
