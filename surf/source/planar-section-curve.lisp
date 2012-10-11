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

(define-object planar-section-curve (curve)
  
  :documentation (:description "Produces a single curve by sectioning a surface with a plane. If multiple results are expected,
use planar-section-curves instead."
                  
                  :examples "<pre>

 (in-package :surf)

 (define-object test-planar-section-curve (base-object)
  
  :objects
  ((planar-section-curve :type 'planar-section-curve
                         :surface (the test-surf)
                         :plane-normal (the (face-normal-vector :top))
                         :plane-point (make-point 0 0 2)
                         :display-controls (list :color :red :line-thickness 4))
   
   (test-surf :type 'test-fitted-surface )))

 (generate-sample-drawing :object-roots (list (make-object 'test-planar-section-curve))
                           :projection-direction (getf *standard-views* :trimetric))

 </pre>")
  
  :input-slots
  ("GDL Surface, face, or trimmed surface. The surface to be sectioned with a plane."
   surface
   
   ("Number. Tolerance used when approximating in e.g. Newton-Raphson iterations.
Default is *3d-approximation-tolerance-default*."
    3d-approximation-tolerance *3d-approximation-tolerance-default*)
   
   ("Number. Angular tolerance (in radians) used when approximating in e.g. Newton-Raphson iterations.
Default is *angle-tolerance-radians-default*."
    angle-tolerance-radians *angle-tolerance-radians-default*)
   
   ("Vector. The normal of the sectioning plane. Defaults to the top vector of the local reference box."
    plane-normal (the (face-center :top)))
   
   ("3D Point. A point on the sectioning plane. Defaults to the center."
    plane-point (the center)))

  :computed-slots
  (
   
   (on-surfaces (list (the surface)))
   
   ("Boolean. This will be non-nil if the curve was generated successfully."
    success? (the %native-curve-iw%))
   
   (native-curve-iw% (getf (the results-plist) :3d-curve))
   
   (results-plist (multiple-value-bind (3d-curves uv-curves 3d-approximation-tolerance angle-tolerance-radians)
                      (intersect-surface-with-plane *geometry-kernel* 
                                                    (the surface)(the plane-normal)(the plane-point)
                                                    :3d-approximation-tolerance (the 3d-approximation-tolerance)
                                                    :angle-tolerance-radians (the angle-tolerance-radians)
                                                    :finalize-on self)
                    (if (consp (rest uv-curves))
                        (error "Intersecting resulted in ~a curves. Please use planar-section-curves instead.~%" 
                               (length uv-curves))
                      (list :uv-curve (first uv-curves) :3d-curve (first 3d-curves)
                            :3d-approximation-tolerance 3d-approximation-tolerance
                            :angle-tolerance-radians angle-tolerance-radians))))

    
   
   (native-trimmed-curve-iw (when (and (typep (the surface) 'face) (the native-curve-iw%))
                              (first (the surface (trim-curves-to-face (list (the native-curve-iw%)))))))


   
   (%native-curve-iw% (or (the native-trimmed-curve-iw) (the native-curve-iw%)))
   
   (native-curve-iw (or (the %native-curve-iw%)  (error "The planar-section-curve found no results."))))

  
  :hidden-objects
  ((%uv-curve% :type 'curve
               :native-curve-iw (getf (the results-plist) :uv-curve)))
  
  
  :functions
  (("GDL Curve object. The UV curve for this curve in the context of the surface.

:&optional (surface (the surface)) \"GDL Surface object. The surface on which the UV curve lies.\""
    uv-curve
    (&optional (surface (the surface)))
    (if (eql surface (first (the on-surfaces)))
        (the %uv-curve%)
      (error "This curve is not on that surface.")))))


(define-object planar-section-curves (base-object)
  
  :documentation (:description "Produces multiple curves by sectioning a surface or a brep with a plane.
If a single result is expected, use planar-section-curve instead."
                  :examples  "<pre>

 (in-package :surf)
 
 (define-object test-planar-section-curves (base-object)
  
  :computed-slots
  ((points-data '(((0 0 0)(0 1 0)(1 1 0)(1 0 0))
                  ((0 0 1) (0 1 1) (1 1 1) (1 0 1) )
                  ((0 0 2) (0 1 2) (1 1 2) (1 0 2) )
                  ((0 0 3) (0 1 3) (1 1 3) (1 0 3) )))
   
   (control-points (mapcar #'(lambda(list) (mapcar #'apply-make-point list)) 
                           (the points-data))))
   
  :objects
  ((planar-section-curve :type 'planar-section-curves
                         :surface (the test-surf)
                         :plane-normal (the (face-normal-vector :front))
                         :plane-point (make-point 0 0.5 0)
                         :display-controls (list :color :red :line-thickness 4))
   
   (test-surf :type 'b-spline-surface 
               :control-points (the control-points)) ))

 (generate-sample-drawing :object-roots 
                          (list (make-object 'test-planar-section-curves))
                           :projection-direction 
                           (getf *standard-views* :trimetric))

 </pre>" )
  
  :input-slots
  (("GDL Surface object. The surface to be sectioned with a plane. Specify this or brep, not both."
    surface nil)

   ("GDL Brep object. The brep to be sectioned with a plane. Specify this or surface, not both."
    brep nil)
   
   ("Number. Tolerance used when approximating in e.g. Newton-Raphson iterations.
Default is *3d-approximation-tolerance-default*."
    3d-approximation-tolerance *3d-approximation-tolerance-default*)
   
   ("Number. Angular tolerance (in radians) used when approximating in e.g. Newton-Raphson iterations.
Default is *angle-tolerance-radians-default*."
    angle-tolerance-radians *angle-tolerance-radians-default*)
   
   ("Vector. The normal of the sectioning plane. Defaults to the top vector of the local reference box."
    plane-normal (the (face-center :top)))
   
   ("3D Point. A point on the sectioning plane. Defaults to the center."
    plane-point (the center)))

  :computed-slots
  (
   (native-curves-iw  (getf (the data-plist) :native-curves-iw))
   
   (native-uv-curves-iw (getf (the data-plist) :native-uv-curves-iw))
   
   ("Number. The actual tolerance achieved by the operation."
    3d-approximation-tolerance-achieved (getf (the data-plist) :3d-approximation-tolerance))
   
   ("Number. The actual angle tolerance achieved by the operation."
    angle-tolerance-radians-achieved (getf (the data-plist) :angle-tolerance-radians))

   
   (data-plist 
    (progn
      (when (and (the brep) (the surface))
        (error "planar-section-curves can only be called with brep or surface, not both.~%"))
      (cond ((the surface)
             
             (multiple-value-bind (3d-curves uv-curves 3d-approximation-tolerance angle-tolerance-radians)
                 (intersect-surface-with-plane *geometry-kernel* (the surface) (the plane-normal) (the plane-point)
                                               :3d-approximation-tolerance (the 3d-approximation-tolerance)
                                               :angle-tolerance-radians (the angle-tolerance-radians)
                                               :finalize-on self)
               (list :native-curves-iw 3d-curves :native-uv-curves-iw uv-curves
                     :3d-approximation-tolerance 3d-approximation-tolerance :angle-tolerance-radians angle-tolerance-radians)))
            ((the brep)
             (multiple-value-bind (3d-curves uv-curves faces)
                 (brep-planar-section-curves *geometry-kernel*
                                             (the brep %native-brep%)
                                             (the plane-point)
                                             (the plane-normal)
                                             :tolerance (the 3d-approximation-tolerance)
                                             :angle-tolerance-radians (the angle-tolerance-radians))
               (list :native-curves-iw 3d-curves 
                     :native-uv-curves-iw uv-curves
                     :faces faces)))
             
            (t (error "planar-section-curves must be provided with a brep or a surface.")))))
   
   
   (native-trimmed-curves-iw (if (typep (the surface) 'face)
                                 (the surface (trim-curves-to-face (the native-curves-iw)))
                               (the native-curves-iw))))
  
  :objects
  (
   (faces 
    :type 'face
    :sequence (:size (length (getf (the data-plist) :faces)))
    :%native-face% (nth (the-child index) (getf (the data-plist) :faces)))
   
   ("Sequence of GDL uv curve objects. The UV curves for each returned curve. This is also passed into each 
curve object and available from there."
    uv-curves 
    :type 'curve
    :sequence (:size (length (getf (the data-plist) :native-uv-curves-iw)))
    :native-curve-iw (nth (the-child index) (the native-uv-curves-iw)))
   
   ("Sequence of GDL Curve Objects. The curves resulting from sectioning."
    curves ;;:type 'trimmed-curve
    :type 'curve
    :sequence (:size (length (the native-trimmed-curves-iw)))
    :native-curve-iw (nth (the-child index) (the native-trimmed-curves-iw))
    :face (when (the brep) (the (faces (the-child index))))
    :pseudo-inputs (face)
    :uv-curve (the (uv-curves (the-child index))))))



  


