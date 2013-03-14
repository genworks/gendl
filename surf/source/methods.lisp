;;
;; Copyright 2002-2011, 2012 Genworks International
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

(defmacro define-vanilla-wrapper (name (&rest args))
  `(defmethod ,name ((geometry-kernel vanilla-geometry-kernel) ,@args)
     (declare (ignore ,@(remove '&key args)))
     (error "Don't know how to do ~a using ~s.~%" ',name geometry-kernel)))

(defun make-uniform-knot-vector (cpoint-length degree)
  (let* ((knot-vector-length (+ cpoint-length degree 1))
         (clamp-length (1+ degree))
         (internal-length (- knot-vector-length (twice clamp-length))))

    (when (minusp internal-length) (error "Cannot make knot vector with ~a points and degree ~a.~%"
                                          cpoint-length degree))
    (append (make-list clamp-length :initial-element 0.0)
            (let ((increment (/ (1+ internal-length))) result)
              (dotimes (i internal-length (nreverse result))
                (push (* (1+ i) increment) result)))
            (make-list clamp-length :initial-element 1.0))))

(defmethod make-geometry-kernel ((type t) &key (in-memory t) (host nil) (login nil) (password nil))
  (declare (ignore in-memory host login password))
  (error "Don't know about geometry kernel type `~s'. Please try (require ~(~s~)).~%" type type))

(defmethod make-geometry-kernel ((type (eql :vanilla)) &key (in-memory t) (host nil) (login nil) (password nil))
  (declare (ignore in-memory host login password))
  (setq *geometry-kernel* (make-instance 'vanilla-geometry-kernel)))

(unless *geometry-kernel* (make-geometry-kernel :vanilla))

(defmethod decomposed-curves ((geometry-kernel vanilla-geometry-kernel) native-curve)
  (declare (ignore native-curve))
  (error "Don't know how to decompose nurbs using ~s.~%" geometry-kernel))

(defmethod nurbs-to-beziers ((geometry-kernel vanilla-geometry-kernel)
                             native-curve tolerance degree parameterization maintain-end-tangents? container
                             rational? &key control-points-only?)
  (declare (ignore native-curve tolerance degree parameterization maintain-end-tangents? container rational? control-points-only?))
  (error "Don't know how to convert nurbs to beziers using ~s.~%" geometry-kernel))

(defmethod make-arc-curve ((geometry-kernel vanilla-geometry-kernel)
                           center orientation radius start-angle end-angle)
  (declare (ignore center orientation radius start-angle end-angle))
  (error "Don't know how to make an arc curve using ~s.~%" geometry-kernel))

(defmethod make-elliptical-curve ((geometry-kernel vanilla-geometry-kernel)
                                  center orientation minor-axis-length major-axis-length start-angle end-angle)
  (declare (ignore center orientation minor-axis-length major-axis-length start-angle end-angle))
  (error "Don't know how to make an ellipse curve using ~s.~%" geometry-kernel))


(defmethod get-curve-control-points ((geometry-kernel vanilla-geometry-kernel) curve)
  (declare (ignore curve))
  (error "Don't know how to get control points from curve using ~s.~%" geometry-kernel))

(defmethod is-curve-rational ((geometry-kernel vanilla-geometry-kernel) curve)
  (declare (ignore curve))
  (error "Don't know how to check for rationality of curve using ~s.~%" geometry-kernel))



(defmethod get-curve-parameter-bounds ((geometry-kernel vanilla-geometry-kernel) curve)
  (declare (ignore curve))
  (error "Don't know how to get parameter bounds from curve using ~s.~%" geometry-kernel))

(defmethod curve-on-plane? ((geometry-kernel vanilla-geometry-kernel) curve plane-point plane-normal &key distance-tolerance)
  (declare (ignore curve plane-point plane-normal distance-tolerance))
  (error "Don't know how to detect curve on plane using ~s.~%" geometry-kernel))

(defmethod get-curve-dropped-point ((geometry-kernel vanilla-geometry-kernel) curve 3d-point &key distance-tolerance)
  (declare (ignore curve 3d-point distance-tolerance))
  (error "Don't know how to get dropped point from curve using ~s.~%" geometry-kernel))

(defmethod get-curve-minimum-radius ((geometry-kernel vanilla-geometry-kernel) curve)
  (declare (ignore curve))
  (error "Don't know how to get minimum radius of curve using ~s.~%" geometry-kernel))

(defmethod get-offset-point-along ((geometry-kernel vanilla-geometry-kernel) curve parameter distance)
  (declare (ignore curve parameter distance))
  (error "Don't know how to offset point along curve using ~s.~%" geometry-kernel))

(defmethod get-surface-curve-intersection-points ((geometry-kernel vanilla-geometry-kernel) surface curve &key 3d-tolerance)
  (declare (ignore surface curve 3d-tolerance))
  (error "Don't know how to do surface-curve intersection using ~s.~%" geometry-kernel))

(defmethod get-curve-plane-intersection-point ((geometry-kernel vanilla-geometry-kernel) curve plane-point plane-normal)
  (declare (ignore curve plane-point plane-normal))
  (error "don't know how to get plane intersection point for curve using ~s.~%" geometry-kernel))

(defmethod get-curve-curve-intersection-point ((geometry-kernel vanilla-geometry-kernel) curve other-curve &key distance-tolerance)
  (declare (ignore curve other-curve distance-tolerance))
  (error "don't know how to get curve intersection point for curve using ~s.~%" geometry-kernel))


(defmethod global-curve-solve ((geometry-kernel vanilla-geometry-kernel) curve &key other-curve operation-type)
  (declare (ignore curve other-curve operation-type))
  (error "don't know how to do global curve solution using ~s.~%" geometry-kernel))

(defmethod global-surface-curve-solve ((geometry-kernel vanilla-geometry-kernel) surface &key curve operation-type)
  (declare (ignore surface curve operation-type))
  (error "don't know how to do global surface curve solution using ~s.~%" geometry-kernel))


(defmethod global-surface-surface-solve ((geometry-kernel vanilla-geometry-kernel) surface &key other-surface operation-type)
  (declare (ignore surface other-surface operation-type))
  (error "don't know how to do global surface surface solution using ~s.~%" geometry-kernel))

(defmethod surface-evaluate-geometric ((geometry-kernel vanilla-geometry-kernel) surface u v)
  (declare (ignore surface u v))
  (error "don't know how to do geometric evaluation of surface point using ~s.~%" geometry-kernel))

(defmethod surface-global-line-intersect ((geometry-kernel vanilla-geometry-kernel) surface point vector &key distance-tolerance)
  (declare (ignore surface point vector distance-tolerance))
  (error "don't know how to do surface global line intersect using ~s.~%" geometry-kernel))

(defmethod surface-get-domain-components ((geometry-kernel vanilla-geometry-kernel) surface)
  (declare (ignore surface))
  (error "don't know how to get surface domain components using ~s.~%" geometry-kernel))

(defmethod global-curve-point-solve ((geometry-kernel vanilla-geometry-kernel) curve &key point operation-type)
  (declare (ignore curve point operation-type))
  (error "don't know how to do global curve-point solution using ~s.~%" geometry-kernel))

(defmethod make-b-spline-curve ((geometry-kernel vanilla-geometry-kernel) control-points
                                weights degree knot-vector)
  (declare (ignore control-points weights degree knot-vector))
  (error "Don't know how to make b-spline-curve using ~s.~%" geometry-kernel))


(define-vanilla-wrapper make-offset-curve (&key curve plane-normal offset-distance tolerance))


(defmethod get-curve-b-spline-data ((geometry-kernel vanilla-geometry-kernel) curve)
  (declare (ignore curve))
  (error "Don't know how to get curve b-spline-data using ~s.~%" geometry-kernel))

(defmethod get-surface-b-spline-data ((geometry-kernel vanilla-geometry-kernel) surface)
  (declare (ignore surface))
  (error "Don't know how to get surface b-spline-data using ~s.~%" geometry-kernel))

(defmethod get-point-at-arc-length ((geometry-kernel vanilla-geometry-kernel) curve percentage &key tolerance)
  (declare (ignore curve percentage tolerance))
  (error "Don't know how to get point at arc-length using ~s.~%" geometry-kernel))

(defmethod %get-point-on-curve ((geometry-kernel vanilla-geometry-kernel) curve parameter)
  (declare (ignore curve parameter))
  (error "Don't know how to get point on curve using ~s.~%" geometry-kernel))

(defmethod get-curve-tangent ((geometry-kernel vanilla-geometry-kernel) curve parameter)
    (declare (ignore curve parameter))
    (error "Don't know how to get curve tangent using ~s.~%" geometry-kernel))

(defmethod get-curve-curvature ((geometry-kernel vanilla-geometry-kernel) curve parameter)
    (declare (ignore curve parameter))
    (error "Don't know how to get curve curvature info using ~s.~%" geometry-kernel))

(defmethod get-curve-total-length ((geometry-kernel vanilla-geometry-kernel) curve u1 u2 tolerance tolerance-type)
  (declare (ignore curve u1 u2 tolerance tolerance-type))
  (error "Don't know how to get curve length using ~s.~%" geometry-kernel))

(defmethod get-curve-first-derivative ((geometry-kernel vanilla-geometry-kernel) curve)
  (declare (ignore curve))
  (error "Don't know how to get first derivative of curve using ~s.~%" geometry-kernel))

(defmethod get-curve-second-derivative ((geometry-kernel vanilla-geometry-kernel) curve)
  (declare (ignore curve))
  (error "Don't know how to get second derivative of curve using ~s.~%" geometry-kernel))

(defmethod get-surface-control-points ((geometry-kernel vanilla-geometry-kernel) surface)
  (declare (ignore surface))
  (error "Don't know how to get control points from surface using ~s.~%" geometry-kernel))

(defmethod make-b-spline-surface ((geometry-kernel vanilla-geometry-kernel) control-points
                                  weights u-degree u-knot-vector v-degree v-knot-vector)
  (declare (ignore control-points weights u-degree u-knot-vector v-degree v-knot-vector))
  (error "Don't know how to make b-spline-surface using ~s.~%" geometry-kernel))

(defmethod get-iso-curves ((geometry-kernel vanilla-geometry-kernel) surface direction number)
  (declare (ignore surface direction number))
  (error "Don't know how to get iso curves using ~s.~%" geometry-kernel))

(defmethod %get-point-on-surface ((geometry-kernel vanilla-geometry-kernel) surface u v)
  (declare (ignore surface u v))
  (error "Don't know how to get point on surface using ~s.~%" geometry-kernel))

(defmethod get-surface-normal ((geometry-kernel vanilla-geometry-kernel) surface u v)
    (declare (ignore surface u v))
    (error "Don't know how to get surface normal using ~s.~%" geometry-kernel))

(defmethod interpolate-curve ((geometry-kernel vanilla-geometry-kernel) points degree parameterization 
                              &key vectors vector-type interpolant? tolerance)
  (declare (ignore points degree parameterization vectors vector-type interpolant? tolerance))
  (error "Dont know how to interpolate curve using ~s.~%" geometry-kernel))

(defmethod interpolate-surface ((geometry-kernel vanilla-geometry-kernel) points u-degree v-degree parameterization)
  (declare (ignore points u-degree v-degree parameterization))
  (error "Dont know how to interpolate surface using ~s.~%" geometry-kernel))

(defmethod make-linear-curve ((geometry-kernel vanilla-geometry-kernel) start end)
  (declare (ignore start end))
  (error "Don't know how to make a linear curve using ~s.~%" geometry-kernel))

(defmethod make-spherical-surface ((geometry-kernel vanilla-geometry-kernel) center radius start-sweep end-sweep profile-angle profile-degree revolution-degree)
  (declare (ignore center radius start-sweep end-sweep profile-angle profile-degree revolution-degree))
  (error "Don't know how to make a spherical surface using ~s.~%" geometry-kernel))

(defmethod make-revolved-surface ((geometry-kernel vanilla-geometry-kernel) &key curve axis-point axis-vector arc flag)
  (declare (ignore curve axis-point axis-vector arc flag))
  (error "Don't know how to make a revolved surface using ~s.~%" geometry-kernel))


(defmethod make-planar-surface ((geometry-kernel vanilla-geometry-kernel) p00 p01 p10 p11)
  (declare (ignore  p00 p01 p10 p11))
  (error "Don't know how to make a planar surface using ~s.~%" geometry-kernel))

;; HWLibs methods

(defmethod write-iges-file ((geometry-kernel vanilla-geometry-kernel) pathname &key points curves surfaces tolerance)
  (declare (ignore pathname points curves surfaces tolerance))
  (error "Don't know how to write iges file using ~s.~%" geometry-kernel))

(defmethod write-iges-file* ((geometry-kernel vanilla-geometry-kernel) pathname 
                             &key points curves surfaces trimmed-surfaces solids quiet? 
                                  units units-scale tolerance write-analytic-curves?)
  (declare (ignore pathname points curves surfaces trimmed-surfaces solids quiet? units units-scale tolerance write-analytic-curves?))
  (error "Don't know how to write iges file using ~s.~%" geometry-kernel))

(defmethod read-iges-file ((geometry-kernel vanilla-geometry-kernel) pathname)
  (declare (ignore pathname))
  (error "Don't know how to read iges file using ~s.~%" geometry-kernel))

(defmethod read-iges-file* ((geometry-kernel vanilla-geometry-kernel) 
                            pathname &key quiet? finalize-on)
  (declare (ignore pathname quiet? finalize-on))
  (error "Don't know how to read iges file using ~s.~%" geometry-kernel))


(defmethod write-step-file* 
    ((geometry-kernel vanilla-geometry-kernel) pathname
     &key points curves surfaces trimmed-surfaces solids quiet?
          units units-scale tolerance write-analytic-curves?)
  (declare (ignore pathname points curves surfaces trimmed-surfaces 
                   solids quiet? units units-scale tolerance 
                   write-analytic-curves?))
  (error "Don't know how to write STEP file using ~s.~%" 
         geometry-kernel))


(defmethod read-step-file* ((geometry-kernel vanilla-geometry-kernel) pathname &key quiet? finalize-on 
                                                                                    make-all-surfaces-trimmed?
                                                                                    break-up-breps-into-trimmed-surfaces?
                                                                                    group-trimmed-surfaces-into-brep?
                                                                                    make-single-brep?)
  (declare (ignore pathname quiet? finalize-on make-all-surfaces-trimmed? break-up-breps-into-trimmed-surfaces? group-trimmed-surfaces-into-brep? make-single-brep?))
  (error "Don't know how to read STEP file using ~s.~%" geometry-kernel))

;;SMLib methods

(defmethod write-native-file ((geometry-kernel vanilla-geometry-kernel) file-name &key curves surfaces breps file-type quiet?)
  (declare (ignore file-name curves surfaces breps file-type quiet?))
  (error "Don't know how to write native file using ~s.~%" geometry-kernel))

(defmethod read-native-file ((geometry-kernel vanilla-geometry-kernel) file-name &key curves surfaces trees breps file-type quiet?)
  (declare (ignore file-name curves surfaces trees breps file-type quiet?))
  (error "Don't know how to read native file using ~s.~%" geometry-kernel))


(defmethod poly-brep-write-to-stl-file ((geometry-kernel vanilla-geometry-kernel) poly-brep file-name format)
  (declare (ignore poly-brep file-name format))
  (error "Don't know how to write STL file using ~s.~%" geometry-kernel))


(defmethod make-poly-brep ((geometry-kernel vanilla-geometry-kernel) brep 
                           &key dCHTol dCrvTessAngle dSrfTessAngle dMax3DEdge
                                dMaxAspect bSmoothResults)
  (declare (ignore brep dCHTol dCrvTessAngle dSrfTessAngle dMax3DEdge dMaxAspect bSmoothResults))
  (error "Don't know how to make poly-brep using ~s.~%" geometry-kernel))


(defmethod make-brep ((geometry-kernel vanilla-geometry-kernel) &key tolerance)
  (declare (ignore tolerance))
  (error "Don't know how to make brep using ~s.~%" geometry-kernel))
           
(defmethod make-b-spline-curve*-2d ((geometry-kernel vanilla-geometry-kernel) b-spline-curve*)
  (declare (ignore b-spline-curve*))
  (error "Don't know how to make b-spline-curve* using ~s.~%" geometry-kernel))
           
(defmethod make-b-spline-surface* ((geometry-kernel vanilla-geometry-kernel) b-spline-surface &key (schedule-finalization? t))
  (declare (ignore b-spline-surface schedule-finalization?))
  (error "Don't know how to make b-spline-surface* using ~s.~%" geometry-kernel))
           
(defmethod make-b-spline-curve* ((geometry-kernel vanilla-geometry-kernel) b-spline-curve &key (schedule-finalization? t))
  (declare (ignore b-spline-curve schedule-finalization?))
  (error "Don't know how to make b-spline-curve* using ~s.~%" geometry-kernel))
           
(defmethod make-trimmed-face ((geometry-kernel vanilla-geometry-kernel) brep loops-3d loops-uv orientations loop-points basis* basis-orientation)
  (declare (ignore brep loops-3d loops-uv orientations loop-points basis* basis-orientation))
  (error "Don't know how to make trimmed face using ~s.~%" geometry-kernel))
           
(defmethod make-trimmed-iso-curves ((geometry-kernel vanilla-geometry-kernel) face tolerance curves-3d curves-uv)
  (declare (ignore face tolerance curves-3d curves-uv))
  (error "Don't know how to make trimmed isos using ~s.~%" geometry-kernel))
           
(defmethod make-face-boundary-curves ((geometry-kernel vanilla-geometry-kernel) face)
  (declare (ignore face))
  (error "Don't know how to make face boundary using ~s.~%" geometry-kernel))
           
(defmethod get-b-spline-curve*-data ((geometry-kernel vanilla-geometry-kernel) b-spline-curve*)
  (declare (ignore b-spline-curve*))
  (error "Don't know how to get curve* data using ~s.~%" geometry-kernel))
           
(defmethod drop-curve ((geometry-kernel vanilla-geometry-kernel) curve-in surface)
  (declare (ignore curve-in surface))
  (error "Don't know how to drop curve using ~s.~%" geometry-kernel))

(define-vanilla-wrapper project-curve (curve-in surface projection-vector approximation-tolerance angle-tolerance))

(defmethod curve-convert-uv-to-3d ((geometry-kernel vanilla-geometry-kernel) curve-uv surface)
  (declare (ignore curve-uv surface))
  (error "Don't know how to convert uv-curve to 3d-curve using ~s.~%" geometry-kernel))
           
(defmethod face-compute-precise-properties ((geometry-kernel vanilla-geometry-kernel) face desired-accuracy origin estimated-area face-thickness)
  (declare (ignore face desired-accuracy origin estimated-area face-thickness))
  (error "Don't know how to compute precise properties using ~s.~%" geometry-kernel))

(defmethod surface-area ((geometry-kernel vanilla-geometry-kernel) surface desired-accuracy)
  (declare (ignore surface desired-accuracy))
  (error "Don't know how to compute surface area using ~s.~%" geometry-kernel))

(defmethod get-faces-from-brep ((geometry-kernel vanilla-geometry-kernel) brep)
  (declare (ignore brep))
  (error "Don't know how to get faces from brep using ~s.~%" geometry-kernel))

(defmethod get-edges-from-brep ((geometry-kernel vanilla-geometry-kernel) brep)
  (declare (ignore brep))
  (error "Don't know how to get edges from brep using ~s.~%" geometry-kernel))

(defmethod get-bspline-curve-from-edge ((geometry-kernel vanilla-geometry-kernel) edge)
  (declare (ignore edge))
  (error "Don't know how to get bspline-curve from edge using ~s.~%" geometry-kernel))

(defmethod get-uv-curve-from-edge ((geometry-kernel vanilla-geometry-kernel) edge surface)
  (declare (ignore edge surface))
  (error "Don't know how to get bspline-curve from edge using ~s.~%" geometry-kernel))

(defmethod get-3d-point-from-vertex ((geometry-kernel vanilla-geometry-kernel) vertex)
  (declare (ignore vertex))
  (error "Don't know how to get 3d-point from vertex using ~s.~%" geometry-kernel))
  
(defmethod get-regions-from-brep ((geometry-kernel vanilla-geometry-kernel) brep)
  (declare (ignore brep))
  (error "Don't know how to get regions from brep using ~s.~%" geometry-kernel))

(defmethod get-shells-from-brep ((geometry-kernel vanilla-geometry-kernel) brep)
  (declare (ignore brep))
  (error "Don't know how to get shells from brep using ~s.~%" geometry-kernel))

(defmethod get-shells-from-region ((geometry-kernel vanilla-geometry-kernel) region)
  (declare (ignore region))
  (error "Don't know how to get shells from region using ~s.~%" geometry-kernel))

(defmethod get-vertices-from-brep ((geometry-kernel vanilla-geometry-kernel) brep)
  (declare (ignore brep))
  (error "Don't know how to get vertices from brep using ~s.~%" geometry-kernel))

(defmethod get-surface-from-face ((geometry-kernel vanilla-geometry-kernel) face)
  (declare (ignore face))
  (error "Don't know how to get surface from face using ~s.~%" geometry-kernel))

(defmethod get-edges-from-face ((geometry-kernel vanilla-geometry-kernel) face)
  (declare (ignore face))
  (error "Don't know how to get edges from face using ~s.~%" geometry-kernel))

(defmethod get-nurbs-from-surface ((geometry-kernel vanilla-geometry-kernel) surface)
  (declare (ignore surface))
  (error "Don't know how to get nurbs from surface using ~s.~%" geometry-kernel))

(defmethod get-nurbs-from-curve ((geometry-kernel vanilla-geometry-kernel) curve)
  (declare (ignore curve))
  (error "Don't know how to get nurbs from curve using ~s.~%" geometry-kernel))

(defmethod extract-face-hole-curves ((geometry-kernel vanilla-geometry-kernel) face)
  (declare (ignore face))
  (error "Don't know how to extract face hole curves using ~s.~%" geometry-kernel))

(defmethod extract-face-island-curves ((geometry-kernel vanilla-geometry-kernel) face)
  (declare (ignore face))
  (error "Don't know how to extract island hole curves using ~s.~%" geometry-kernel))

(defmethod u-min ((geometry-kernel vanilla-geometry-kernel) iw-surface)
  (declare (ignore iw-surface))
  (error "Don't know how to get u-min from surface using ~s.~%" geometry-kernel))

(defmethod u-max ((geometry-kernel vanilla-geometry-kernel) iw-surface)
  (declare (ignore iw-surface))
  (error "Don't know how to get u-max from surface using ~s.~%" geometry-kernel))

(defmethod v-min ((geometry-kernel vanilla-geometry-kernel) iw-surface)
  (declare (ignore iw-surface))
  (error "Don't know how to get v-min from surface using ~s.~%" geometry-kernel))

(defmethod v-max ((geometry-kernel vanilla-geometry-kernel) iw-surface)
  (declare (ignore iw-surface))
  (error "Don't know how to get v-max from surface using ~s.~%" geometry-kernel))

(defmethod set-long ((geometry-kernel vanilla-geometry-kernel) object value)
  (declare (ignore object value))
  (error "Don't know how to set long attribute on object using ~s.~%" geometry-kernel))

(defmethod get-long ((geometry-kernel vanilla-geometry-kernel) object)
  (declare (ignore object))
  (error "Don't know how to get long attribute from object using ~s.~%" geometry-kernel))

(defmethod get-levels ((geometry-kernel vanilla-geometry-kernel) object)
  (declare (ignore object))
  (error "Don't know how to get levels from object using ~s.~%" geometry-kernel))

(defmethod get-color ((geometry-kernel vanilla-geometry-kernel) object)
  (declare (ignore object))
  (error "Don't know how to get color from object using ~s.~%" geometry-kernel))

(defmethod add-box-primitive ((geometry-kernel vanilla-geometry-kernel) brep length width height x y z xx xy xz yx yy yz)
  (declare (ignore  brep length width height x y z xx xy xz yx yy yz))
  (error "Don't know how to add box primitive using ~s.~%" geometry-kernel))

(defmethod add-cone-primitive ((geometry-kernel vanilla-geometry-kernel) brep height base-radius top-radius start-angle end-angle x y z xx xy xz yx yy yz)
  (declare (ignore  brep height base-radius top-radius start-angle end-angle x y z xx xy xz yx yy yz))
  (error "Don't know how to add cone primitive using ~s.~%" geometry-kernel))

(defmethod add-linear-sweep-primitive ((geometry-kernel vanilla-geometry-kernel) brep curve-list axis-vector distance)
  (declare (ignore  brep curve-list axis-vector distance))
  (error "Don't know how to add linear-sweep primitive using ~s.~%" geometry-kernel))


(defmethod build-composites-from-curves ((geometry-kernel vanilla-geometry-kernel) 
                                         curves &key (same-point-tolerance 0.01) (distance-to-create-line 0.1))
  (declare (ignore curves same-point-tolerance distance-to-create-line))
  (error "Don't know how to build composites from curves using ~s.~%" geometry-kernel))


(defmethod build-composite-curve ((geometry-kernel vanilla-geometry-kernel) curves)
  (declare (ignore curves))
  (error "Don't know how to build composite curve using ~s.~%" geometry-kernel))


(defmethod intersect-surface-with-plane ((geometry-kernel vanilla-geometry-kernel) surface plane-normal plane-point &key 3d-approximation-tolerance
                                                                                                                         angle-tolerance-radians)
  (declare (ignore surface plane-normal plane-point 3d-approximation-tolerance angle-tolerance-radians))
  (error "Don't know how to intersect surface and plane using ~s.~%" geometry-kernel))

(defmethod iw-copy-curve ((geometry-kernel vanilla-geometry-kernel) iw-curve &key finalize?)
  (declare (ignore iw-curve finalize?))
  (error "Don't know how to copy iw-curve using ~s.~%." geometry-kernel))

(defmethod iw-trim-curve ((geometry-kernel vanilla-geometry-kernel) iw-curve u1 u2)
  (declare (ignore iw-curve u1 u2))
  (error "Don't know how to trim iw-curve using ~s.~%." geometry-kernel))

(defmethod curve-reverse ((geometry-kernel vanilla-geometry-kernel) curve)
  (declare (ignore curve))
  (error "Don't know how to make reversed curve using ~s~%." geometry-kernel))

(defmethod bounding-box ((geometry-kernel vanilla-geometry-kernel) surface)
  (declare (ignore surface))
  (error "Don't know how to get bounding box for surface using ~s~%." geometry-kernel))

(defmethod surface-drop-point ((geometry-kernel vanilla-geometry-kernel) surface poing &key 3d-tolerance)
  (declare (ignore surface poing 3d-tolerance))
  (error "Don't know how to drop point on surface using ~s.~%" geometry-kernel))


(defmethod get-triangles ((geometry-kernel vanilla-geometry-kernel) 
                          brep  &key minimum-number-of-segments maximum-3d-distance-between-points
                                     minimum-parametric-ratio chord-height
                                     angle-tolerance-degrees max-edge-length
                                     min-edge-length min-edge-length-ratio-uv max-aspect-ratio)
  (declare (ignore brep minimum-number-of-segments maximum-3d-distance-between-points minimum-parametric-ratio 
                   chord-height angle-tolerance-degrees max-edge-length min-edge-length min-edge-length-ratio-uv max-aspect-ratio))
  (error "Don't know how to get triangles from brep ~s.~%" geometry-kernel))


(defmethod make-merge-container ((geometry-kernel vanilla-geometry-kernel) brep other-brep approximation-tolerance angle-tolerance)
  (declare (ignore brep other-brep approximation-tolerance angle-tolerance))
  (error "Don't know how to make merge container using ~s.~%" geometry-kernel))


(defmethod do-boolean-merge-operation ((geometry-kernel vanilla-geometry-kernel) 
                                       merge-container operation manifold? 
                                       &key make-manifold?)
  (declare (ignore merge-container operation manifold? make-manifold?))
  (error "Don't know how to do boolean merge operation with ~s.~%" geometry-kernel))

(defmethod do-boolean-separate-operation ((geometry-kernel vanilla-geometry-kernel) merge-container manifold? sew-and-orient?)
  (declare (ignore merge-container manifold? sew-and-orient?))
  (error "Don't know how to do boolean separate operation with ~s.~%" geometry-kernel))


(defmethod make-linear-swept-brep ((geometry-kernel vanilla-geometry-kernel) facial-brep vector distance)
  (declare (ignore facial-brep vector distance))
  (error "Don't know how to do linear swept brep with ~s.~%" geometry-kernel))

(defmethod in? ((geometry-kernel vanilla-geometry-kernel) brep point)
  (declare (ignore brep point))
  (error "Don't know how to check for point in brep using ~s.~%" geometry-kernel))


(defmethod loft-surface ((geometry-kernel vanilla-geometry-kernel) curves &key stacks)
  (declare (ignore curves stacks))
  (error "Don't know how to do lofted surface with ~s.~%" geometry-kernel))





(defmethod make-faces-from-surface ((geometry-kernel vanilla-geometry-kernel) brep surface &key continuity-type end-caps?)
  (declare (ignore brep surface continuity-type end-caps?))
  (error "Don't know how to make faces from surface  with ~s.~%" geometry-kernel))

(defmethod make-edge-from-curve ((geometry-kernel vanilla-geometry-kernel) brep curve)
  (declare (ignore brep curve))
  (error "Don't know how to make edge from curve  with ~s.~%" geometry-kernel))


(defmethod brep-get-wires ((geometry-kernel vanilla-geometry-kernel) brep)
  (declare (ignore brep))
  (error "Don't know how to get wires from brep  with ~s.~%" geometry-kernel))

(defmethod brep-get-edges ((geometry-kernel vanilla-geometry-kernel) brep)
  (declare (ignore brep))
  (error "Don't know how to get edges from brep  with ~s.~%" geometry-kernel))

(defmethod brep-copy ((geometry-kernel vanilla-geometry-kernel) brep)
  (declare (ignore brep))
  (error "Don't know how to copy a brep  with ~s.~%" geometry-kernel))

(defmethod brep-get-curves ((geometry-kernel vanilla-geometry-kernel) brep)
  (declare (ignore brep))
  (error "Don't know how to get curves from brep  with ~s.~%" geometry-kernel))

(defmethod brep-get-my-curves ((geometry-kernel vanilla-geometry-kernel) brep)
  (declare (ignore brep))
  (error "Don't know how to get curves from brep  with ~s.~%" geometry-kernel))

(defmethod brep-compute-precise-properties ((geometry-kernel vanilla-geometry-kernel) brep tolerance)
  (declare (ignore brep tolerance))
  (error "Don't know how to compute precise properties from brep  with ~s.~%" geometry-kernel))

(define-vanilla-wrapper brep-compute-properties (brep &key edge-tess-tolerance face-tess-tolerance))

(defmethod make-blended-brep ((geometry-kernel vanilla-geometry-kernel) brep &key default-radius specs)
  (declare (ignore  brep default-radius specs))
  (error "Don't know how to make blended brep with ~s.~%" geometry-kernel))

(defmethod make-stitched-solid-brep ((geometry-kernel vanilla-geometry-kernel) &key surfaces proper-faces)
  (declare (ignore surfaces proper-faces))
  (error "Don't know how to make stitched solid with ~s.~%" geometry-kernel))


(defmethod make-transformed-brep ((geometry-kernel vanilla-geometry-kernel) brep &key translation x-vector y-vector scale-vector)
  (declare (ignore brep translation x-vector y-vector scale-vector))
  (error "Don't know how to make transformed-brep with ~s.~%" geometry-kernel))

(defmethod make-ruled-surface ((geometry-kernel vanilla-geometry-kernel) &key curve-1 curve-2 (direction :u))
  (declare (ignore curve-1 curve-2 direction)))


(defmethod iwbrep-sew-faces ((geometry-kernel vanilla-geometry-kernel) brep &key (tolerance 0.01))
  (declare (ignore brep tolerance))
  (error "Don't know how to sew faces using ~s.~%" geometry-kernel))

(defmethod tag-edges ((geometry-kernel vanilla-geometry-kernel) brep brep-id)
  (declare (ignore brep brep-id))
  (error "Don't know how to tag edges using ~s.~%" geometry-kernel))





(define-vanilla-wrapper brep-assert-valid (native-brep &key warn?))

(define-vanilla-wrapper approximate-surface (points normals &key u-start v-start u-required v-required tolerance))
(define-vanilla-wrapper loft-surface* (curves &key rail-1 rail-2 v-degree tolerance))

(define-vanilla-wrapper interpolate-c11-surface (points  &key tangent-method))
(define-vanilla-wrapper return-compatible-surfaces (surfaces &key length))
(define-vanilla-wrapper return-compatible-curves (curves &key length))

(define-vanilla-wrapper join-surfaces (surface1 surface2 &key tolerance direction))

(define-vanilla-wrapper brep-intersect (brep other-brep &key tolerance angle-tolerance))
(define-vanilla-wrapper brep-intersect? (brep other-brep &key tolerance angle-tolerance))

(define-vanilla-wrapper is-surface-rational (surface))

(define-vanilla-wrapper max-second-deriv (curve))

(define-vanilla-wrapper curve-extent (curve))

(define-vanilla-wrapper reduce-curve-degree (native-curve tolerance))

(define-vanilla-wrapper gprcap (curve &key tolerance))

(define-vanilla-wrapper poly-brep-get-brep-mesh (native-poly-brep))

(define-vanilla-wrapper brep-calculate-tight-bounding-box (brep))
  
(define-vanilla-wrapper get-filled-regions (brep))

(define-vanilla-wrapper make-brep-from-regions (brep-in regions))

(define-vanilla-wrapper brep-validate-and-update-tolerances (native-brep))

(define-vanilla-wrapper brep-planar-section-curves (native-brep
                                                  plane-point
                                                  plane-normal
                                                  &key tolerance angle-tolerance-radians))

(define-vanilla-wrapper make-offset-surface (&key
                                           surface offset-distance u-degree v-degree
                                           parameterization tolerance))

(define-vanilla-wrapper make-offset-brep (brep &key distance tolerance))

(define-vanilla-wrapper make-shelled-brep (brep &key distance tolerance))



(define-vanilla-wrapper read-brep-from-file (file-name &key finalize-on))

(define-vanilla-wrapper make-brep-manifold (brep &key finalize-on regions-to-keep keep-internal-faces?))

(define-vanilla-wrapper brep-get-tolerance (brep))
(define-vanilla-wrapper brep-reset-tolerance (brep))

(define-vanilla-wrapper copy-face-to-new-brep (brep face))


(define-vanilla-wrapper make-extended-surface (&key surface curve direction which-end continuity))

(define-vanilla-wrapper make-approximated-curve (&key curve tolerance pinned-parameters))

(define-vanilla-wrapper split-surface-at-param (&key native-surface u-or-v parameter))

(define-vanilla-wrapper skin-with-spine (&key profile-curves  
                                            synchronized?
                                            spine-curve
                                            spine-parameters
                                            align-profiles?
                                            spine-z-axis
                                            subdivision-level
                                            derivative-selection
                                            skinning-degree
                                            u-or-v))
(define-vanilla-wrapper interpolate-conic (&key 
                                         points 
                                         parameterization
                                         tangency
                                         smooth-corners?))

(define-vanilla-wrapper make-coons-patch (&key curve-left
                                             curve-right
                                             curve-bottom
                                             curve-top
                                             derivative-left
                                             derivative-right
                                             derivative-bottom
                                             derivative-top))

(define-vanilla-wrapper make-approximated-subsurface (&key surface
                                                         curve-top
                                                         curve-bottom
                                                         curve-left
                                                         curve-right
                                                         curve-top-uv
                                                         curve-bottom-uv
                                                         curve-left-uv
                                                         curve-right-uv))

(define-vanilla-wrapper curve-subdivide-at-discontinuities (&key curve
                                                               continuity-type
                                                               continuity-angle))


(define-vanilla-wrapper merge-brep (&key brep 
                                       brep-to-merge 
                                       copy-brep-map))


(define-vanilla-wrapper merge-breps (&key breps operation))

(define-vanilla-wrapper sew-brep (&key brep tolerances))

(define-vanilla-wrapper surface-create-silhouette-curves (&key surface eye-point-or-vector 
                                                             perspective? tolerance angle-tolerance))

(define-vanilla-wrapper tooccl (curve &key tolerance))
(define-vanilla-wrapper  curve-knot-scale (curve &key u-min u-max)) 
(define-vanilla-wrapper  surf-grid-points (surface &key  u-parameters  v-parameters u-direction  v-direction))

(define-vanilla-wrapper closed-boolean-operation (&key first-brep
                                                     rest-breps
                                                     operation
                                                     approximation-tolerance
                                                     angle-tolerance
                                                     sew-and-orient?
                                                     manifold?))

(define-vanilla-wrapper closed-boolean-separate-operation (&key first-brep
                                                              other-brep
                                                              approximation-tolerance
                                                              angle-tolerance
                                                              sew-and-orient?
                                                              manifold?))

(define-vanilla-wrapper global-brep-brep-solve (brep 
                                              &key other-brep operation-type distance-tolerance))


(define-vanilla-wrapper get-faces-from-edge (edge))


(define-vanilla-wrapper reduce-surface-knot (surface &key tolerance direction))

(define-vanilla-wrapper make-extend-curve (curve &key distance distance-type extending-from continuity)) 

(define-vanilla-wrapper approximated-compatible-curves (curve-list &key length tolerance))

(define-vanilla-wrapper make-tx-assembly-instance (&key name))

(define-vanilla-wrapper tx-assembly-add-subassembly (parent-assembly-instance-pointer
						   child-assembly-instance-pointer))

(define-vanilla-wrapper tx-assembly-add-brep (assembly-instance-pointer
					    native-brep
					    &key name red green blue))

(define-vanilla-wrapper tx-assembly-add-curve (assembly-instance-pointer
					    native-curve
					    &key name red green blue))

(define-vanilla-wrapper tx-assembly-add-point (assembly-instance-pointer
					     point
					     &key name red green blue))

(define-vanilla-wrapper rational-to-nonrational   (curve &key tolerance
						       maintain-end-tangents?
						       nonrational-degree
						       parameterization))

(define-vanilla-wrapper make-spiral (height
				   radius-1
				   radius-2
				   pitch
				   right-or-left
				   tolerance))

(define-vanilla-wrapper make-cardinal-spline (&key control-points tension-params periodic?))
  