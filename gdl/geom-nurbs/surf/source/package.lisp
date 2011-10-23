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

(in-package :common-lisp-user)


(defpackage :surf
  (:shadowing-import-from :gdl #:the)
  (:use :common-lisp :gdl :geom-base)
  (:documentation "GDL NURBS Surface and Solids Facility")
  (:shadow #:step)
  (:export #:make-geometry-kernel 
           #:*geometry-kernel* 
           #:*finalize-lofted-surfaces?*
           #:*chain-beziers-for-display?*
           #:curve 
           #:iso-curve
           #:trimmed-curve
           #:b-spline-curve 
           #:arc-curve
           #:elliptical-curve
           #:linear-curve
           #:fitted-curve
           #:planar-offset-curve
           #:filleted-curve
           #:projected-curve
           #:approximated-curve
           #:silhouette-curves
           #:dropped-curve
           #:surface 
           #:b-spline-surface
           #:extended-surface
           #:offset-surface
           #:offset-solid
           #:shelled-solid
           #:manifold-solid
           #:fitted-surface
           #:coons-surface
           #:surface-knot-reduction
           #:joined-surfaces
           #:compatible-surfaces
           #:compatible-curves
           #:extended-curve
           #:lofted-surface
           #:ruled-surface
           #:test-fitted-surface
           #:spherical-surface
           #:revolved-surface
           #:revolved-surfaces
           #:planar-surface
           #:rectangular-surface
           #:trimmed-surface
           #:approximated-subsurface
           #:basic-surface
           
           #:split-surface
           
           #:brep
           #:edge
           #:face
           #:iges-reader
           #:native-reader
           #:brep-reader
           #:step-reader
           #:composed-curve
           #:subdivided-curve
           #:composed-curves
           #:decomposed-curve
           #:decomposed-curves
           #:planar-section-curve
           #:planar-section-curves
           #:global-filleted-polyline-curves
           #:global-filleted-polyline-curve
           #:planar-contour-surface
           #:transformed-curve
           #:boxed-curve
           #:boxed-surface
           #:transformed-surface
           #:transformed-solid
           #:stitched-solid
           
           #:extruded-solid
           #:blended-solid
           
           #:get-3d-point-of
           #:get-parameter-of
           
           #:get-other-parameter-of
           #:get-uv-point-of
           #:get-point-on-surface
           #:get-point-on-curve
           #:get-point-on-other-curve
           
           #:%get-point-on-surface
           #:%get-point-on-curve
           
           
           #:iges #:native #:stl 
           
           
           #:step
           
           
           #:box-solid
           #:cone-solid
           #:cylinder-solid
           #:separated-solid
           #:subtracted-solid
           #:merged-solid
           #:validated-solid
           #:united-solid
           #:intersected-solid
           #:swept-solid
           #:brep-intersect
           #:brep-intersect?
           #:regioned-solid
           
           #:merged-brep
           
           #:general-sweep
           #:normal-sweep
           
           #:edge-blend-surface
           
           
           #:with-pinned-values
           #:pin-value-to-range
           
           #:fitted-conic
           
           #:*3d-approximation-tolerance-default*
           #:*boolean-operation-tolerance-default*
           #:*approximation-tolerance-factor*
           #:*brep-tolerance-default*
           #:*angle-tolerance-radians-default*
           #:*display-tolerance*
           #:*3d-tolerance-default*
           #:*brep-wireframe-tessellation?*
           #:*curve-tessellation?*
           #:*crease-angle-default*
           #:*output-units-default*
           #:*isos-default*
           #:*brep-isos-default*
           #:*brep-vrml-timeout*
           #:*nurbs-to-beziers-timeout*
           #:*boolean-allow-multiple-regions?*
           #:*boolean-error-on-invalid-brep?*
           
           #:iwbrep-sew-and-orient
           
           #:test-b-spline-surface
           #:normalized-curve
           #:surf-grid-points
           #:closed-boolean-operation
           #:closed-boolean-separate-operation
           #:global-brep-brep-solve
           
           #:get-faces-from-edge
           
           #:*tess-min-number-of-segments*
           #:*tess-max-3d-edge-factor*
           #:*tess-min-parametric-ratio*
           #:*tess-max-chord-height*
           #:*tess-max-angle-degrees*
           #:*tess-min-3d-edge*
           #:*tess-min-edge-ratio-uv*
           #:*tess-max-aspect-ratio*))

(pushnew (find-package :surf) gdl:*reserved-word-protected-packages*)

(defmacro gdl:define-package (name &rest body)
  `(defpackage ,name 
     (:use :common-lisp :gdl :geom-base :surf)
     (:shadowing-import-from :gdl #:the)
     (:shadowing-import-from :surf #:step)
     ,@body))


(gdl:define-package :gdl-user)
