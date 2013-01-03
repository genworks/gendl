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

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (gdl:define-package :geom-base
      (:documentation "GDL Base Geometry Module")
  
    (:export #:keyed-transform*vector
	     #:with-translated-state
	     #:raphael
	     #:*raphael-translation*
	     #:make-vector
	     #:make-point
	     #:apply-make-point
	     #:merge-display-controls
	     #:*nominal-x-vector*
	     #:*nominal-y-vector*
	     #:*nominal-z-vector*
	     #:*nominal-x-vector-r*
	     #:*nominal-y-vector-r*
	     #:*nominal-z-vector-r*
	     #:*trimetric-normal-vector*
	     #:*trimetric-normal-vector-left*
	     #:*trimetric-normal-vector-right-rear*
	     #:*trimetric-normal-vector-left-rear*
	     #:*left-handed-transform?*
	     #:+lh-identity-matrix+
	     #:+nominal-origin+
	     #:*standard-views*
	     #:point-expression   
	     #:+postnet-bits+   
	     #:*hash-transforms?*
	     #:get-x
	     #:get-y
	     #:get-z
	     #:get-w
	     #:get-u
	     #:get-v
	     #:determinant
	     #:subtract-vectors
	     #:add-vectors
	     #:3d-distance
	     #:scalar*vector
	     #:matrix*vector
	     #:transpose-matrix
	     #:multiply-matrices
	     #:dot-vectors
	     #:alignment
	     #:make-transform
	     #:angle-between-vectors-d
	     #:angle-between-vectors
	     #:unitize-vector
	     #:orthogonal-component
	     #:same-direction-vectors?
	     #:parallel-vectors?
	     #:reverse-vector
	     #:cross-vectors
	     #:length-vector
	     #:zero-vector?
	     #:degree
	     #:radians-to-degrees
	     #:radians-to-grads
	     #:translate-along-vector
	     #:array-to-list
	     #:coincident-point?
	     #:projected-vector
	     #:rotate-point-d
	     #:rotate-point
	     #:rotate-vector
	     #:rotate-vector-d
	     #:inter-circle-sphere
	     #:inter-line-sphere
	     #:inter-line-plane
	     #:translate
	     #:create-obliqueness
	     #:proj-point-on-line
	     #:pythagorize
	     #:roll
	     #:rotation
	     #:transform-and-translate-point
	     #:transform-numeric-point
	     #:quaternion-to-rotation
	     #:quaternion-to-matrix
	     #:matrix-to-quaternion
	     #:matrix-to-rotation
	     #:normalize-quaternion
	     #:degrees-to-radians
	     #:acosd
	     #:asind
	     #:atand
	     #:midpoint
	     #:between?
	     #:curve-parameter-<
	     #:roughly-aligned-vectors?
	     #:distance-to-line
	     #:equi-space-points
	     #:sort-points-along-vector
	     #:bounding-box-from-points
	     #:flatten-lines
	     #:flatten-curves

	     #:arc
	     #:base-object
	     #:base-coordinate-system
	     #:base-geometry-object
	     #:bezier-curve
	     #:box
	     #:bbox
	     #:c-cylinder
	     #:circle
	     #:cone
	     #:cylinder
	     #:ellipse
	     #:global-filleted-polygon-projection
	     #:global-filleted-polyline
	     #:global-polygon-projection
	     #:ifs-output-mixin
	     #:global-polyline
	     #:graph
	     #:l-line
	     #:null-geometric-object
	     #:outline-specialization-mixin
	     #:point
	     #:route-pipe
	     #:sphere
	     #:spherical-cap
	     #:torus
	     #:cut-cylinder    ;;do later
	     #:filleted-polyline ;; do later
	     #:line		 ;;do later
	     #:point 
	     #:polyline		;; do later
	     #:polygon-projection ;;do later
           
	     #:points-display
           
	     #:note
	     #:text-block
	     #:text-lines
	     #:typeset-block
	     #:typeset-blocks
	     #:base-drawing
	     #:base-view
	     #:document
	     #:horizontal-dimension
	     #:parallel-dimension
	     #:vertical-dimension
	     #:label
	     #:linear-dimension
	     #:leader-line
           
	     #:dxf
	     #:obj
	     #:pdf-multipage
	     #:pdf
	     #:pdf-raw
	     #:png
	     #:jpeg
	     #:vrml
	     #:x3d
	     #:vector-graphics
             
           
	     #:pie-chart
           
	     #:*gs-text-alpha-bits*
	     #:*gs-graphics-alpha-bits*

	     ;;
	     ;;
	     ;; FLAG -- consider exporting these if requested. 
	     ;;
	     ;;#:3d-vector-to-array
	     ;;#:array-to-3d-vector
           
	     )))


(pushnew (find-package :geom-base) gdl:*reserved-word-protected-packages*)

(defmacro gdl:define-package (name &rest body)
  `(defpackage ,name 
     (:shadowing-import-from :gdl #:the)
     (:use :common-lisp :gdl :geom-base)
     ,@body))


(gdl:define-package :gdl-user)

