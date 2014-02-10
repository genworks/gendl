;;;; -*- coding: utf-8 -*-

(asdf:defsystem
 #:geom-base
 :description
 "The Gendl™ Wireframe Geometry"
 :author
 "Genworks International"
 :license
 "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
 :serial
 t
 :version
 "20140208"
 :depends-on
 (:glisp)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:file "prereqs/source/presets")
  (:file "prereqs/source/cl-pdf-patches")
  (:file "prereqs/source/matrix")
  (:file "prereqs/source/parameters")
  (:file "prereqs/source/utilities")
  (:file "formats/source/2d-output")
  (:file "formats/source/dxf")
  (:file "formats/source/macro-redefs")
  (:file "formats/source/obj")
  (:file "formats/source/pdf-multipage")
  (:file "formats/source/pdf-raw")
  (:file "formats/source/pdf")
  (:file "formats/source/raster")
  (:file "formats/source/vector-graphics")
  (:file "wire/source/base-object")
  (:file "wire/source/arc")
  (:file "wire/source/l-line")
  (:file "wire/source/circle")
  (:file "wire/source/bezier-curve")
  (:file "wire/source/cylinder")
  (:file "wire/source/global-polyline")
  (:file "wire/source/global-filleted-polyline")
  (:file "wire/source/global-polygon-projection")
  (:file "wire/source/sphere")
  (:file "wire/source/arcoid-mixin")
  (:file "wire/source/bounding-box")
  (:file "wire/source/box")
  (:file "wire/source/c-cylinder")
  (:file "wire/source/cone")
  (:file "wire/source/directional-light")
  (:file "wire/source/ellipse")
  (:file "wire/source/fillet")
  (:file "wire/source/global-filleted-polygon-projection")
  (:file "wire/source/null-geometric-object")
  (:file "wire/source/outline-specialization-mixin")
  (:file "wire/source/point-light")
  (:file "wire/source/point")
  (:file "wire/source/points-display")
  (:file "wire/source/primitives")
  (:file "wire/source/route-pipe")
  (:file "wire/source/spherical-cap")
  (:file "wire/source/spot-light")
  (:file "wire/source/torus")
  (:file "wire/source/utilities")
  (:file "wire/source/views")
  (:file "annotations/source/angular-dimension")
  (:file "annotations/source/arrowhead")
  (:file "annotations/source/center-line")
  (:file "annotations/source/horizontal-dimension")
  (:file "annotations/source/label")
  (:file "annotations/source/leader-arc")
  (:file "annotations/source/leader-line")
  (:file "annotations/source/linear-dimension")
  (:file "annotations/source/parallel-dimension")
  (:file "annotations/source/sample-drawing")
  (:file "annotations/source/vertical-dimension")
  (:file "drawing/source/base-view")
  (:file "drawing/source/document")
  (:file "drawing/source/lenses")
  (:file "drawing/source/renderer-mixin")
  (:file "text/source/general-note")
  (:file "text/source/typeset-block")
  (:file "graphs/source/graph")
  (:file "graphs/source/legend")
  (:file "graphs/source/pie-chart")
  (:file "lenses/source/vrml")
  (:file "lenses/source/x3d")
  (:file "math/source/polynomial")
  (:file "zzinit/source/initialize")
  (:file "zzinit/source/zzinit")))
