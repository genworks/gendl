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

(defvar *geometry-kernel* nil)

(defparameter *chain-beziers-for-display?* nil
  "Boolean. Indicates whether display bezier curves within a single 
nurbs curve should be chained. Might be necessary for filled rendering. 
Legacy behavior was t. Now defaults to nil."
  )

(defparameter *display-tolerance* 0.01 
  "Number. Default tolerance used in approximating non-cubic or rational 
NURBS curves with cubic beziers for display with PDF and GWL web-based 
image displays (PNG or JPEG). Smaller values give better display fidelity 
for very small objects at the expense of speed, larger values sacrifice 
display fidelity for better speed. Default value is 0.01, which should give 
decent performance but might give distorted rendering for extremely small objects. 
This could be made to be somewhat adaptive, i.e. resetting itself on the fly 
to a smaller value when dealing with smaller curves. Please let us know if you 
think this would be useful.
<p>
Note that 3rd degree non-rational curves do not require approximation and are not
affected by this paramter.
</p>
")


(defparameter *crease-angle-default* 1.5
  "Number. The crease-angle for VRML indexed-face-set display. This is an angle in radians, 
which if the two polygon faces have more than this angle between them, the edge 
will not be smoothed.")

(defparameter *finalize-lofted-surfaces?* nil
  "Boolean. For debugging of memory leak. If you experience high C heap usage,
set this to t. But be sure to watch out for Signal 11 in this case."
  )

(defparameter *approximation-tolerance-factor* 5
  "Number. Factor multiplied by minimum brep adaptive-tolerance to compute tolerance for boolean operations.")


(defparameter *3d-approximation-tolerance-default* 0.001
  "Number. Tolerance used when approximating in e.g. Newton-Raphson iterations.")


(defparameter *boolean-operation-tolerance-default* nil
  "Number or nil. Tolerance used when doing boolean operations. Defaults to nil, which
means that boolean operations will use the approximation-tolerance-adaptive computed
by each brep. Defaults to nil. Note: this used to default to 0.001 (same as *3d-approximation-tolerance-default*).")


(defparameter *angle-tolerance-radians-default* 0.5729577951308232
  "Number. Angular tolerance (in radians) used when approximating in e.g. Newton-Raphson iterations.")


(defparameter *3d-tolerance-default* 0.00000001 
  "Number. Tolerance used for intersections etc. Defaults 1.0e-8.")

(defparameter *brep-tolerance-default* nil
  "Number. Tolerance used for creating breps. Defaults to nil. Note: this used to default to 1.0e-08")


(defparameter *output-units-default* :inches "Keyword symbol, one of :inches, :millimeters, :feet, :miles, :meters, :kilometers,
     :mils, :microns, :centimeters, :microinches, or :no-units-specified. This determines the units when outputting geometry.
     This value can be overridden with the :units keyword to the with-writer (for IGES, and soon STEP and other formats). Default
     value is :inches.")


(defparameter *isos-default* (list :n-u 11 :n-v 11)
  "Plist with keys :n-u and :n-v. Specifies the default number of isoparametric curves to display for surfaces, if not present in display-controls")

(defparameter *brep-isos-default* nil ;;(list :n-u 8 :n-v 8)
  "Plist with keys :n-u and :n-v, nil, or keyword :tess. 
Specifies the default number of isoparametric curves to display for breps, if not present in display-controls.
nil indicates to display only the edge curves. Keyword :tess indicates to use tessellation-lines. Defaults to
 nil.")

(defparameter *brep-vrml-timeout* 20)

(defparameter *nurbs-to-beziers-timeout* 2)

(defparameter *boolean-allow-multiple-regions?* nil "Boolean. This global parameter provides the default value
for the allow-multiple-regions? input to the boolean-merge family of primitives. 
If set to non-nil, throw warning but not error if any of the input breps has more than one 
infinite region. Defaults to nil.")

(defparameter *boolean-error-on-invalid-brep?* t "Boolean. This global parameter provides the default value
for the error-on-invalid-brep? input to the boolean-merge family of primitives. 
If set to non-nil, we throw an error instead of a warning if the resulting brep does not
pass the built-in validation test. If nil, we throw a warning but continue to return the resulting brep.
Defaults to t.")


(defparameter *debug?* nil)


(defparameter *tess-min-number-of-segments* 0
  "Integer. Min number of segments for curve tessellations for computing default tessellation-parameters. 
Default is 0, which means the value is ignored.")

(defparameter *tess-max-3d-edge-factor* 0.1
  "Number. Factor determining Max 3D edge length for a brep tessellation element for computing 
default tessellation-parameters.. This is multiplied by the longest diagonal of the brep, so a 
value of 0.1 would be 1/10 the diagonal. Defaults to 0.1.")

(defparameter *tess-min-parametric-ratio* 0
  "Number. Minimum parametric ratio for brep tessellations for computing default tessellation-parameters. 
Default is 0, which means the value is ignored.")

(defparameter *tess-max-chord-height* 0
  "Number. Maximum deviation of tessellation chord from true surface in brep tessellations
for computing default tessellation-parameters. Default is 0, which means the value is ignored.")

(defparameter *tess-max-angle-degrees* 12
  "Number. Maximum angle deviation in degrees of tessellation face or edge from true surface or curve
for computing default tessellation-parameters. Default is 15.")

(defparameter *tess-min-3d-edge* 0
  "Number. Minimum 3D edge length for tessellation element for computing default tessellation-parameters.
 Default is 0, which means the value is ignored.")

(defparameter *minimum-segment-length-default* 0.01)

(defparameter *tess-min-edge-ratio-uv* 0
  "Number. Minimum edge UV ratio for tessellation element. 
for computing default tessellation-parameters. Default is 0, which means the value is ignored.")

(defparameter *tess-max-aspect-ratio* 2.2 "Number. Maximum aspect
  ration for tessellation element.  for computing default
  tessellation-parameters.

Default is 2.2. Note: this parameter does not seem to be respected in
all cases, we are looking into the cause of this.")

(defparameter *brep-wireframe-tessellation?* nil)

(defparameter *curve-tessellation?* nil)
