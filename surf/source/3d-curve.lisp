;;
;; Copyright 2013 Genworks International 
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


(define-object 3d-curve (b-spline-curve)
  
  :documentation (:description "Given a uv on-surface curve and its
surface, produce the corresponding 3d curve.  Note this must be a true
on-surface curve, not just an arbitrary curve whose domain happens to
be in the parameter space of the surface.")

  :input-slots ("GDL Curve object. Curve which is a proper on-surface
		curve (e.g. produced by a project or drop operation
		onto the surface)."  uv-curve

		"GDL Surface object. The surface corresponding to the given uv-curve."
		surface)

  :computed-slots ((knot-vector (the uv-curve knot-vector))
		   (weights (the uv-curve weights))
		   (degree (the uv-curve degree))
		   (control-points (mapcar #'(lambda(point)
					       (the surface (point (get-x point) (get-y point))))
					   (the uv-curve control-points)))))


(define-object 3d-approximated (approximated-curve)

  :documentation (:description "Given a curve in uv parameter space of
  a surface, produce a corresponding 3d curve with brute-force fitting
  techniques.")

  :input-slots 
  ("GDL Curve object. Curve whose points are understood to be 2D u, v
   parameter values on the surface."  
   uv-curve

   "GDL Surface object. The surface corresponding to the given uv-curve."
   surface

   ("Integer. How many point samples to take for fitting
   purposes. Default is 42." 
    number-of-samples 42)

   ("Number. The tolerance for the final approximation, to be
   multiplied by the 3d curve length. Default is 10e-5."
   tolerance-factor 10e-5))

  :hidden-objects
  ((3d-curve :type 'fitted-curve
	     :points (mapcar #'(lambda(point)
				 (the surface (point (get-x point) (get-y point))))
			     (the uv-curve (equi-spaced-points (the number-of-samples))))))

  :computed-slots
  ((tolerance (* (the tolerance-factor) (the 3d-curve total-length)))
   (curve-in (the 3d-curve))))
  
  