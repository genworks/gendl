;;
;; Copyright 2014 Genworks International 
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

(define-object conic-curve (curve)

  :documentation (:description "A curve constructed from conic inputs. Inputs are two endpoints, two end tangents,
and a point on the curve."
                  
                  :examples "<pre>

 (in-package :gdl-user)

 (define-object conic-curve-test (conic-curve)
    :computed-slots
    ((start-point (make-point -10 0 0))
     (start-tangent (make-point 0.1 1 0))
     (end-point (make-point 10 0 0))
     (end-tangent (make-point -0.1 1 0))
     (point-on-curve (make-point 0 12 0))))
     
  (generate-sample-drawing :objects (make-object 'conic-curve-test))


</pre>")


  :input-slots ("3D Point. Start Point" 
		start-point
		"3D Vector. Start Tangent"
		start-tangent
		"3D Point. End Point" 
		end-point
		"3D Vector. End Tangent"
		end-tangent
		"3D Point. A point on the curve."
		point-on-curve)
		
  :computed-slots
  ((native-curve (funcall 
		  (symbol-function
		   (read-from-string "smlib::make-conic-curve"))
		  *geometry-kernel* 
		  (the start-point) (the start-tangent) 
		  (the end-point) (the end-tangent)
		  (the point-on-curve)))))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'conic-curve))
