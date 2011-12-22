;;
;; Copyright 2002, 2009 Genworks International and Genworks BV 
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


(in-package :gdl-lift-tests)


(define-object planar-offset-curve-test (planar-offset-curve)
  :computed-slots
  ((curve-in (the curve))
   (plane-normal (make-vector 0 0 -1))
   (distance 1)
   
   (control-points (list (make-point 0 0 0)
			 (make-point 2 3.0 0.0) 
			 (make-point 4 2.0 0.0) 
			 (make-point 5 0.0 0.0) 
			 (make-point 4 -2.0 0.0) 
			 (make-point 2 -3.0 0.0) 
			 (make-point 0 0 0)))
   
   (regression-test-data (multiple-value-list (the b-spline-data))))
  
  :objects
  ((curve :type 'b-spline-curve
           :control-points (the control-points)
	   :degree 4)))



(register-test-definition 'planar-offset-curve-test)
