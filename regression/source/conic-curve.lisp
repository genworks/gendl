;;
;; Copyright 2014 Genworks International and Genworks BV 
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


(define-object conic-curve-test (conic-curve)
    :computed-slots
    ((start-point (make-point -10 0 0))
     (start-tangent (make-point 0.1 1 0))
     (end-point (make-point 10 0 0))
     (end-tangent (make-point -0.1 1 0))
     (point-on-curve (make-point 0 12 0))
     
     (regression-test-data (list (multiple-value-list (the b-spline-data))
				 (the %curves-to-draw%)
				 ))))


(register-test-definition 'conic-curve-test)
