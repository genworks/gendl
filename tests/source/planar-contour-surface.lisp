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


(define-object planar-contour-surface-test (planar-contour-surface)
  :computed-slots
  ((regression-test-data (multiple-value-list (the b-spline-data))))
 
  :hidden-objects
  ((island-curve :type 'b-spline-curve
		 :control-points (list (make-point 3 5 1)
				       (make-point 5 8.0 1) 
				       (make-point 7 10.0 1) 
				       (make-point 8 5.0 1) 
				       (make-point 7 0.0 1) 
				       (make-point 5 0.0 1) 
				       (make-point 3 5 1)))))


(register-test-definition 'planar-contour-surface-test)
