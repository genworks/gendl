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


(define-object compatible-curves-test (compatible-curves)

  :computed-slots
  ((curve-list (list (the b-spline) (the linear-curve)))
   
   (regression-test-data (append-elements (the curves) (multiple-value-list (the-element b-spline-data)))))
  :objects
  ((b-spline :type 'b-spline-curve
	     :control-points (list (make-point 0 0 0)
				   (make-point -1 0 0)
				   (make-point -1 1 0)
				   (make-point 0 1 0)))
   
   (linear-curve :type 'linear-curve
		 :start (make-point 0 1 0)
		 :end (make-point 4 1 0))))

(register-test-definition 'compatible-curves-test)


