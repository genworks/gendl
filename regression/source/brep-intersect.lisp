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

(in-package gdl-lift-tests)


(define-object brep-intersect-test (brep-intersect)
  
  :computed-slots
  ((points-data '(((0 0 0)(4 1 0)(8 1 0)(10 0 0)(8 -1 0)(4 -1 0)(0 0 0))
		   ((0 0 2) (4 2 2) (8 2 2) (10 0 2) (8 -2 2) (4 -2 2) (0 0 2))
		   ((0 0 4) (4 2 4) (8 2 4) (10 0 4) (8 -2 4) (4 -2 4) (0 0 4))
		   ((0 0 7) (4 1 7) (8 1 7) (10 0 7) (8 -1 7) (4 -1 7) (0 0 7))))
   (control-points (mapcar #'(lambda(list) (mapcar #'apply-make-point list)) (the points-data)))
   (brep (the b-spline brep))
   (other-brep (the plane brep))
   
   (regression-test-data (mapcar #'(lambda(curve)
				     (multiple-value-list (the-object curve b-spline-data)))
				 (the edges list-elements))))
  
  :objects
  ((b-spline :type 'b-spline-surface
	     :control-points (mapcar #'(lambda(list) (mapcar #'apply-make-point list)) (the points-data)))
   (plane :type 'planar-surface
			:p00 (make-point -1 -2 3.5)
			:p01 (make-point 10 -2 3.5)
			:p10 (make-point -1 2 3.5)
			:p11 (make-point 10 2 3.5))))

(register-test-definition 'brep-intersect-test)
