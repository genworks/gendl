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

(define-object trimmed-surface-test (trimmed-surface)
  
  :computed-slots
  ((basis-surface (the b-spline-surface))
   (holes (list (the hole)))
   (uv-inputs t)
   (reverse-island? t)
   (reverse-holes? t)
   (points-data '(((0 0 0)(4 1 0)(8 1 0)(10 0 0)(8 -1 0)(4 -1 0)(0 0 0))
		   ((0 0 2) (4 2 2) (8 2 2) (10 0 2) (8 -2 2) (4 -2 2) (0 0 2))
		   ((0 0 4) (4 2 4) (8 2 4) (10 0 4) (8 -2 4) (4 -2 4) (0 0 4))
		  ((0 0 7) (4 1 7) (8 1 7) (10 0 7) (8 -1 7) (4 -1 7) (0 0 7))))
   
   
   (regression-test-data (append (multiple-value-list (the precise-properties))
				 (the %curves-to-draw%)
				 (the %lines-to-draw%)))
   
   
   )
  
  :objects
  ((b-spline-surface :type 'b-spline-surface
		     :control-points (mapcar #'(lambda(list) (mapcar #'apply-make-point list)) (the points-data)))
   
   (island :type 'b-spline-curve
	   :control-points (list (make-point 0 0 0)
				 (make-point 0 1 0)
				 (make-point 1 1 0)
				 (make-point 1 0 0)
				 (make-point 0 0 0)))
		   
   (hole :type 'b-spline-curve 
	 :control-points (list (make-point 0.5 0.5 0)
			       (make-point 0.5 0.75 0)
			       (make-point 0.75 0.75 0)
			       (make-point 0.75 0.5 0)
			       (make-point 0.5 0.5 0)))))


(register-test-definition 'trimmed-surface-test)
