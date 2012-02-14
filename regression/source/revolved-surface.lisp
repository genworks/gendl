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


(define-object revolved-surface-test (revolved-surface)

  :computed-slots
  ((regression-test-data (multiple-value-list (the b-spline-data))))
  
  :hidden-objects ((curve :type 'arc-curve
			  :center (translate (the center) :right 50)
			  :orientation (alignment :top (the (face-normal-vector :rear)))
			  :start-angle (half pi)
			  :end-angle (* 3/2 pi)
			  :radius 10)))



(register-test-definition 'revolved-surface-test)
