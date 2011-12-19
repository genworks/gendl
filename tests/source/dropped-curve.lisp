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

(define-object dropped-curve-test (dropped-curve)
  
  :input-slots
  ((grid-width 10 :settable)
   (grid-length 10 :settable)
   (grid-height 5 :settable))
  
  :computed-slots
  ((curve-in (the raised-island))
   (surface (the basis-surface))
   
   (regression-test-data (multiple-value-list (the b-spline-data))))

  :objects
  ((basis-surface :type 'fitted-surface
		  :points (list (list (make-point 0 0 0)
				      (make-point (/ (the grid-width) 4) 0 0)
				      (make-point (half (the grid-width)) 0 0)
				      (make-point (* 3/4 (the grid-width)) 0 0)
				      (make-point (the grid-width) 0 0))
				
				(list (make-point 0 (/ (the grid-length) 4) 0)
				      (make-point (/ (the grid-width) 4) (/ (the grid-length) 4) (/ (the grid-height) 4))
				      (make-point (half (the grid-width)) (/ (the grid-length) 4) 
						  (* (/ (the grid-height) 4) 1.6))
				      (make-point (* 3/4 (the grid-width)) (/ (the grid-length) 4) (/ (the grid-height) 4))
				      (make-point (the grid-width) (/ (the grid-length) 4) 0))
				
				(list (make-point 0 (half (the grid-length)) 0)
				      (make-point (/ (the grid-width) 4) (half (the grid-length)) 
						  (* (/ (the grid-height) 4) 1.8))
				      (make-point (half (the grid-width)) (half (the grid-length)) (the grid-height))
				      (make-point (* 3/4 (the grid-width)) (half (the grid-length)) (* 3/4 (the grid-height)))
				      (make-point (the grid-width) (half (the grid-length)) 0))
				
				(list (make-point 0 (* 3/4 (the grid-length)) 0)
				      (make-point (/ (the grid-width) 4) (* 3/4 (the grid-length)) 
						  (min (* (/ (the grid-height) 4) (* (/ (the grid-height) 4) 1.4)) 
						       (the grid-height)))
				      (make-point (half (the grid-width)) (* 3/4 (the grid-length)) 
						  (min (* (/ (the grid-height) 4) (* (/ (the grid-height) 4) 1.8)) 
						       (the grid-height)))
				      (make-point (* 3/4 (the grid-width)) (* 3/4 (the grid-length)) 
						  (/ (the grid-height) 4))
				      (make-point (the grid-width) (* 3/4 (the grid-length)) 0))
				
				(list (make-point 0 (the grid-length) 0)
				      (make-point (/ (the grid-width) 4) (the grid-length) 0)
				      (make-point (half (the grid-width)) (the grid-length) 0)
				      (make-point (* 3/4 (the grid-width)) (the grid-length) 0)
				      (make-point (the grid-width) (the grid-length) 0))))
   
   (raised-island :type 'b-spline-curve
		  :control-points (list (make-point 3 5 7) (make-point 5 8 7) (make-point 7 10 7) 
					(make-point 8 5 7) (make-point 7 0 7) (make-point 5 0 7) 
					(make-point 3 5 7)))))


(register-test-definition 'dropped-curve-test)
