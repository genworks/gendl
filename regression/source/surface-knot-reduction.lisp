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

(define-object surface-knot-reduction-test (base-object) 

  :input-slots
  ((control-points (list (make-point 0 0 0)
			 (make-point 2 3.0 0.0) 
			 (make-point 4 2.0 0.0) 
			 (make-point 5 0.0 0.0) 
			 (make-point 4 -2.0 0.0) 
			 (make-point 2 -3.0 0.0) 
			 (make-point 0 0 0))))

  
  :computed-slots 
  ((regression-test-data (multiple-value-list (the S-knot-reduction b-spline-data))))
					
		   
		   
  :objects
  ((curve-1 :type 'b-spline-curve
	    :display-controls (list :line-thickness 2 :color :green-spring-medium)
	    :control-points (the control-points))

   (curve-2 :type 'boxed-curve 
	    :display-controls (list :line-thickness 2 :color :blue)
	    :curve-in (the curve-1)
	    :center (make-point 0 0 8))

   (curve-3 :type 'transformed-curve
	    :display-controls (list :line-thickness 2 :color :green)
	    :curve-in (the curve-1)
			:to-location (translate 
				      (the center) 
				      :up 3)
			:center (the center)
			:scale-x 1.3
			:scale-y 1.3)
   
   (curve-4 :type 'transformed-curve
	    :display-controls (list :line-thickness 2 :color :red)
	    :curve-in (the curve-1)
			:to-location (translate 
				      (the center) 
				      :up 7)
			:center (the center)
			:scale-x 2.2
			:scale-y 2.2)
  
   (lofted-surface-test-simple :type 'lofted-surface
			       :display-controls (list :color :red-violet :isos (list :n-v 19 :n-u 19))
			       :tolerance 0.01
			       :curves (list (the curve-1) (the curve-3)
					     (the curve-4) (the curve-2)))			       

  (S-knot-reduction :type 'surface-knot-reduction
		    :surface (the lofted-surface-test-simple))))


(register-test-definition 'surface-knot-reduction-test)
