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


(define-object extruded-solid-test (extruded-solid)
  :computed-slots
  ((profile (the fitted))
   (height 10)

   (regression-test-data (progn (format t "computing regression test data~%")
				(append (multiple-value-list (the precise-properties))
					(the %curves-to-draw%)
					(the %lines-to-draw%))))
   
   (number-of-sample-points 20 :settable))

  :objects
  ((fitted :type 'fitted-curve
	   :points (the circle (equi-spaced-points (the number-of-sample-points))))
   (circle :type 'circle :radius 10)
   ))
   

(register-test-definition 'extruded-solid-test)


(define-object extruded-solid-test-2 (extruded-solid)
  :computed-slots
  ((profile (the polyline))
   (height 10)

   (regression-test-data (append (multiple-value-list (the precise-properties))
				 (the %curves-to-draw%)
				 (the %lines-to-draw%))))

  :objects
  ((polyline :type 'global-filleted-polyline-curve-test)))



(define-object extruded-solid-test-3 (extruded-solid)
  :computed-slots
  ((profile (the polyline))
   (height 10)

   (regression-test-data (append (multiple-value-list 
				  (the precise-properties))
				 (the %curves-to-draw%)
				 (the %lines-to-draw%))))

  :objects
  ((polyline :type 'global-filleted-polyline-curve-open-test)))




;;
;; This one gives a clean manifold.
;;
;; Compare with swept-solid-test-2 which does not. 
;;
;;
(define-object extruded-solid-test-4 (extruded-solid)
  :computed-slots
  ((profile (the composed))
   (height 10)
   (regression-test-data (append (multiple-value-list 
				  (the precise-properties))
				 (the %curves-to-draw%)
				 (the %lines-to-draw%)))
   
   
   )

  :objects
  ((composed :type 'composed-curve
	     :curves (list (the polyline) (the (sides 0)) (the offset-polyline) (the (sides 1))))
   
   (polyline :type 'global-filleted-polyline-curve-open-test)

   (sides :type 'linear-curve
	  :sequence (:size 2)
	  :start (ecase (the-child index)
		   (0 (the polyline start))
		   (1 (the polyline end)))
	  :end (ecase (the-child index)
		 (0 (the offset-polyline start))
		 (1 (the offset-polyline end))))
   
   (offset-polyline :type 'boxed-curve
		    :curve-in (the polyline)
		    :center (translate (the-child curve-in center) 
				       :rear (abs (- (get-x (the polyline end))
						     (get-x (the polyline start))))))))
