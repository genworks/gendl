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



(define-object swept-solid-test (swept-solid)
  
  :computed-slots
  ((facial-brep (the trimmed brep))
   (vector (make-vector 0 0 1))
   (distance 10)
   
   (regression-test-data (append (multiple-value-list (the precise-properties))
				 (the %curves-to-draw%)
				 (the %lines-to-draw%))))
  
  :hidden-objects
  ((trimmed :type 'trimmed-surface
	    :uv-inputs t
	    :island (the island-3d uv-curve)
	    :holes (list (the hole uv-curve))
	    :basis-surface (the basis))
  
   (basis :type 'planar-surface
	  :p00 (make-point 0 0 0)
	  :p01 (make-point 0 10 0)
	  :p10 (make-point 10 0 0)
	  :p11 (make-point 15 15 0))
   
   (island-3d :type 'projected-curve
	      :curve-in (the raised-island)
	      :surface (the basis)
	      :projection-vector (make-vector 0 0 -1))

   
   (hole :type 'projected-curve
	 :curve-in (the raised-hole)
	 :surface (the basis)
	 :projection-vector (make-vector 0 0 -1))
   
   (raised-hole :type 'b-spline-curve
		:control-points (list (make-point 3.5 4.5 10)
				      (make-point 4.5 6 10) 
				      (make-point 5.5 7 10) 
				      (make-point 6 4.5 10) 
				      (make-point 5.5 2 10) 
				      (make-point 4.5 2 10) 
				      (make-point 3.5 4.5 10)))
   
   (raised-island :type 'b-spline-curve
		  :control-points (list (make-point 3 5 1)
					(make-point 5 8 1) 
					(make-point 7 10 1) 
					(make-point 8 5 1) 
					(make-point 7 0 1) 
					(make-point 5 0 1) 
					(make-point 3 5 1)))))

(register-test-definition 'swept-solid-test)



;;
;; This one does not give a clean manifold.
;;
;; Compare with extruded-solid-test-4 which gives a clean one. 
;;
;;
(define-object swept-solid-test-2 (swept-solid)
  
  :computed-slots
  ((facial-brep (the e-s-t-3))
   (vector (make-vector 0 1 0))
   (distance 10)
   
   (regression-test-data (append (multiple-value-list (the precise-properties))
				 (the %curves-to-draw%)
				 (the %lines-to-draw%))))
  
  :objects
  ((e-s-t-3 :type 'extruded-solid-test-3)))




(define-object swept-solid-test-3 (swept-solid)
  
  :computed-slots
  ((facial-brep (the ruled brep))
   (vector (make-vector 0 0 1))
   (distance 10)
   
   (regression-test-data (append (multiple-value-list (the precise-properties))
				 (the %curves-to-draw%)
				 (the %lines-to-draw%))))
  
  :objects
  ((polyline :type 'global-filleted-polyline-curve-open-test)
   
   (offset-polyline :type 'boxed-curve
		    :curve-in (the polyline)
		    :center (translate (the-child curve-in center) 
				       :rear (abs (- (get-x (the polyline end))
						     (get-x (the polyline start))))))
   
   (ruled :type 'ruled-surface
	  :curve-1 (the polyline)
	  :curve-2 (the offset-polyline))))
   







