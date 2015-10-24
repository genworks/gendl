;;
;; Copyright 2002-2011, 2012 Genworks International
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

(in-package :surf)


(define-object cardinal-spline (curve)

  :documentation (:description 
		  "This object makes a Cardinal Spline, which defaults to a Catmull-Rom Spline for nil tension-params (which means they all default to 0.0)"
		  :author "Dave Cooper, Genworks International")

  :input-slots (through-points
		(tension-params nil)
		(periodic? nil)
		(alpha 0))

  :computed-slots ((native-curve-iw (make-cardinal-spline *geometry-kernel* 
							  :periodic? (the periodic?)
							  :tension-params (the tension-params)
							  
							  :control-points (if (and (the periodic?)
										   (coincident-point? (first (the through-points))
												      (lastcar (the through-points))))
									      (butlast (the through-points))
									      (the through-points))
							  :alpha (the alpha)))))
							  


(define-object test-uniform-cr (cardinal-spline)

  :computed-slots 
  ((through-points (list (make-point 0 0 0)
			 (make-point 1 1 0)
			 (make-point 1.1 1 0)
			 (make-point 2 0 0)))


   (periodic? t)))

(define-object test-centripetal-cr (cardinal-spline)

  :computed-slots 
  ((alpha 0.5)
   (through-points (list (make-point 0 0 0)
			 (make-point 1 1 0)
			 (make-point 1.1 1 0)
			 (make-point 2 0 0)))


   (periodic? t)))

(define-object test-chordal-cr (cardinal-spline)

  :computed-slots 
  ((alpha 1)
   (through-points (list (make-point 0 0 0)
			 (make-point 1 1 0)
			 (make-point 1.1 1 0)
			 (make-point 2 0 0)))


   (periodic? t)))






