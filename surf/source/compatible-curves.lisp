;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
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


;;
;; FLAG -- have to get rid of raw smlib call in here. 
;;
(define-object compatible-curves (base-object)

  :documentation (:description "Experimental. This primitive takes in a list of GDL 
curve objects and will compute a sequence of new curve objects which have been made 
to be compatible in terms of number of control points, knot vectors, and degree."
                  
                  :author "Dave Cooper, Genworks"
                  
                  :examples "<pre>
 
 (in-package :gdl-user)

 (define-object test-compatible-curves ()
  
  :objects
  ((b-spline :type 'b-spline-curve
             :control-points (list (make-point 0 0 0)
                                   (make-point -1 0 0)
                                   (make-point -1 1 0)
                                   (make-point 0 1 0)))
   
   (linear-curve :type 'linear-curve
                 :start (make-point 0 1 0)
                 :end (make-point 4 1 0))
   
   (compatible-curves :type 'compatible-curves
                      :curve-list (list (the b-spline) (the linear-curve)))))

  (generate-sample-drawing :object-roots (list (make-object 'test-compatible-curves)))


</pre>")
                  
  
  :input-slots (("List of GDL curve objects." curve-list)
                ("Tolerance is used to check knots removability for data reduction. A nil value indicates that no data reduction 
                  is to be attempted. Defaults to nil." tolerance nil ))
  
  :computed-slots ((%native-curves-list% 
                    (if (the tolerance)
                        (approximated-compatible-curves  *geometry-kernel* 
							 (the curve-list) :tolerance (the tolerance)) 
			(return-compatible-curves *geometry-kernel* 
						  (mapsend (the curve-list) :copy-new)))))
  
  :objects (("Sequence of GDL Curve objects. These are the resultant curves which 
 are supposed to be compatible."
             curves :type 'curve
                    :sequence (:size (length (the %native-curves-list%)))
                    :native-curve (nth (the-child index) (the %native-curves-list%)))))

(define-object test-compatible-curves ()
  
  :objects
  ((b-spline :type 'b-spline-curve
             :control-points (list (make-point 0 0 0)
                                   (make-point -1 0 0)
                                   (make-point -1 1 0)
                                   (make-point 0 1 0)))
   
   (linear-curve :type 'linear-curve
                 :start (make-point 0 1 0)
                 :end (make-point 4 1 0))
   
   (compatible-curves :type 'compatible-curves
                      :curve-list (list (the b-spline) (the linear-curve)))))
   
