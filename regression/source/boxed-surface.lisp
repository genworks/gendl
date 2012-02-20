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

(define-object boxed-surface-test (base-object)
  
  :computed-slots
  ((points-data '(((0 0 0)(4 1 0)(8 1 0)(10 0 0)(8 -1 0)(4 -1 0)(0 0 0))
		   ((0 0 2) (4 2 2) (8 2 2) (10 0 2) (8 -2 2) (4 -2 2) (0 0 2))
		   ((0 0 4) (4 2 4) (8 2 4) (10 0 4) (8 -2 4) (4 -2 4) (0 0 4))
		   ((0 0 7) (4 1 7) (8 1 7) (10 0 7) (8 -1 7) (4 -1 7) (0 0 7))))
   (control-points (mapcar #'(lambda(list) (mapcar #'apply-make-point list)) (the points-data)))
   
   (regression-test-data (mapcar #'(lambda(curve)
				     (multiple-value-list (the-object curve b-spline-data)))
				 (rest (the children)))))
  
  :objects
  ((b-spline :type 'b-spline-surface
	     :control-points (the control-points))
   
   (boxed :type 'boxed-surface
	  :surface-in (the b-spline))
   
   (translated :type 'boxed-surface
	       :surface-in (the b-spline)
	       :center (translate (the center) :left 15))
   
   (twisted :type 'boxed-surface
	    :surface-in (the boxed)
	    :orientation (alignment :left (the (face-normal-vector :top))
				    :rear (rotate-vector-d (the (face-normal-vector :rear))
							   30
							   (the (face-normal-vector :top)))))
   
   (rotated :type 'boxed-surface
 	    :surface-in (the b-spline)
	    :display-controls (list :color :purple)
	    :orientation (alignment :left 
				    (rotate-vector-d (the (face-normal-vector :left))
						     50
						     (the (face-normal-vector :rear)))))
   
   (rotated-about :type 'boxed-surface
		  :surface-in (the b-spline)
		  :display-controls (list :color :purple)
		  :orientation-center (translate (the center) :right 2.5)
		  :orientation (alignment :left 
					  (rotate-vector-d (the (face-normal-vector :left))
							   45
							   (the (face-normal-vector :rear)))))
   
   (moved-up :type 'boxed-surface
	     :surface-in (the rotated-about)
	     :center (translate (the rotated-about center) 
				:up 7
				:left 5))
   
   (straightened :type 'boxed-surface
		 :surface-in (the moved-up)
		 :orientation 
		 (alignment :left 
			    (rotate-vector-d (the-child surface-in (face-normal-vector :left))
					     45
					     (the-child surface-in (face-normal-vector :rear)))
			    :rear (the-child surface-in (face-normal-vector :rear))))
   
   (rotated-straightened :type 'boxed-surface
			 :surface-in (the straightened)
			 :orientation (the moved-up orientation)
			 :orientation-center (translate (the-child surface-in center) :up 2.5))
   
   (rotated-straightened-moved :type 'boxed-surface
			       :surface-in (the rotated-straightened)
			       :center (translate (the-child surface-in center) :right 5))
   
   (moved-up-and-straightened :type 'boxed-surface
			      :surface-in (the straightened)
			      :center (translate (the-child orientation-center) :right 7)
			      :orientation (alignment :left (the-child surface-in (face-normal-vector :rear))
						      :front
						      (rotate-vector-d (the-child surface-in (face-normal-vector :left))
								       45
								       (the-child surface-in (face-normal-vector :rear))))
			      :orientation-center (translate (the straightened center) :up 2.5))
   
   (moved-up-and-straightened-1 :type 'boxed-surface
				:surface-in (the straightened)
				:center (translate (the-child surface-in center) :right 14)
				:orientation (the rotated-straightened orientation)
				:orientation-center (translate (the straightened center) :up 2.5))
   
   (moved-up-and-straightened-2 :type 'boxed-surface
				:surface-in (the straightened)
				:center (translate (the-child surface-in center) :right 21)
				:orientation (the rotated-straightened orientation)
				:orientation-center (translate (the straightened center) :up 2.5))
   
   (transformed :type 'boxed-surface
		:surface-in (the b-spline)
		:center (translate (the center) :left 50)
		:orientation (alignment :rear 
					(rotate-vector-d (the (face-normal-vector :rear))
							 30
							 (the (face-normal-vector :right)))))))

(register-test-definition 'boxed-surface-test)
