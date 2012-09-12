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


(define-object spiral-curve (curve)

  :documentation (:description 
		  "This object approximates a spiral around the Z axis using a cubic NURBS (b-spline-curve)."
		  :author "Dave Cooper, Genworks International")


  :input-slots
  ("Number. The height of the spiral." 
   height

   "Number. Initial radius at Z=0." radius-1

   "Number. Final radius at z=height." radius-2

   ("Keyword Symbol, :right or :left. Defaults to :right." right-or-left :right)
   
   "Number. The number of turns (1 = 360 degrees) in the spiral." number-of-turns

   ("Number. The amount by which to divide the height of the curve-in to compute the default tolerance. Default is 1000." 
    tolerance-divisor 1000)
   
   ("Number. The tolerance to use for non-rational approximation of a rational curve-in. 
Defaults to the height divided by the tolerance-divisor." 
    tolerance (div (the height) (the tolerance-divisor))))
    

  :computed-slots
  ((native-curve (make-spiral *geometry-kernel* 
			      (to-double-float (the height))
			      (to-double-float (the radius-1))
			      (to-double-float (the radius-2))
			      (to-double-float (the number-of-turns))
			      (ecase (the right-or-left) (:right 0) (:left 1))
			      (to-double-float (the tolerance))))))

;;
;; FLAG -- add to Lift tests. 
;;

(define-object spiral-curve-test (base-object) 

  :computed-slots ()

  :objects 
  ((spiral-curve :type 'surf::spiral-curve
		 :height 10.0
		 :radius-1 2.0
		 :radius-2 2.0
		 :number-of-turns 2.0
		 :right-or-left :right
		 :tolerance 0.001)
   
   
   (boxed-1 :type 'boxed-curve
	    :curve-in (the spiral-curve)
	    :center (translate (the center) :left 10))

   (boxed-2 :type 'boxed-curve
	    :curve-in (the spiral-curve)
	    :orientation (alignment :top (the (:face-normal-vector :rear))))))
