(in-package :gdl-user)

;;
;; Copyright 2015 Genworks International 
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


(define-object redirected-surface (b-spline-surface)

  :documentation 
  (:description 
   "Generates a new surface with U and V parameters swapped and/or
   reversed in their direction.")


  :input-slots
  ("GDL surface object" 
   surface-in

   ("Boolean. Specifies whether existing U and V should be swapped before possibly reversing. Defaults to nil." swap-uv? nil)
   ("Boolean. Specifies whether resulting U parameter space should be reversed. Defaults to nil." reverse-u? nil)
   ("Boolean. Specifies whether resulting V parameter space should be reversed. Defaults to nil." reverse-v? nil))


  :computed-slots
  ((control-points (the (transpose-and-reverse-as-needed (the surface-in control-points))))
   (weights (the (transpose-and-reverse-as-needed (the surface-in weights))))
   (u-degree (the surface-in u-degree))
   (v-degree (the surface-in v-degree))
   (u-knot-vector (the (swap-and-reverse-as-needed (the surface-in u-knot-vector) (the surface-in v-knot-vector) (the reverse-u?))))
   (v-knot-vector (the (swap-and-reverse-as-needed (the surface-in v-knot-vector) (the surface-in u-knot-vector) (the reverse-v?)))))


  :functions 
  ((transpose-and-reverse-as-needed 
    (list-o-lists)
    (let* ((swapped (if (the swap-uv?)
			(apply #'mapcar #'list list-o-lists)
			list-o-lists))
	   (rows (if (the reverse-u?) (reverse swapped) swapped)))
      (if (the reverse-v?) (mapcar #'reverse rows) rows)))
   
   (swap-and-reverse-as-needed 
    (u-knot-vector v-knot-vector reverse?)
    (cond ((and (the swap-uv?) reverse?) (the (reverse-knots v-knot-vector)))
	  ((the swap-uv?) v-knot-vector)
	  (reverse? (the (reverse-knots u-knot-vector)))
	  (t u-knot-vector)))
   
   (reverse-knots 
    (knot-vector)

    (let* ((min (first knot-vector)) (max (lastcar knot-vector)))
      (mapcar #'(lambda(knot)
		  (let ((difference (- max knot)))
		    (+ min difference))) 
	      (reverse knot-vector))))))



(in-package :gdl-user)


(define-object test-redirected-surfaces (base-object)

  :input-slots ((n-u 12) (n-v 12))

  :objects
  ((test-surface :type 'test-fitted-surface 
		 :display-controls (list :color :green :isos (list :n-u (the n-u)
								   :n-v (the n-v))))

   (swapped-uv :type 'surf::redirected-surface
	       :surface-in (the test-surface)
	       :swap-uv? t
	       :display-controls (list :color :red :isos (list :n-u (the n-u)
								 :n-v (the n-v))))

   (swapped-uv-reverse-u :type 'surf::redirected-surface
			 :surface-in (the test-surface)
			 :swap-uv? t
			 :reverse-u? t
			 :display-controls (list :orange :green :isos (list :n-u (the n-u)
								   :n-v (the n-v))))

   (swapped-uv-reverse-v :type 'surf::redirected-surface
			 :surface-in (the test-surface)
			 :swap-uv? t
			 :reverse-v? t
			 :display-controls (list :color :yellow :isos (list :n-u (the n-u)
									   :n-v (the n-v))))

   (swapped-uv-reverse-uv :type 'surf::redirected-surface
			 :surface-in (the test-surface)
			 :swap-uv? t
			 :reverse-v? t
			 :reverse-u? t
			 :display-controls (list :color :blue :isos (list :n-u (the n-u)
									   :n-v (the n-v))))

   (reverse-u  :type 'surf::redirected-surface
	       :surface-in (the test-surface)
	       :reverse-u? t
	       :display-controls (list :color :indigo :isos (list :n-u (the n-u)
								 :n-v (the n-v))))

   (reverse-v  :type 'surf::redirected-surface
	       :surface-in (the test-surface)
	       :reverse-v? t
	       :display-controls (list :color :violet :isos (list :n-u (the n-u)
								 :n-v (the n-v))))

   
   (reverse-uv  :type 'surf::redirected-surface
		:surface-in (the test-surface)
		:reverse-v? t
		:reverse-u? t
		:display-controls (list :color :cyan :isos (list :n-u (the n-u)
								  :n-v (the n-v))))))

   
