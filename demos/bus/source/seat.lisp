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

(in-package :genworks.demos.bus)

(define-object seat (base-object)

  :input-slots
  (fare-class
   reclined-angle
   max-reclined-angle
   (x-vector (make-vector 1 0 0))
   (y-vector (make-vector 0 1 0))
   (z-vector (make-vector 0 0 1))
   (base-width 18)
   (base-height 3.5)
   (base-length 36)
   (back-width 25)
   (back-height (* (the :base-height) 1.33))
   (back-length (the :base-length))
   (leg-width 3.5)
   (leg-height 15)
   (leg-length (* (the :base-length) 0.69))
   (leg-offset 4)
   
   (use-local-box? nil)
   )

  :computed-slots
  ((self-orientation (alignment :rear (the :y-vector) :right (the :x-vector) :top
				(the :z-vector)))
   (clearance-max (- (get-x (the :back (:face-center :bottom)))
		     (get-x (the :center))))
   (x-max (- (get-x (the :back (:edge-center :bottom :right)))
	     (get-x (the :center))))
   (x-min (- (get-x (the :base (:edge-center :top :left))) (get-x (the :center))))
   (clearance-extent (- (the :clearance-max) (the :x-min)))
   (x-extent (- (the :x-max) (the :x-min))))

  :objects
  ((legs :type 'box
	 :sequence (:size 2)
	 :length (the :leg-length)
	 :width (the :leg-width)
	 :height (the :leg-height)
	 :center (translate (the :center) :down
			    (+ (half (the-child :height)) (the :base-height)) :left
			    (ecase (the-child :index)
			      (0 (- (the :base :width) (the :leg-offset)))
			      (1 (the :leg-offset)))))
   (base :type 'box
	 :length (the :base-length)
	 :width (the :base-width)
	 :height (the :base-height)
	 :center (translate (the :center) :left (half (the-child :width)) :down
			    (half (the-child :height))))
   (back :type 'box
	 :length (the :back-length)
	 :width (the :back-width)
	 :height (the :back-height)
	 :center (let ((nominal-center
			(translate (the :center) :up (half (the-child :width)))))
		   (rotate-point-d nominal-center (the :center) (the :y-vector)
				   :angle (the :reclined-angle)))
	 :transformation-matrix (alignment :right
					   (rotate-vector-d (the :z-vector)
							    (the :reclined-angle)
							    (the :y-vector))
					   :top
					   (rotate-vector-d (reverse-vector
							     (the :x-vector))
							    (the :reclined-angle)
							    (the :y-vector)))
	 :orientation (the-child :transformation-matrix))))
