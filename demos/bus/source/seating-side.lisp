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

(define-object seating-side (base-object)

  :input-slots
  (reclined-angle
   fare-class
   side
   body-reference-point
   number-of-rows
   x-vector
   x-max-typical
   inter-seat-spacing
  
   )

  :computed-slots
  ((inboard-direction (ecase (the side) 
			(:left :rear) 
			(:right :front)))
   
   (use-local-box? nil)
   )

  :objects
  ((seats :type 'seat
	  :sequence (:size (the number-of-rows))
	  :pass-down (:reclined-angle :max-reclined-angle :x-vector)
	  :transformation-matrix 
	  (alignment :rear (the-child y-vector) :right
		     (the-child x-vector) :top
		     (the-child z-vector))
	  :orientation (the-child transformation-matrix)
	  :center (translate 
		   (the body-reference-point) :up
		   (+ (the-child (legs 0) height) 
		      (the-child base height))
		   :left
		   (+ (the x-max-typical)
		      (* (the-child index) (the inter-seat-spacing)))
		   (the inboard-direction) 
		   (half (the-child base-length))))))
