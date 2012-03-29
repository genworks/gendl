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


(in-package :training-g102)

(define-object tower-box (base-object)
  
  :input-slots ((length 10)
		(width 20)
		(height 15))
  
  :objects
  ((brick :type 'box)))


(define-object tower-boxes (base-object)
  
  :input-slots ((length 10)
		(width 20)
		(height 15)
		(number-of-bricks 25))
  
  :objects
  ((bricks :type 'box
	   :sequence (:size (the number-of-bricks)))))




(define-object stacked-boxes (base-object)
  
  :input-slots ((length 10)
		(width 20)
		(height 15)
		(number-of-bricks 25)
		)
  
  :computed-slots 
  ((tower-height (* (the number-of-bricks) (the height)))
   
   (tower-height-measured (3d-distance (the (bricks 0) (face-center :bottom))
				       (the bricks last (face-center :top)))))
   
  
  :objects
  ((bricks :type 'box
	   :sequence (:size (the number-of-bricks))
	   :center (translate (the center) :up (* (the-child height)
						  (the-child index))))))


(define-object twisted-stacked-boxes (base-object)
  
  :input-slots ((length 10)
		(width 20)
		(height 15)
		(number-of-bricks 25)
		(twist-degrees 5))


  :computed-slots 
  ((tower-height (* (the number-of-bricks) (the height)))
   
   (tower-height-measured (3d-distance (the (bricks 0) (face-center :bottom))
				       (the bricks last (face-center :top))))
   
   (total-volume (apply #'+ (list-elements (the bricks) (the-element volume)))))


  
  :objects
  ((bricks :type 'box
	   :sequence (:size (the number-of-bricks))
	   :orientation (alignment :rear (rotate-vector-d (the (face-normal-vector :rear))
							  (* (the-child index) (the twist-degrees))
							  (the (face-normal-vector :top))))
	   :center (translate (the center) :up (* (the-child height)
						  (the-child index))))))
