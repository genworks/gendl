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


(in-package :gdl-user)


(define-object table (box)
  
  :input-slots 
  ((leg-type 't-support-1)
   (length 50) 
   (width 70)
   (height 20 :settable)
   (top-thickness 1/2)
   
   (number 2 :settable)
   
   )

  
  :objects
  ((table-top :type 'box
	      :length (the length)
	      :width (the width)
	      :height (the top-thickness))
   
   (legs :type (the leg-type)
	 :length (the height)
	 :width 2
	 :height 3
	 :orientation (alignment (ecase (first (the-child index))
				   (1 :bottom)
				   (0 :top))
				 (the (face-normal-vector :rear)))
	 :center (translate (the center)
			    (ecase (second (the-child index))
			      (0  :right)
			      (1  :left))
			    (- (half (the width)) (half (the-child width)))
			    (ecase (first (the-child index))
			      (0  :rear)
			      (1  :front))
			    (- (half (the length))
			       (half (the-child height)))
			    
			    :down (+ (half (the top-thickness))
				     (half (the-child length))))
			      
	 :sequence (:matrix :longitudinal (the number) :lateral (the number)))))
   
   
