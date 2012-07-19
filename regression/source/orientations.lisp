;;
;; Copyright 2012 Genworks International
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

(define-object orientations (base-object)
  
  :input-slots ((max-depth 2) (depth 0) (length 5) (width 15) (height 2)
		(display-controls (list :color :grey :transparency 0.7)))

  :objects
  ((left-box :type (if (> (the depth) (the max-depth)) 'null-part 'orientations )
	     :display-controls (list :line-thickness 2 :color :red :transparency 0.5)
	     :max-depth (the max-depth)
	     :depth (1+ (the depth))
	     :center (translate (the center) :left (- (half (the width)) (half (the-child width))))
	     :length (half (the length)) :width (half (the width)) :height (half (the height))
	     :orientation (alignment :rear (rotate-vector-d (the (face-normal-vector :rear))
							    45
							    (the (face-normal-vector :right)))
				     
				     :right 
				     (the (face-normal-vector :right))
				     #+nil
				     (rotate-vector-d (the (face-normal-vector :right))
						      20
						      (the (face-normal-vector :top)))))

   (right-box :type (if (> (the depth) (the max-depth)) 'null-part 'orientations)
	      :display-controls (list :color :green :transparency 0.5)
	      :max-depth (the max-depth)
	      :depth (1+ (the depth))
	      :center (translate (the center) :right (- (half (the width)) (half (the-child width))))
	      :length (half (the length)) :width (half (the width)) :height (half (the height))
	      :orientation (alignment :rear (rotate-vector-d (the (face-normal-vector :rear))
							     45
							     (the (face-normal-vector :right)))
				      :right 
				      (the (face-normal-vector :right))
				      #+nil
				      (rotate-vector-d (the (face-normal-vector :left))
							      20
							      (the (face-normal-vector :bottom)))))
   

   (axis :type 'axes )
   
   (box :type 'box)

   ))


(define-object axes (base-object)
  
  :input-slots 
  ((length 10 :defaulting)
   (width 10 :defaulting)
   (height 10 :defaulting))

  :objects
  ((x-line :type 'c-cylinder
	   :radius 0.1
	   :start (the center)
	   :end (translate (the-child start) :right (the width))
	   :display-controls (list :color :red))

   (y-line :type 'c-cylinder
	   :radius 0.1
	   :start (the center)
	   :end (translate (the-child start) :rear (the length))
	   :display-controls (list :color :green))

   (z-line :type 'c-cylinder
	   :radius 0.1
	   :start (the center)
	   :end (translate (the-child start) :top (the height))
	   :display-controls (list :color :blue))))
