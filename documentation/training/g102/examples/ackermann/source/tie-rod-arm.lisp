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

(in-package :ackermann)

(define-object tie-rod-arm (box)

  :input-slots
  (kingpin-center
   kingpin-axis
   front-vector
   offset
   current-rotation-angle
   (ball-radius 20))

  :computed-slots
  ((center (translate-along-vector (the kingpin-center) (the kingpin-axis) (the offset)))
   (orientation (alignment :top (the kingpin-axis) :front
			   (rotate-vector-d (the front-vector) (the current-rotation-angle) 
					    (the kingpin-axis))))
   (ball-center (the (face-center :rear))))

  :objects
  ((ball :type 'sphere
	 :radius (the ball-radius)
	 :center (the ball-center))
   (pipe :type 'c-cylinder
	 :radius (half (the width))
	 :start (the center)
	 :end (the ball center))))
