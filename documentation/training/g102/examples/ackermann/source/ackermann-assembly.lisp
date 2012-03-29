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

(define-object assembly (box)
  
  :documentation (:description "Schematic of a typical truck steering system.")
  
  :input-slots
  (("List of 3D points. Enabled direct placement of the kingpin axes."
    kingpin-centers (list :left (make-point 0 -200 0) :right (make-point 0 200 0)))
   ("List of keyword and number. Indicates current turn state of the mechanism."
    current-rotation-angle-spec (list :left 30)))


  :computed-slots
  ((center 
    (midpoint (getf (the kingpin-centers) :left) 
	      (getf (the kingpin-centers) :right)))
   
   (length (3d-distance (getf (the kingpin-centers) :left) 
			(getf (the kingpin-centers) :right)))
   
   (tie-rod-length (3d-distance 
		    (the known-linkage-object straight-ahead-tie-rod-arm-ball-center)
		    (the other-linkage-object straight-ahead-tie-rod-arm-ball-center)))
   (width 100)
   (height 100)
   
   (known-linkage-object 
    (the (linkages (ecase (first (the current-rotation-angle-spec)) 
		     (:left 0) (:right 1)))))
   (known-linkage-object-sphere (the known-linkage-object tie-rod-ball-socket-sphere))
   (other-linkage-object (the (linkages (- 1 (the known-linkage-object index)))))
   (other-linkage-object-circle 
    (the other-linkage-object tie-rod-arm-ball-articulation-circle))

   ;;
   ;; FLAG -- compute this based on intersection of sphere and circle.
   ;;
   (other-tie-rod-ball-location 
    (inter-circle-sphere (the other-linkage-object-circle center)
			 (the other-linkage-object-circle radius)
			 (the other-linkage-object kingpin-axis)
			 (the known-linkage-object-sphere center)
			 (the known-linkage-object-sphere radius)
			 nil))

   
   (other-tie-rod-rotation 
    (angle-between-vectors-d 
     (the other-linkage-object straight-ahead-tie-rod-arm-vector)
     (subtract-vectors (the other-tie-rod-ball-location)
		       (the other-linkage-object tie-rod-arm-pivot-point))
     (the other-linkage-object kingpin-axis) t)))

  :objects
  (
   (ball :type 'sphere
	 :display-controls (list :color :red)
	 :center (the other-tie-rod-ball-location)
	 :radius 10)
   
   (tie-rod :type 'c-cylinder
	    :radius 1
	    :start (the (linkages 0) tie-rod-arm-ball-center)
	    :end (the (linkages 1) tie-rod-arm-ball-center))
   (linkages :type 'simple-linkage
	     :sequence (:size 2)
	     :side (ecase (the-child index) (0 :left) (1 :right))
	     :tie-rod-length (the tie-rod-length)
	     :kingpin-center (getf (the kingpin-centers) (the-child side))
	     :current-rotation-angle (if (eql (the-child side) (first (the current-rotation-angle-spec)))
					 (second (the current-rotation-angle-spec))
				       (the other-tie-rod-rotation)))))
