;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
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

(in-package :geom-base)

;;
;; FLAG -- consider name change to something less cryptic
;;
(define-object c-cylinder (cylinder)
  
  :documentation
  (:description "Provides a simple way to create a cylinder, by specifying a start point and an end point."
   
   
   :examples "<pre>

 (in-package :gdl-user)

 (define-object c-cylinder-sample (c-cylinder)
   :computed-slots
   ((display-controls (list :color :plum :transparency 0.2))
    (start (make-point 0 0 0))
    (end (make-point 0 0 10))
    (number-of-sections 7)
    (radius 3)))

   (generate-sample-drawing :objects (make-object 'c-cylinder-sample)
                            :projection-direction (getf *standard-views* :trimetric))
   

</pre>")
  
  :input-slots ("3D Point. Center of the start cap." start 
                "3D Point. Center of the end cap." end)
  
  :computed-slots
  ((%renderer-info% (list :vrml? t :view-default :trimetric))
   
   ("List of two 3D Points. Represents line segment connecting center of end cap to center of start cap."
    center-line (list (the :end) (the :start)))
   
   ("3D Point. Center point of the center-line."
    center (mid-point (the :center-line)))
   
   ("Number. Distance between cap centers."
    length (line-length (the :center-line)))
   
   ("3x3 Orthonormal Rotation Matrix. Resultant orientation given the specified start and end points."
    orientation (alignment :front (direction-vector (the :center-line))))))
