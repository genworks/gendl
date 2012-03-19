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


(define-object arc (arcoid-mixin base-object)
  
  :documentation (:description "A segment of a circle.
The start point is at the 3 o'clock position, and positive
angles are measured anti-clockwise."
                  :examples  "<pre>

 (in-package :gdl-user)

 (define-object arc-sample (arc) 
   :computed-slots ((radius 30) (end-angle (half pi/2))))

 (generate-sample-drawing :objects (make-object 'arc-sample))

</pre>")
 
   :input-slots
  ("Number. Distance from center to any point on the arc."
   radius
   
   (arc 2pi)
   
   ("Angle in radians. Start angle of the arc. Defaults to zero."
    start-angle 0)
   
   ("Angle in radians. End angle of the arc. Defaults to twice pi."
    end-angle (the arc))) 

  :computed-slots
  ((%curves-to-draw% 
    
    (when (not (zerop (the radius)))
      (if (and (= (the :start-angle-normalized) 0)
               (= (the :end-angle-normalized) (twice pi)))
          (progn
            (list (the (:circle-point-list :right :rear))
                  (the (:circle-point-list :rear :left))
                  (the (:circle-point-list :left :front))
                  (the (:circle-point-list :front :right))))
        (let* ((number-of-sub-arcs (ceiling (/ (the :start-to-end-angle) (half pi))))
               (angle-per-sub-arc (/ (the :start-to-end-angle) number-of-sub-arcs))
               (start-angle-normalized (the :start-angle-normalized))
               result)
          (dotimes (n number-of-sub-arcs (nreverse result))
            (push (sub-arc (the :radius) 
                           (+ start-angle-normalized (* n angle-per-sub-arc))
                           (+ start-angle-normalized (* (1+ n) angle-per-sub-arc))
                           (the :center) 
                           (the (:face-normal-vector :right))
                           (the (:face-normal-vector :rear))
                           (the (:face-normal-vector :front))
                           (the (:face-normal-vector :top))) result))))))
   
   (%renderer-info% (list :vrml? t :view-default :top))
   
   (curves (the :%curves-to-draw%))
   

   
   ("3D Point. The start point of the arc."
    start (first (first (the :curves))))
   
   ("3D Point. The end point of the arc."
    end (fourth (lastcar (the :curves))))
   
   (height 0) (width (twice (the radius))) (length (the width)))

  
  :functions
  ((circle-point-list
    (face-1 face-2)
    (let ((center (the :center))
          (radius (the :radius))
          (kappa-factor (* (the :radius) +kappa+)))
      (list (translate center face-1 radius)
            (translate center face-1 radius face-2 kappa-factor)
            (translate center face-2 radius face-1 kappa-factor)
            (translate center face-2 radius))))
   
   (interpolated-points
    (&key (curve-chords *curve-chords*))
    (let ((beziers (list-elements (the beziers))))
      (case (length beziers)
        (0)
        (1 (the-object (first beziers) (equi-spaced-points curve-chords)))
        (2 (append (butlast (the-object (first beziers) (equi-spaced-points curve-chords)))
                   (the-object (second beziers) (equi-spaced-points curve-chords))))
        (otherwise (append (mapcan #'(lambda(bezier) 
                                       (butlast (the-object bezier (equi-spaced-points curve-chords)))) 
                                   (butlast beziers))
                           (the-object (lastcar beziers) (equi-spaced-points curve-chords)))))))
       
   

   ("3D Vector. Returns the tangent to the arc at the given point (which should be on the arc).
:arguments (point \"3D point. The point at which you want the tangent.\")"
    tangent 
    (point)
    (let ((top (the (face-normal-vector :top)))
	  (radial (subtract-vectors point (the center))))
      (cross-vectors top radial)))
   
   ("List of points. Returns a list of points equally spaced around the arc, including
the start and end point of the arc. 
:&optional ((number-of-points 4) \"Number. How many points to return.\")"

    equi-spaced-points
    (&optional (number-of-points 4))
    (when (>= number-of-points 1)
      (let ((delta (div (the start-to-end-angle) (1- number-of-points))) result)
        (dotimes (n number-of-points result)
          (push (the (point-on-arc (* n delta))) result)))))
   
   ("3D Point. The point on the arc at a certain angle from the start.
:arguments (angle \"Number in Radians\")"
    point-on-arc
    (angle)
    (if (the start)
	(rotate-point (the start) (the center) (the (face-normal-vector :top)) :angle angle)
	(the center)))))







