;;
;; Copyright 2002-2011 Genworks International 
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

(define-object arcoid-mixin ()
  
  :documentation (:description  "This object is a low level object used to define 
an arc like object. It is not recommended to be used directly by GDL common users. 
For developers it should be used as a mixin.")

  :input-slots
  ("Number. Distance from center to any point on the arc."
   radius
   
   (arc 2pi)
   
   ("Angle in radians. Start angle of the arc. Defaults to zero."
    start-angle 0)
   
   ("Angle in radians. End angle of the arc. Defaults to twice pi."
    end-angle (the arc)))

  
  :computed-slots
  ((angles-normalized (the (normalize-start-end (the start-angle) (the end-angle))))
   (start-angle-normalized (apply #'min (the angles-normalized)))
   (end-angle-normalized (apply #'max (the angles-normalized)))
   
   (start-to-end-angle (- (the end-angle-normalized) (the start-angle-normalized)))
   
   (total-length (* (the radius) (the start-to-end-angle)))
   
   (%arcs% (list self)))
  
  
  :functions
  ((normalize-start-end
    (start-angle end-angle)
    (let* ((start-angle-normalized (when start-angle (mod start-angle 2pi)))
           (end-angle-normalized (when end-angle
                                   (let ((nominal (mod end-angle 2pi)))
                                     (if (or (zerop nominal)
                                             (< nominal start-angle-normalized))
                                         (+ nominal 2pi) nominal)))))
      (list start-angle-normalized end-angle-normalized)))))









