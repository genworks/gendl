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

(in-package :surf)



(define-object planar-surface (surface)
  
  :documentation (:description "Creates a flat quadrilateral surface specified by its four corner points."
                  :examples "<pre>

 (in-package :surf)

 (define-object test-planar-surface (planar-surface)
   :computed-slots
   ((display-controls (list :color :fuchsia :transparency 0.5))
    (p00 (make-point 0 0 0))
    (p01 (make-point 0 1 0))
    (p10 (make-point 1 0 0))
    (p11 (make-point 1.5 1.5 0))))

  (generate-sample-drawing :objects (make-object 'test-planar-surface)
                           :projection-direction :trimetric)


</pre>")
  
  :input-slots
  ("3D point. Front-left  corner of the planar surface." p00
   "3D point. Front-right corner of the planar surface." p01 
   "3D point. Rear-left  corner of the planar surface." p10
   "3D point. Rear-right corner of the planar surface." p11)

  :computed-slots
  ((native-surface (make-planar-surface *geometry-kernel* (the p00) (the p01) (the p10) (the p11)))))

(define-object test-planar-surface (planar-surface)
  :computed-slots
  ((p00 (make-point 0 0 0))
   (p01 (make-point 0 1 0))
   (p10 (make-point 1 0 0))
   (p11 (make-point 1.5 1.5 0)))
  
  :hidden-objects
  ((view :type 'base-view
         :projection-vector (getf *standard-views* :trimetric)
         :page-width (* 5 72) :page-length (* 5 72)
         :objects (list self))))

