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


(define-object swept-solid (brep)
  
  :documentation (:description "This primitive will take a brep as input,
and sweep all its faces in the given direction by the given distance,
to produce another brep."
  
   :examples "<pre>
 (in-package :gdl-user)

 (define-object swept-solid-example (swept-solid)

   :computed-slots
   ((facial-brep (the trimmed brep))
    (vector (make-vector 0 0 1))
    (distance 10)
    (display-controls (list :isos (list :n-u 8 :n-v 8) :color :blue :transparency 0.3)))

  
   :hidden-objects
   ((rectangle :type 'rectangular-surface
               :width 20 :length 20)

    (trim-curve :type 'global-filleted-polyline-curve
                :vertex-list (list (translate (the center) :right 8 :rear 8)
                                   (translate (the center) :left 8 :rear 8)
                                   (translate (the center) :left 8 :front 8)
                                   (translate (the center) :right 8 :front 8)
                                   (translate (the center) :right 8 :rear 8))
                :default-radius 3)

    (trimmed :type 'trimmed-surface
             :basis-surface (the rectangle)
             :reverse-island? t
             :island (the trim-curve))))

 (generate-sample-drawing :objects (make-object 'swept-solid-example)
                          :projection-direction (getf *standard-views* :trimetric))

</pre>")
  
  
  :input-slots ("GDL Brep object. The original brep, which can contain one or more faces, planar and/or non-planar."
                facial-brep 
                
                "GDL Vector. The direction in which the sweep is desired."
                vector 
                
                "Number. The distance over which the sweep is desired."
                distance)
   
  :computed-slots ((%native-brep% 
                    (make-linear-swept-brep *geometry-kernel* (the facial-brep %native-brep%) 
                                            (the vector) (the distance)))))
                                        



