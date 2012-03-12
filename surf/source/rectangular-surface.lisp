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

(define-object rectangular-surface (planar-surface)

  :documentation (:description "Creates a flat rectangular surface specified by the same inputs as box or base-object."
                  
                  :examples 
 "<pre>

 (in-package :surf)

 (define-object test-rectangular-surface (rectangular-surface)
   :computed-slots ((display-controls (list :color :green-spring-medium)) 
                    (length 20) (width 30) (height 0)))
 
 (generate-sample-drawing :objects (make-object 'test-rectangular-surface)
                          :projection-direction :trimetric)


 </pre>")
  
  :input-slots
  ((center (make-point 0 0 0) :defaulting) length width height)
  
  :computed-slots
  ((p00 (translate (the center) :left (half (the width)) :front (half (the length))))
   (p01 (translate (the center) :left (half (the width)) :rear (half (the length))))
   (p10 (translate (the center) :right (half (the width)) :front (half (the length))))
   (p11 (translate (the center) :right (half (the width)) :rear (half (the length))))))
  
(define-object test-rectangular-surface (rectangular-surface)
  :computed-slots ((length 20) (width 30) (height 0))
  :hidden-objects ((view :type 'base-view
                         :projection-vector (getf *standard-views* :trimetric)
                         :width (* 5 72) :length (* 5 72)
                         :objects (list self))))
