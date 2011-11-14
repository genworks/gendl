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

(define-object arc-curve (curve arc)

  :documentation (:description "An arc represented exactly as a quadratic NURBS curve. Inputs are the same as for arc. 
Messages are the union of those for arc and those for curve."
                  
                  :examples "<pre>

  (in-package :surf)

  (define-object test-arc-curve (arc-curve)
    :computed-slots
    ((center (make-point 0 0 0)) (radius 10) (start-angle 0) (end-angle 2pi)))

  (generate-sample-drawing :objects (make-object 'test-arc-curve))


 (define-object test-arc-curve2 (arc-curve)
    :computed-slots
    ((center (make-point 0 0 0)) (radius 5) (start-angle (* 0.25 pi)) (end-angle pi)))
  
  (generate-sample-drawing :objects (make-object 'test-arc-curve))

</pre>")

  :computed-slots
  ((native-curve (make-arc-curve *geometry-kernel* (the center) (the orientation) 
                                 (the radius) (the start-angle-normalized) (the end-angle-normalized)))))


;; (define-object test-arc-curve (arc-curve)
;;  :computed-slots
;;  ((center (make-point 0 0 0)) (radius 10) (start-angle 0) (end-angle 2pi))
  
;;  :hidden-objects ((view :type 'base-view
;;                       :objects (list self)
;;                       :page-width (* 5 72) :page-length (* 5 72))))


;; (define-object test-arc-curve-2 (arc-curve)
;;  :computed-slots
;;  ((center (make-point 0 0 0)) (radius 10) (start-angle 0) (end-angle pi))
;;  
;;  :hidden-objects ((view :type 'base-view
;;                       :objects (list self)
;;                       :page-width (* 5 72) :page-length (* 5 72))))
