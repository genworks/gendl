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

(define-object elliptical-curve (curve ellipse)

  :documentation (:description "An ellipse represented exactly as a quintic NURBS curve. Inputs are the same as for ellipse. 
Messages are the union of those for ellipse and those for curve."
                  
                  :examples "<pre>

  (in-package :surf)

  (define-object test-elliptical-curve (elliptical-curve)
    :computed-slots
    ((center (make-point 0 0 0)) 
     (major-axis-length 10) 
     (minor-axis-length 5) 
     (start-angle 0) 
     (end-angle 2pi)))
  
  (generate-sample-drawing :objects (make-object 'test-elliptical-curve))


</pre>")

  :computed-slots
  ((native-curve (make-elliptical-curve *geometry-kernel* (the center) (the orientation) 
                                        (the minor-axis-length) (the major-axis-length) 
                                        (the start-angle-normalized) (the end-angle-normalized)))))



