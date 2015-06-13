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


(in-package :surf)

(define-object circular-curve (arc-curve)
  :documentation
  (:description 
   "An full circule represented exactly as a quadratic NURBS
    curve. Inputs and messages are the same as for arc-curve."

                  
   :examples "<pre>

  (in-package :surf)

  (define-object test-circular-curve (circular-curve)
    :computed-slots
    ((center (make-point 0 0 0)) (radius 10)))

  (generate-sample-drawing :objects (make-object 'test-circular-curve))

</pre>"))



;;
;; FLAG move to main package.lisp. 
;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'circular-curve :surf))

;;
;; FLAG -- add Lift regression test in gendl/regression/source/
;;

