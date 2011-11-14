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

(define-object ruled-surface (surface)
  
  :documentation (:description "Creates a surface between two NURBS curves.:"

                  :examples "<pre>
                  
 (in-package :surf)

 (define-object test-ruled-surface (ruled-surface)
  
   :computed-slots ((display-controls (list :color :orchid-dark)))

   :hidden-objects
   ((curve-1 :type 'linear-curve
             :start (make-point -1 -1 0)
             :end (make-point -1 1 0))
   
    (curve-2 :type 'fitted-curve
             :points (list (make-point 1 -1 0) (make-point 0 0 0) 
                           (make-point .5 .5 0) (make-point 1 2 0)))))


 (generate-sample-drawing :objects (make-object 'test-ruled-surface)
                          :projection-direction :trimetric)

</pre>")
  
  :input-slots
  ("GDL Curve object. First boundary of the ruled surface." curve-1
   "GDL Curve object. Second boundary of the ruled surface." curve-2
   ("Keyword symbol, either :u or :v. The direction of parameterization of the surface between the two curves." direction :u))
  
  
  :computed-slots
  ((native-surface (make-ruled-surface *geometry-kernel* :curve-1 (the curve-1) :curve-2 (the curve-2) :direction (the direction)))))


(define-object test-ruled-surface (ruled-surface)
  
  :hidden-objects
  ((curve-1 :type 'linear-curve
            :start (make-point -1 -1 0)
            :end (make-point -1 1 0))
   
   (curve-2 :type 'fitted-curve
            :points (list (make-point 1 -1 0) (make-point 0 0 0) 
                          (make-point .5 .5 0) (make-point 1 2 0)))))


