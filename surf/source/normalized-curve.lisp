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

(define-object normalized-curve (curve)
  
  :documentation (:description "This object creates a new curve from an exiting 
curve by  reasigning the lowest and highest parameter value of the underlying 
mathematical definition of the curve. This is a precise method, it does not 
change the curve geometry.")
                   
  :input-slots 
  (
   ("GDL NURBS Curve. The curve to be normalized." curve-in)
   
   ("The lowest parameter value of the underlying mathematical definition 
for this parametric curve"   
       u-min 0.0)
   
   ("The highest parameter value of the underlying mathematical definition 
for this parametric curve"  
       u-max 1.0)
   
   (tolerance 0.0001))
  
  :computed-slots ((native-curve (the curve-knots-interval-scaled native-curve )))
  
  :hidden-objects
  (
   ;;;
   ;;;it has to be made a new object for "curve-knots-remove" it is not used in this one.
   ;;;
   
   (curve-knots-remove :type 'curve
                       :native-curve (tooccl  *geometry-kernel* 
                                              (the curve-in native-curve)
                                              :tolerance (the tolerance)))
   
   (curve-knots-interval-scaled 
    :type 'curve
    :native-curve (curve-knot-scale *geometry-kernel* 
                                    ;;(the curve-knots-remove copy native-curve) 
                                    (the curve-in copy native-curve) 
                                    :u-min (the u-min)
                                    :u-max (the u-max)))))

