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

(define-object decomposed-curves (base-object)
  
  :documentation (:description "Given an input curve, creates a sequence of curve segments that do not contain 
knots with degree-fold mutiplicity, i.e. each piece is at least C^ continuous."
                  :examples "<pre>
  
  (in-package :surf)  

  (define-object test-decomposed-curves (decomposed-curves)
  :computed-slots
  ((curve-in (the composed-curve)))
  
  :objects ((composed-curve :type 'test-composed-curve)))

 (generate-sample-drawing :object-roots (make-object 'test-decomposed-curves))

 </pre>")
  
  :input-slots
  ("GDL Curve. Curve (presumably multi-segment) to be decomposed." curve-in
   
   (tolerance-for-native-beziers (the curve-in tolerance-for-native-beziers))
   (tolerance (the curve-in tolerance))
   )
  
  :computed-slots
  ((%decomposed-native-curves% (decomposed-curves *geometry-kernel* (the curve-in native-curve))))
  
  :objects
  (("Sequence of GDL curve objects. The resulting segment curves after decomposition."
    curves 
    :type 'curve
    :sequence (:size (length (the %decomposed-native-curves%)))
    :tolerance (the tolerance)
    :tolerance-for-native-beziers (the tolerance-for-native-beziers)
    :native-curve (nth (the-child index) (the %decomposed-native-curves%)))))


;; (define-object test-decomposed-curves (decomposed-curves)
;;  :computed-slots
;;  ((curve-in (the composed-curve)))
;;  
;;  :objects ((composed-curve :type 'test-composed-curve)))
