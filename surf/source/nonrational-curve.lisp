;;
;; Copyright 2002-2011, 2012 Genworks International
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


(define-object non-rational-curve (curve)

  :documentation (:description 
		  "This object accepts a rational curve and approximates it with a non-rational curve."
		  :author "Dave Cooper, Genworks International")


  :input-slots
  ("GDL Curve object. Presumably this is a Rational curve (else this object will do nothing)." curve-in
   ("Number. The amount by which to divide the total-length of the curve-in to compute the default tolerance. Default is 1000." 
    tolerance-divisor 1000)
   ("Number. The tolerance to use for non-rational approximation of a rational curve-in. 
Defaults to the curve-in's total length divided by the tolerance-divisor." 
    tolerance (div (the curve-in total-length) (the tolerance-divisor)))
    
   ("Boolean. Determines whether to try to maintain tangents at the ends. Defaults to t." maintain-end-tangents? t)
    
   ("Integer. Determines the degree of the non-rational curve. Defaults to 3." non-rational-degree 3)

   ("Keyword symbol, one of :uniform, :chord-length, :centripetal, or :inherited. The default is :inherited." 
    parameterization :inherited))


  :computed-slots
  ((native-curve (if (the curve-in rational?) (rational-to-nonrational *geometry-kernel* (the curve-in)
								       :tolerance (the tolerance)
								       :maintain-end-tangents? (the maintain-end-tangents?)
								       :nonrational-degree (the non-rational-degree)
								       :parameterization (the parameterization))))))