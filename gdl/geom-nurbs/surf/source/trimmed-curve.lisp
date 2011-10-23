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

(define-object trimmed-curve (curve)
  
  :documentation (:description "Creates a curve based on an existing curve but possibly with new start and end parameters (<tt>u1</tt> and <tt>u2</tt>)."
                  :examples "<pre>
  
 (in-package :surf)

 (define-object test-trimmed-curve ()

   :objects
   ((b-spline-curve :type 'b-spline-curve
                     :control-points (list (make-point 0 0 0)
                                           (make-point 2 3.0 0.0) 
                                           (make-point 4 2.0 0.0) 
                                           (make-point 5 0.0 0.0) 
                                           (make-point 4 -2.0 0.0) 
                                           (make-point 2 -3.0 0.0) 
                                           (make-point 0 0 0)))
    
    (trimmed-curve :type 'trimmed-curve
                   :built-from (the b-spline-curve)
                   :u1 0.2
                   :u2 0.8
                   :display-controls (list :color :red :line-thickness 1.5))))

 (generate-sample-drawing :object-roots (make-object 'test-trimmed-curve))

   </pre>" )
  
  :input-slots
  ("GDL Curve Object. The underlying curve from which to build this curve."
   built-from
   
   
   
   ("Number. Specified start parameter. Defaults to the <tt>u1</tt> of the built-from."
    u1 (the built-from u1))
   
   

   
   
   ("Number. Specified end parameter. Defaults to the <tt>u2</tt> of the built-from."
    u2 (the built-from u2))
   
   

   
   
   )
  
  
  :computed-slots
  ((native-curve-iw (cond ((null (the built-from))
                           (error "In trimmed-curve, you must specify a valid curve for the built-from."))
                          ((<= (the %u2%) (the %u1%))
                           (error "In ~s ~s (built-from ~s), u2 is not greater than u1. Cannot Trim." self (the strings-for-display) (the built-from)))
                          (t (let ((native-curve-iw 
                                    (iw-copy-curve *geometry-kernel* (the built-from native-curve-iw)
                                                   :finalize? t
                                                   )))
                               (let ((u-min (the built-from u-min))
                                     (u-max (the built-from u-max))
                                     (u1 (min (the %u1%) (the %u2%)))
                                     (u2 (max (the %u1%) (the %u2%))))
                                 ;;
                                 ;; FLAG -- this error should never occur now.
                                 ;;
                                 (when (or (< u1 u-min) (> u2 u-max))
                                   (error "Trim parameters out of range"))
                        
                                 (iw-trim-curve *geometry-kernel* native-curve-iw u1 u2)
                                 native-curve-iw)))))
   
   
   (%u1% (progn
          (if (< (the u1) (the built-from u-min))
              (progn (warn "Requested trim parameter ~a is less than the u-min of the original curve ~a. Pinning to the u-min value.~%"
                           (the u1) (the built-from u-min))
                     (the built-from u-min))
            (the u1))))
   
   
   (%u2% (progn
          (if (> (the u2) (the built-from u-max))
              (progn (warn "Requested trim parameter ~a is greater than the u-max of the original curve ~a. Pinning to the u-max value.~%"
                           (the u2) (the built-from u-max))
                     (the built-from u-max))
            (the u2))))   
   
   ("GDL Curve. The original untrimmed curve, same as the <tt>built-from.</tt>"
    basis (the built-from))))


 (define-object test-trimmed-curve ()

   :objects
   ((b-spline-curve :type 'b-spline-curve
                    :control-points (list (make-point 0 0 0)
                                          (make-point 2 3.0 0.0) 
                                          (make-point 4 2.0 0.0) 
                                          (make-point 5 0.0 0.0) 
                                          (make-point 4 -2.0 0.0) 
                                          (make-point 2 -3.0 0.0) 
                                          (make-point 0 0 0)))
    
    (trimmed-curve :type 'trimmed-curve
                   :built-from (the b-spline-curve)
                   :u1 0.2
                   :u2 0.8
                   :display-controls (list :color :red :line-thickness 1.5))))
