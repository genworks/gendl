;;
;; Copyright 2002, 2009 Genworks International and Genworks BV 
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

(define-object composed-curve (curve)
  
  :documentation (:description "Creates a single NURBS curve from a list (ordered or unordered) NURBS curves. 
If the result is more than one curve, this object will throw an error and you should use <tt>composed-curves</tt>
instead."
  
  :examples "<pre>
 (in-package :surf)

 (define-object test-composed-curve (composed-curve)
  :computed-slots
  ((curves (the filleted-polyline-curves ordered-curves)))
  
  :hidden-objects
  ((filleted-polyline-curves :type 'test-global-filleted-polyline-curves)))

 (generate-sample-drawing :objects (the-object (make-object 'test-composed-curve))
                          :projection-direction (getf *standard-views* :trimetric))

</pre>")
  
  
  
  :input-slots
  ("List of GDL curve objects. These are the curves to be composed into a single curve."
   curves
   
   ("Number. Distance two curve endpoints can be apart and be considered coincident, the composite will be built
without doing anything to the endpoints. Default is 0.01. Note: This input-slot must be non-zero."
    coincident-point-tolerance 0.01)
   
   ("Number. Distance two curve endpoints can be apart and have a linear curve segment automatically added between the points. Default is 0.1."
    distance-to-create-line 0.1)
   
   (native-curve-iw (if (null (the curves))
                        (error "Trying to construct a composed curve from an empty list of curves. 
Root path is ~s...~%" 
                               (the root-path))
                      (let ((result (build-composites-from-curves *geometry-kernel* (the curves)
                                                                  :distance-to-create-line (the distance-to-create-line)
                                                                  :same-point-tolerance (the coincident-point-tolerance))))
                        (if (consp (rest result))
                            (error "Composing resulted in ~a curves. Please use composed-curves instead.~%" (length result))
                          (first result)))))))

(define-object composed-curves (base-object)

  :documentation (:description "Creates multiple NURBS curves by composing a list (ordered or unordered) NURBS curves. 
If the result is expected to be a single curve, you may wish to use <tt>composed-curve</tt> instead.")
  
  :input-slots
  ("List of GDL curve objects. These are the curves to be composed into a single curve."
   curves-in
   
   ("Number. Distance two curve endpoints can be apart and be considered coincident, the composite will be built
without doing anything to the endpoints. Default is 0.01. Note: This input-slot must be non-zero."
    coincident-point-tolerance 0.01)
   
   ("Number. Distance two curve endpoints can be apart and have a linear curve segment automatically added between the points. Default is 0.1."
    distance-to-create-line 0.1))
  
  :computed-slots
  ((native-curves-iw (build-composites-from-curves 
                      *geometry-kernel* 
                      (the curves-in)
                      :distance-to-create-line (the distance-to-create-line)
                      :same-point-tolerance (the coincident-point-tolerance))))

  :objects
  (("Sequence of GDL Curve Objects. The curves resulting from composition."
    curves :type 'curve
    :sequence (:size (length (the native-curves-iw)))
    :native-curve-iw (nth (the-child index) (the native-curves-iw)))))
  

;;(define-object test-composed-curve (composed-curve)
;;  :computed-slots
;;  ((curves (the filleted-polyline-curves ordered-curves)))
;;  
;;  :hidden-objects
;;  ((filleted-polyline-curves :type 'test-global-filleted-polyline-curves)))
