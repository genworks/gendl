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

(define-object global-filleted-polyline-curves (global-filleted-polyline)
  
  :documentation 

  (:description "Produces a list of linear-curves and arc-curves which represent the straight sections
and fillets of a global-filleted-polyline. Note also global-filleted-polyline-curve, which composes the segments together 
into a single curve."
                  
                  :examples "<pre>

 (in-package :gdl-user)

 (define-object test-global-filleted-polyline-curves (global-filleted-polyline-curves)
                    
  :computed-slots
  ((default-radius 5)
   (vertex-list (list (make-point 0 0 0)
                      (make-point 10 10 0)
                      (make-point 30 10 0)
                      (make-point 40 0 0)
                      (make-point 30 -10 0)
                      (make-point 10 -10 0)
                      (make-point 0 0 0))))

  :hidden-objects
  ((points :type 'point
           :sequence (:size (length (rest (the vertex-list))))
           :center (nth (the-child index) (rest (the vertex-list))))

   (view :type 'base-view
         :page-width (* 5 72) :page-height (* 5 72)
         :objects (cons self (list-elements (the points))))))

 (generate-sample-drawing :objects (the-object (make-object 'test-global-filleted-polyline-curves))
                          :projection-direction (getf *standard-views* :trimetric))

</pre>")
                  
  :computed-slots
  ((%lines-to-draw% (append-elements (the straight-curves) (the-element %lines-to-draw%)))

   (%curves-to-draw% (append  (append-elements (the fillet-curves) (the-element %curves-to-draw%))))
    
   ("List of GDL NURBS curve objects. The curve segments in the right order for chaining together."
    ordered-curves (let ((tail (nreverse (mapcan #'list (list-elements (the straight-curves)) (list-elements (the fillet-curves))))))
                     (if (< (the fillet-curves number-of-elements) (the straight-curves number-of-elements))
                         (cons (the straight-curves last) tail) tail)))

   (fillet-curve-types (mapcar #'(lambda(fillet-index)
				   (if (typep (the (fillets fillet-index)) 'geom-base::fillet)
				       'arc-curve 'null-part))
			       (list-of-numbers 0 (1- (the fillets number-of-elements))))))
  
  :hidden-objects
  (("GDL Sequence of GDL NURBS curve objects. The arc-curves representing the fillets."
    fillet-curves 
    :type (:sequence (the fillet-curve-types))
    :sequence (:size (the fillets number-of-elements))
    :pseudo-inputs (fillet)
    :fillet (the (fillets (the-child :index)))
    :radius (the-child fillet radius)
    :start-angle (the-child fillet  start-angle-normalized)
    :end-angle (the-child fillet end-angle-normalized)
    :center (the-child fillet center)
    :orientation (the-child fillet orientation))
   
   ("GDL Sequence of GDL NURBS curve objects. The linear-curves representing the straights."
    straight-curves :type 'linear-curve
    :sequence (:size (length (the straights)))
    ;;
    ;; FLAG clean up use of nth
    ;;
    :pseudo-inputs (pair)
    :pair (nth (the-child index) (the straights))
    :start (second (the-child pair))
    :end (first (the-child pair)))))


(define-object global-filleted-polyline-curve
    (global-filleted-polyline-curves composed-curve)
  
  :documentation (:description "Provides a singular composed curve made 
from a global-filleted-polyline-curves object")
  
  :computed-slots
  ((curves (remove-if #'(lambda(item) (typep item 'null-part)) (the ordered-curves)))))

  




