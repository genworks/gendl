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


(define-object linear-curve (%linear-curve%)

  :documentation (:description "A GDL NURBS Curve representing a straight line segment. The inputs are the same as
for l-line, namely <tt>start</tt> and <tt>end</tt> (3d points)."
                  
                  :examples "<pre>

 (in-package :surf)

 (define-object test-linear-curve (linear-curve)
   :computed-slots ((start (make-point 0 0 0)) (end (make-point 10 10 0))))
  
 (generate-sample-drawing :objects (make-object 'test-linear-curve))

</pre>")
  
  
  :input-slots
  (line-constraints
   
   (start (if (the trim-start)
              (proj-point-on-line (the trim-start)
                                  (the constraint-object start)
                                  (the constraint-object direction-vector))
            (the constraint-object start)))
   
   (end (if (the trim-end)
            (proj-point-on-line (the trim-end)
                                (the constraint-object start)
                                (the constraint-object direction-vector))
          (the constraint-object end))))

  
  :computed-slots
  ((trim-start (getf (the line-constraints) :trim-start))
   
   (trim-end (getf (the line-constraints) :trim-end))
   
   (constraints (let ((lst (the line-constraints)))
                  (dolist (key (list :trim-start :trim-end) lst)
                    (setq lst (remove-plist-key lst key)))))
   
   (keys (plist-keys (the constraints)))
   
   (exprs (plist-values (the constraints)))
   
   (constraint-type (cond ((every #'(lambda (key)
                                      (eql :through-point key)) 
                                  (the keys))
                           :2-points)
                          ((and (getf (the constraints) :through-point)
                                (getf (the constraints) :at-angle))
                           :point-angle)
                          ((and (getf (the constraints) :tangent-to)
                                (getf (the constraints) :at-angle))
                           :tangent-angle)
                          ((= (count :tangent-to (the keys)) 2)
                           :tangent-tangent)
                          ((and (getf (the constraints) :through-point)
                                (getf (the constraints) :tangent-to))
                           :through-point-tangent-to)
                          (t (error "Constrained-Line -- constraint configuration is not yet supported")))))
  
  :hidden-objects
  ((constraint-object :type (case (the constraint-type)
                              (:2-points 'line-constraints-2-points)
                              (:point-angle 'line-constraints-point-angle)
                              (:tangent-angle 'line-constraints-tangent-angle)
                              (:tangent-tangent 'line-constraints-tangent-tangent)
                              (:through-point-tangent-to 'line-contraints-through-point-tangent-to))
                      :constraints (the constraints)))
  
  :functions
  ((tangent-point (constraint-index)
               (when (member (the constraint-type) (list :tangent-angle :tangent-tangent))
                 (case constraint-index
                   (0 (the constraint-object start))
                   (1 (the constraint-object end)))))))


(define-object %linear-curve% (line curve) 
  
  :computed-slots
  ((native-curve (make-linear-curve *geometry-kernel* (the start) (the end))))

  
  :hidden-objects
  ((reverse :type 'linear-curve 
            :start (the end)
            :end (the start))))


(define-object test-linear-curve (linear-curve)
  :computed-slots ((start (make-point 0 0 0)) (end (make-point 10 10 0)))
  
  :hidden-objects
  ((view :type 'base-view
	 :pseudo-inputs (page-length page-width)
         :projection-vector (getf *standard-views* :trimetric)
         :page-width (* 5 72) :page-length (* 5 72)
         :objects (list self))))
