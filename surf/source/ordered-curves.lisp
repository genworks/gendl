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


(defun ordered-curves (curves &key (start (when curves 
                                            (the-object (first curves) start)))
                                   (closed? nil)
                                   (tolerance *3d-approximation-tolerance-default*)
                                   (distance-to-create-line 0.001))
  (the-object (make-object 'ordered-curves 
                           :curves curves
                           :start start
                           :closed? closed?
                           :tolerance tolerance
                           :distance-to-create-line distance-to-create-line)
              ordered-list))

              
                                   

(define-object ordered-curves (curve)


  :input-slots
  (curves 
   (start (when (the curves)
            (the-object (first (the curves)) start)))
   (closed? nil)
   (tolerance 0.01)
   
   (distance-to-create-line 0.001))   
  

  
  :computed-slots 
  ((ordered-list (cons (the built-from)
                       (let ((rest (unless (typep (the rest-chain) 'null-part)
                                     (the rest-chain ordered-list))))
                         (if (typep (the connector) 'null-part)
                             rest
                           (cons (the connector) rest)))))

   
   (built-from (first (ensure-list (the first-curve))))

   (first-curve (dolist (curve (the curves))
                  (when (coincident-point? (the start) (the-object curve start)
                                           :tolerance (the tolerance))
                    (return curve))
                  (when (coincident-point? (the start) (the-object curve end)
                                           :tolerance (the tolerance))
                    (return (list (the-object curve reverse) curve)))))
   
   (rest-curves (cond ((consp (the first-curve)) (remove (second (the first-curve)) (the curves)))
                      ((the first-curve) (remove (the first-curve)(the curves)))
                      (t (error "start point not found among curves in curve-chainer")))))
  
  :objects
  ((connector :type (cond ((and (the rest-curves) 
                                (coincident-point? (the first-curve end) (the rest-chain built-from start)
                                                   :tolerance (the distance-to-create-line)))
                           'null-part)
                          ((null (the rest-curves)) 'null-part)
                          (t 'linear-curve))
              :start (the built-from end)
              :end (when (the rest-curves) (the rest-chain built-from start)))
                        
   
   (rest-chain :type (if (the rest-curves) 'ordered-curves 'null-part)
               :curves (the rest-curves)
               :start (the first-curve end)
               :tolerance (the tolerance))))

