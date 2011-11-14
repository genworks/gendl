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



  




(defun chain-nurbs-curves 
    (curves &optional (start (when curves (the-object (first curves) start)))
                      (closed? t)
                      (tolerance 0.01))
  (when curves
    (let ((start-match (position start curves 
                                 :test #'(lambda(p1 p2) 
                                           (coincident-point? p1 p2 :tolerance tolerance))
                                 :key #'(lambda(obj) (the-object obj start))))
          (end-match (position start curves 
                               :test #'(lambda(p1 p2) 
                                         (coincident-point? p1 p2 :tolerance tolerance)) 
                               :key #'(lambda(obj) (the-object obj end)))))

      (cond ((and (not closed?) start-match end-match)
             (error "start matches both start and end of curve in chain-curves"))
            ((and (null start-match) (null end-match))
             (print-variables start start-match end-match)
             (setq gdl-user::*curves* curves)
             (error "curves did not chain in chain-curves"))
            (start-match
             (cons (nth start-match curves) 
                   (chain-nurbs-curves (append (subseq curves 0 start-match)
                                         (subseq curves (1+ start-match)))
                                       (the-object (nth start-match curves) end) closed?)))
            (end-match (cons (the-object (nth end-match curves) reverse) 
                             (chain-nurbs-curves (append (subseq curves 0 end-match)
                                                         (subseq curves (1+ end-match)))
                                                 (the-object (nth end-match curves) start) 
                                                 closed?)))))))



(defmethod evaluate-object ((category (eql :gdl-geometry-kernel-instance)) args)
  (let ((self (apply #'make-object 'native-reader args)))
    ;;
    ;; FLAG -- pull individual smlib object from the reader
    ;;
    (cond ((not (zerop (the curves number-of-elements)))
           (the (curves 0)))
          ((not (zerop (the surfaces number-of-elements)))
           (the (surfaces 0)))
          ((not (zerop (the breps number-of-elements)))
           (the (breps 0))))))


#+nil
(define-object basis-copy (b-spline-surface)
  :input-slots
  ("GDL Surface. Surface to provide the source basis data." built-from)
  
  :computed-slots
  ((b-spline-data (multiple-value-list (the built-from b-spline-data)))
   (control-points (first (the b-spline-data)))
   (weights (second (the b-spline-data)))
   (u-knot-vector (third (the b-spline-data)))
   (v-knot-vector (fourth (the b-spline-data)))
   (u-degree (fifth (the b-spline-data)))
   (v-degree (sixth (the b-spline-data)))))


#+nil
(define-object ordered-curves (curve)


  :input-slots
  (curves 
   (start (when (the curves)
            (the-object (first (the curves)) start)))
   (closed? nil)
   (tolerance 0.01))
  
  
  ;;:computed-slots 
  ((ordered-list (cons (the first-curve)
                       (unless (typep (the rest-chain) 'null-part)
                         (the rest-chain ordered-list)))) 
   
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
  

  ;;:objects
  ((rest-chain :type (if (the rest-curves) 'ordered-curves 'null-part)
               :curves (the rest-curves)
               :start (the first-curve end)
               :tolerance (the tolerance))))


#+nil
(define-object curve-chain (curve)


  ;;:input-slots
  #+nil
  (curves 
   (start (when (the curves)
            (the-object (first (the curves)) start)))
   (closed? nil)
   (tolerance 0.01))
  

  ;;:computed-slots 
  #+nil
  ((ordered-list (cons (the first-curve)
                       (unless (typep (the rest-chain) 'null-part)
                         (the rest-chain ordered-list)))) 
   
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
  

  ;;:objects
  #+nil
  ((rest-chain :type (if (the rest-curves) 'curve-chain 'null-part)
               :curves (the rest-curves)
               :start (the first-curve end)
               :tolerance (the tolerance))))






             
            
  
  
  
  
