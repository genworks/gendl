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

(in-package :geom-base)


(defun get-zeroes-in-interval 
    (coefficient-array min max 
     &key roots (degree (- (array-dimension coefficient-array 0) 1))
          (accuracy (* 10.0 double-float-epsilon)) (maximum-number-of-iterations 30))
  (let ((function (make-polynomial-function coefficient-array))
        (function-prime (make-polynomial-function (poly-prime coefficient-array))))
    
    (if (= degree 1)
        (let ((root (newton-raphson function function-prime min max
                                    :accuracy accuracy
                                    :maximum-number-of-iterations maximum-number-of-iterations)))
          (when root (push root roots)))
      (let ((droots (get-zeroes-in-interval (poly-prime coefficient-array) min max 
                                            :roots roots :degree (1- degree)
                                            :accuracy accuracy 
                                            :maximum-number-of-iterations maximum-number-of-iterations)))
        (if droots
            (progn
              (let ((root (newton-raphson function function-prime min (first droots)
                                          :accuracy accuracy
                                          :maximum-number-of-iterations maximum-number-of-iterations)))
                (when root (push root roots)))
              (dotimes (i (- (length droots) 2))
                (let ((root (newton-raphson function function-prime (nth i droots) (nth (1+ i) droots)
                                            :accuracy accuracy
                                            :maximum-number-of-iterations maximum-number-of-iterations)))
                  (when root (push root roots))))
              (let ((root (newton-raphson function function-prime (nth (1- (length droots)) droots) max
                                          :accuracy accuracy
                                          :maximum-number-of-iterations maximum-number-of-iterations)))
                (when root (push root roots))))
          (let ((root (newton-raphson function function-prime min max
                                      :accuracy accuracy
                                      :maximum-number-of-iterations maximum-number-of-iterations)))
            (when root (push root roots))))))) roots)


(defun make-polynomial-function (coefficient-array)
  #'(lambda(num) (evaluate-polynomial coefficient-array num)))
                     
;;
;; FLAG -- look at making this tail-recursive.
;;
(defun evaluate-polynomial (coefficient-array num)
  (let ((degree (1- (array-dimension coefficient-array 0))))
    (if (zerop degree) 
        (elt coefficient-array 0)
      (+ (evaluate-polynomial (subseq coefficient-array 1) num)
         (* (elt coefficient-array 0) (expt num degree))))))
        

(defun newton-raphson (function function-prime min max
                       &key (accuracy (* 10.0 double-float-epsilon))
                            (maximum-number-of-iterations 30))
  (assert (<= min max))
  (let ((x (half (+ min max))) delta-x accuracy-denominator)
    (dotimes (j maximum-number-of-iterations)
      (let ((f-result (funcall function x))
            (f-prime-result (funcall function-prime x)))
        (when (zerop f-prime-result) (return nil))
        (setq delta-x (/ f-result f-prime-result)
              accuracy-denominator (+ (abs x) (abs (decf x delta-x))))
        (when (< x min) (setq x min))
        (when (> x max) (setq x max))
        (when (and (not (zerop accuracy-denominator))
                   (< (/ (abs delta-x) accuracy-denominator) accuracy))
          (return (values x (1+ j))))))))

          
(defun poly-prime (coefficient-array)
  (let ((degree (array-dimension coefficient-array 0)))
    (make-array (list (1- degree))
                :initial-contents (let ((power degree))
                                    (map 'list #'(lambda(coefficient)
                                                   (* coefficient (decf power)))
                                         (subseq coefficient-array 0 (1- degree)))))))

