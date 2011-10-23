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

(defun line-intersection (n)
  (let ((x1 (first   n))
        (y1 (second  n)) 
        (x2 (third   n))
        (y2 (fourth  n))
        (x3 (fifth   n))  
        (y3 (sixth   n)) 
        (x4 (seventh n))
        (y4 (eighth  n)))
    (let ((denom (- (* (- y4 y3) (- x2 x1)) (* (- x4 x3) (- y2 y1))))
          (ua-num (- (* (- x4 x3) (- y1 y3)) (* (- y4 y3) (- x1 x3))))
          (ub-num (- (* (- x2 x1) (- y1 y3)) (* (- y2 y1) (- x1 x3))))
          (dif-x-1 (- x1 x2))
          (dif-y-1 (- y1 y2))
          (dif-x-2 (- x3 x4))
          (dif-y-2 (- y3 y4))
          )
      (cond 
       ;; If the line is a point this can happen in 2D projection of 3D line  
       ((and (zerop dif-x-1) (zerop dif-y-1)) nil)
       ((and (zerop dif-x-2) (zerop dif-y-2)) nil)
       ;; If the denominator and numerator for the equations for ua and 
       ;; ub are 0 then the two lines are coincident.
       ;;((and (zerop denom) (zerop ua-num) (zerop ub-num)) (list x1 y1));;;?????? this condition is not 100% true ?????
       ;; If the denominator for the equations for ua and ub is 0 then 
       ;; the two lines are parallel.
       ((zerop denom) nil)
       (t 
        (let ((ua (/ ua-num denom))
              (ub (/ ub-num denom)))
          (if (and (<= 0 ua 1) (<= 0 ub 1))
              (list (+ x1 (* ua (- x2 x1)))
                    (+ y1 (* ua (- y2 y1))))
            nil)))))))

(defun get-box-corners (bounding-box)
  (let ((min-y-1 (list (get-x (first  bounding-box))
                       (get-y (first  bounding-box))
                       (get-z (first  bounding-box))))
        (max-y-3 (list (get-x (second bounding-box))
                       (get-y (second bounding-box))
                       (get-z (second bounding-box)))))
    (let ((min-y-2 (list (first  max-y-3)
                         (second min-y-1)
                         (third  min-y-1)))
          (min-y-3 (list (first  max-y-3)
                         (second min-y-1)
                         (third  max-y-3)))     
          (min-y-4 (list (first  min-y-1)
                         (second min-y-1)
                         (third  max-y-3)))
          (max-y-1 (list (first  min-y-1)
                         (second max-y-3)
                         (third  min-y-1)))
          (max-y-2 (list (first  max-y-3)
                         (second max-y-3)
                         (third  min-y-1)))
          (max-y-4 (list (first  min-y-1)
                         (second max-y-3)
                         (third  max-y-3))))
      (list (list min-y-1 min-y-2 min-y-3 min-y-4) (list max-y-1 max-y-2 max-y-3 max-y-4)))))

(defun get-polygon-edges (points)
  ;;This converts the points, which specifies a polygon, in a list of lines defined by (X1 Y1) (X2 Y2)"
  (let ((start (car (last points))))
    (loop for point in points collect
          (list  start point)
        do (setf start point ))))

(defun get-box-edges (bounding-box)
  (append 
   (get-polygon-edges (first (get-box-corners bounding-box))) 
   (mapcan #'(lambda (min max)(list (list min max)))
           (first  (get-box-corners bounding-box))
           (second (get-box-corners bounding-box))) 
   (get-polygon-edges (second (get-box-corners bounding-box)))))

(defun get-xy-boxs-intersection (bounding-box-1 bounding-box-2)
  (remove nil 
          (mapcar #'(lambda (line) 
                      (remove nil 
                              (loop for i in 
                                    (get-box-edges bounding-box-1) collect 
                                    (line-intersection
                                     (list (first (first line ))(second (first line ))(first (second line ))(second (second line ))
                                           (first (first i ))(second (first i))(first (second i))(second (second i)))))))
                  (get-box-edges bounding-box-2))))

(defun get-xz-boxs-intersection (bounding-box-1 bounding-box-2)
  (remove nil 
          (mapcar #'(lambda (line) 
                      (remove nil 
                              (loop for i in 
                                    (get-box-edges bounding-box-1) collect 
                                    (line-intersection
                                     (list (first (first line ))(third  (first line ))(first (second line ))(third  (second line ))
                                           (first (first i ))(third  (first i))(first (second i))(third  (second i)))))))
                  (get-box-edges bounding-box-2))))

(defun get-yz-boxs-intersection (bounding-box-1 bounding-box-2)
  (remove nil 
          (mapcar #'(lambda (line) 
                      (remove nil 
                              (loop for i in 
                                    (get-box-edges bounding-box-1) collect 
                                    (line-intersection
                                     (list (second (first line ))(third  (first line ))(second (second line ))(third  (second line ))
                                           (second (first i ))(third  (first i))(second (second i))(third  (second i)))))))
                  (get-box-edges bounding-box-2))))

(defun box-intersection (bounding-box-1 bounding-box-2)
  (let (( xyz-intersection (length (remove nil (list (get-xy-boxs-intersection bounding-box-1 bounding-box-2)
                    (get-xz-boxs-intersection bounding-box-1 bounding-box-2)
                    (get-yz-boxs-intersection bounding-box-1 bounding-box-2))))))
  (if (cond (( = 3 xyz-intersection) t)) t nil)))


(define-object box-intersection-test (base-object)
  :input-slots
  ((x-factor 0 :settable) 
   (y-factor 0 :settable) 
   (z-factor 0 :settable))
  
  :computed-slots ()
  
  :objects
  ((curve-1 :type 'b-spline-curve 
            :hidden? t
            :degree 2
            :control-points (list #(1.0 0.0 0.0)
                                  #(1.0 0.5 0.5)
                                  #(1.0 1.0 0.0)))
   (curve-2 :type 'linear-curve  
            :hidden? t
            :start  #(0.0 0.0 0.0)
            :end    #(0.0 1.0 0.0))

   (curve-11 :type 'b-spline-curve 
             :hidden? t
             :degree 2
             :control-points (list (make-point (+ (the x-factor) 0.0) 
                                               (+ (the y-factor) 0.0)
                                               (+ (the z-factor) 0.0))
                                   (make-point (+ (the x-factor) 0.0) 
                                               (+ (the y-factor) 0.5)
                                               (+ (the z-factor) 0.5))
                                   (make-point (+ (the x-factor) 0.0) 
                                               (+ (the y-factor) 1.0)
                                               (+ (the z-factor) 0.0)))) 

   (curve-22 :type 'linear-curve  
             :hidden? t
             :start (make-point (+ (the x-factor) 1.0) 
                                (+ (the y-factor) 0.0)
                                (+ (the z-factor) 0.0))
             :end   (make-point (+ (the x-factor) 1.0) 
                                (+ (the y-factor) 1.0)
                                (+ (the z-factor) 0.0)))
   
   (surface-1 :type 'ruled-surface
              :display-controls (list :color :red)
              :curve-1 (the curve-1)
              :curve-2 (the curve-2))
             
   (surface-2 :type 'ruled-surface
              :display-controls (list :color :green)
              :curve-1 (the curve-11)
              :curve-2 (the curve-22))
   
   (message :type 'general-note
            :start (make-point 3 0 0)
            :character-size 0.1
            :display-controls (list :color :yellow)
            :strings (format nil "box-intersection-message  ~a" (box-intersection 
                                                                 (the surface-1  bounding-box)
                                                                 (the surface-2  bounding-box))))

   (test-brep-intersect :type 'brep-intersect
                        :brep (the surface-1 brep)
                        :other-brep  (the surface-1 brep)) 
   ))

