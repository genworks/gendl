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

(defun circle-point-list (center radius face-1-vector face-2-vector &optional (kappa-factor (* radius +kappa+)))
  (list (translate-along-vector center face-1-vector radius)
        (translate-along-vector (translate-along-vector center face-1-vector radius) face-2-vector kappa-factor)
        (translate-along-vector (translate-along-vector center face-2-vector radius) face-1-vector kappa-factor)
        (translate-along-vector center face-2-vector radius)))


;;
;; FLAG -- replace face hash tables with struct since it's always the
;; same 6 fields.
;;
(defun make-face-ht (orientation)
  (let ((ht (make-hash-table :size 6)))
    (if orientation
        (setf (gethash :right ht) (array-to-3d-vector (matrix:multiply-matrix *nominal-x-array* orientation))
              (gethash :left ht) (array-to-3d-vector (matrix:multiply-matrix *nominal-x-array-r* orientation))
              (gethash :rear ht) (array-to-3d-vector (matrix:multiply-matrix *nominal-y-array* orientation))
              (gethash :front ht) (array-to-3d-vector (matrix:multiply-matrix *nominal-y-array-r* orientation))
              (gethash :top ht) (array-to-3d-vector (matrix:multiply-matrix *nominal-z-array* orientation))
              (gethash :bottom ht) (array-to-3d-vector (matrix:multiply-matrix *nominal-z-array-r* orientation)))
      (setf (gethash :right ht)    *nominal-x-array*
            (gethash :left ht)     *nominal-x-array-r*
            (gethash :rear ht)     *nominal-y-array*
            (gethash :front ht)    *nominal-y-array-r*
            (gethash :top ht)      *nominal-z-array*
            (gethash :bottom ht)   *nominal-z-array-r*)) ht))
              

(defun arc-curves (face-ht radius center start-angle end-angle)
  (if (and (= start-angle 0) (= end-angle 2pi))
      (let ((kappa-factor (* radius +kappa+)))
        (list (circle-point-list center radius (gethash :rear face-ht) (gethash :right face-ht) kappa-factor)
              (circle-point-list center radius (gethash :right face-ht) (gethash :front face-ht) kappa-factor)
              (circle-point-list center radius (gethash :front face-ht) (gethash :left face-ht) kappa-factor)
              (circle-point-list center radius (gethash :left face-ht) (gethash :rear face-ht) kappa-factor)))
    (let* ((start-to-end-angle (- end-angle start-angle))
           (number-of-sub-arcs (ceiling (/ start-to-end-angle pi/2)))
           (angle-per-sub-arc (/ start-to-end-angle number-of-sub-arcs))
           result)
      (dotimes (n number-of-sub-arcs (nreverse result))
        (push (sub-arc radius 
                       (+ start-angle (* n angle-per-sub-arc))
                       (+ start-angle (* (1+ n) angle-per-sub-arc))
                       center 
                       (gethash :right face-ht)
                       (gethash :rear face-ht)
                       (gethash :front face-ht)
                       (gethash :top face-ht)) result)))))



