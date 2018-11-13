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

(in-package :geom-base)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro make-vector (&rest args)
    "0D, 1D, 2D, 3D, or 4D Vector. (Internally this is the same as a Point) 
Returns a vector of double-floats from up to 4 numbers."
    (let ((array (gensym)))
      (ecase (length args)
        (0 `(make-array 0))
        (1 `(let ((,array (make-array 1)))
              (setf (svref ,array 0) (coerce ,(first args) 'double-float)) ,array))
        (2 `(let ((,array (make-array 2)))
              (setf (svref ,array 0) (coerce ,(first args) 'double-float))
              (setf (svref ,array 1) (coerce ,(second args) 'double-float)) ,array))
        (3 `(let ((,array (make-array 3)))
              (setf (svref ,array 0) (coerce ,(first args) 'double-float))
              (setf (svref ,array 1) (coerce ,(second args) 'double-float))
              (setf (svref ,array 2) (coerce ,(third args) 'double-float)) ,array))
        (4 `(let ((,array (make-array 4)))
              (setf (svref ,array 0) (coerce ,(first args) 'double-float))
              (setf (svref ,array 1) (coerce ,(second args) 'double-float))
              (setf (svref ,array 2) (coerce ,(third args) 'double-float))
              (setf (svref ,array 3) (coerce ,(fourth args) 'double-float)) ,array)))))

  (defmacro make-point (&rest args)
    "3D Point. (Internally this is the same as a 3D Vector) Returns a vector of double-floats from up to 4 numbers."
    `(make-vector ,@args)))



(defvar *debug?* nil)

(glisp:define-constant *nominal-x-vector* (make-vector 1 0 0))
(glisp:define-constant *nominal-y-vector* (make-vector 0 1 0))
(glisp:define-constant *nominal-z-vector* (make-vector 0 0 1))

(glisp:define-constant *nominal-x-vector-r* (make-vector -1 0 0))
(glisp:define-constant *nominal-y-vector-r* (make-vector 0 -1 0))
(glisp:define-constant *nominal-z-vector-r* (make-vector 0 0 -1))


(glisp:define-constant *nominal-x-array* #2a((1.0d0 0.0d0 0.0d0)))
(glisp:define-constant *nominal-y-array* #2a((0.0d0 1.0d0 0.0d0)))
(glisp:define-constant *nominal-z-array* #2a((0.0d0 0.0d0 1.0d0)))


(glisp:define-constant *nominal-x-array-r* #2a((-1.0d0 0.0d0 0.0d0)))
(glisp:define-constant *nominal-y-array-r* #2a((0.0d0 -1.0d0 0.0d0)))
(glisp:define-constant *nominal-z-array-r* #2a((0.0d0 0.0d0 -1.0d0)))

(glisp:define-constant *trimetric-normal-vector* (make-vector 0.8342367128320977 -0.4377640254359154 0.3352786378480434))
    ;;(make-vector 0.83425635 -0.43777433 0.33528653)

(glisp:define-constant *trimetric-normal-vector-left* (make-vector -0.8342367128320977 -0.4377640254359154 0.3352786378480434))
   ;;(make-vector -0.83425635 -0.43777433 0.33528653)

(glisp:define-constant *trimetric-normal-vector-right-rear* (make-vector 0.8342367128320977 0.4377640254359154 0.3352786378480434))
  ;;(make-vector 0.83425635 0.43777433 0.33528653)

(glisp:define-constant *trimetric-normal-vector-left-rear* (make-vector -0.8342367128320977 0.4377640254359154 0.3352786378480434))
 ;;(make-vector -0.83425635 0.43777433 0.33528653)

(glisp:define-constant +nominal-origin+ (make-point 0 0 0))

(glisp:define-constant +half-sqrt-2+ (half (sqrt 2.0)))


(defparameter *standard-face-hts* (make-hash-table :test #'equalp))

(defparameter *line-thickness-default* 0.5)

(defparameter *standard-views*
    (list :top    *nominal-z-vector*
          :bottom *nominal-z-vector-r*
          :left   *nominal-x-vector-r*
          :right  *nominal-x-vector*
          :front  *nominal-y-vector-r*
          :rear   *nominal-y-vector*
          :trimetric *trimetric-normal-vector*
          :tri-r-r *trimetric-normal-vector-right-rear*
          :tri-l *trimetric-normal-vector-left*
          :tri-l-r *trimetric-normal-vector-left-rear*))


(defparameter *gs-text-alpha-bits* 4 "Integer. The amount of anti-aliasing for Ghostscript to apply
to text when making PNG or JPEG images. Defaults to 4. Set to 0 for no antialiasing.")

(defparameter *gs-graphics-alpha-bits* 4 "Integer. The amount of anti-aliasing for Ghostscript to apply
to graphics when making PNG or JPEG images. Defaults to 4. Set to 0 for no antialiasing.")


(defun point-expression (point)
  `(make-point ,(get-x point) ,(get-y point) ,(get-z point)))

(glisp:define-constant +postnet-bits+
    (make-array 10 :initial-contents '((t t nil nil nil) ;;0
                                       (nil nil nil t t) ;;1 
                                       (nil nil t nil t) ;;2 
                                       (nil nil t t nil) ;;3
                                       (nil t nil nil t) ;;4 
                                       (nil t nil t nil) ;;5
                                       (nil t t nil nil) ;;6
                                       (t nil nil nil t) ;;7
                                       (t nil nil t nil) ;;8
                                       (t nil t nil nil)))
  
  "[Constant] 2D Array of Boolean. Represents the encodings used by the US Post Office Postnet barcoding system.
this will be included and documented in a future version of the GDL PDF output format.")

(defparameter *hash-transforms?* t
  "Boolean. Controls whether the transformations of points from three-dimensional world coordinates
to two-dimensional display coordinates are cached in memory. Set this to NIL to save memory at the
expense of some execution speed. <i>Note: in the current GDL release, transform caching is temporarily
disabled, so this parameter will have no effect.</i> Defaults to T.")


(defparameter *view-transform-inverses-hash* (make-hash-table :test #'equalp))

;;
;; used as dynamically bound variable in obj format and maybe other
;; formats which have to keep track of vertex counts.
;;
(defparameter *vertex-count* nil)



(glisp:define-constant +identity-3x3+ 
    (make-array (list 3 3)
                :initial-contents '((1d0 0d0 0d0)
                                    (0d0 1d0 0d0)
                                    (0d0 0d0 1d0))))


(defparameter *zero-vector-checking?* t 
  "Boolean. Indicates whether GDL should perform checks for zero-length vectors 
where nonzero-length vectors are expected. Defaults to T. Setting to NIL will 
increase execution speed marginally, at the cost of decreased safety. Modify 
the value of this parameter at your own risk.")


(defparameter *break-leaders?* t "Boolean. Indicates whether to try making a gap in dimension leaders for the text block. Defaults to t.")

(defparameter *dxf-translation* (make-vector 0 0))
(defparameter *raphael-translation* (make-vector 0 0))
(defparameter *svg-translation* (make-vector 0 0))
(glisp:define-constant +rear-vector+ (make-vector 0 1 0))
(glisp:define-constant +top-vector+ (make-vector 0 0 1))

(defparameter +lh-identity-matrix+ #2A((1.0 0.0 0.0)(0.0 -1.0 0.0)(0.0 0.0 1.0)))

(defparameter *dxf-entity-id* nil)
