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

(define-object bezier-curve (base-object)
  
  :documentation 
  (:description "GDL currently supports third-degree Bezier curves, which are 
defined using four 3D <i>control-points</i>. The Bezier curve always passes 
through the first and last control points and lies within the convex hull of the control 
points. At the start point (i.e. the first control point), the curve is tangent 
to the vector pointing from the start point to the second control point. 
At the end point (i.e. the last control point), the curve is tangent to the 
vector pointing from the end point to the third control point."
   
   :examples "<pre>
 (in-package :gdl-user)

 (define-object bezier-sample (bezier-curve)
  :computed-slots
  ((control-points (list (make-point 0 0 0)
                         (make-point 1 1 0)
                         (make-point 2 1 0)
                         (make-point 3 0 0))))
  :objects
  ((points-display :type 'points-display
                   :points (the control-points))))

 (generate-sample-drawing :objects (let ((self (make-object 'bezier-sample)))
                                     (list self (the points-display))))
</pre>")

  :input-slots
  ("List of 4 3D Points. Specifies the control points for the Bezier curve."
   control-points
   
   (weights nil)
   (knots nil)
   (degree nil)
   
   )
  
  :computed-slots
  ((start (first (the control-points)))
   (end (lastcar (the control-points)))
   
   (%curves-to-draw% (list (the control-points)))
   
   (%renderer-info% (list :vrml? t :view-default :top))
   
   (coefficients (destructuring-bind (p1 p2 p3 p4) (the control-points)
                   (let (c0 c1 c2 c3)
                     (let* ((a (scalar*vector -1 p1))
                            (b (scalar*vector 3 p2))
                            (c (scalar*vector -3 p3))
                            (d (add-vectors a (add-vectors b (add-vectors c p4)))))
                       (setq c3 (make-vector (get-x d) (get-y d) (get-z d))))
                     (let* ((a (scalar*vector 3 p1))
                            (b (scalar*vector -6 p2))
                            (c (scalar*vector 3 p3))
                            (d (add-vectors a (add-vectors b c))))
                       (setq c2 (make-vector (get-x d) (get-y d) (get-z d))))
                     (let* ((a (scalar*vector -3 p1))
                            (b (scalar*vector 3 p2))
                            (c (add-vectors a b)))
                       (setq c1 (make-vector (get-x c) (get-y c) (get-z c))
                             c0 (make-vector (get-x p1) (get-y p1) (get-z p1))))
                     (make-array 4 :initial-contents (list c3 c2 c1 c0)))))
   
   (bounding-box (bounding-box-from-points (the control-points))))

  
  :methods
  (("List of 3D points. Returns points of intersection in the Z plane between this Bezier curve 
and the infinite line containing point <tt>point</tt> and direction <tt>vector</tt>. Use the 
between? function if you wish to establish whether the point is contained in a particular line 
segment.

:arguments (line \"GDL line object.\")

:&key ((accuracy (* 10.0 double-float-epsilon)) \"Number. Target accuracy.\"
      (maximum-number-of-iterations 30) \"Integer. Maximum iterations of polynomial solver.\")"
    intersection-2D
    ((line line) &rest args)
    (apply #'(lambda()
               (the (line-intersection-2D (the-object line start) (the-object line direction-vector))))
           args))
   
   
   ("List of 3D points. Returns points of intersection in the Z plane between this Bezier curve and 
the circle in the Z plane with center <tt>center</tt> and radius <tt>radius</tt>.

:arguments (circle \"GDL circle object.\")

:&key ((accuracy (* 10.0 double-float-epsilon)) \"Number. Target accuracy.\"
       (maximum-number-of-iterations 30) \"Integer. Maximum iterations of polynomial solver.\")"
    intersection-2D
    ((circle circle) &rest args)
    (apply #'(lambda()
               (the (circle-intersection-2D (the-object circle center) (the-object circle radius))))
           args))
   
   ("Throws error. This is the catch-all error method for 2D-intersection for unknown argument types."
    intersection-2D
    ((unknown-object t) &rest args)
    (declare (ignore args))
    (error "Sorry, base GDL does not yet have a 2D intersection method for ~s and ~s.~%"
           (the :type) (class-name (class-of (the-object unknown-object))))))
  
  
  :hidden-objects
  ((reverse :type 'bezier-curve :control-points (reverse (the control-points))))
  
  :functions
  ((b-spline-data
    ()
    (values (the control-points) (the weights) (the knots) (the degree)))
   
   
   (equi-spaced-points
    (number &key (spacing :parametric))
    (unless (eql spacing :parametric) (error "Equi-spaced-points for bezier-curve only supports :spacing :parametric."))
    (unless (> number 1) (error "Equi-spaced-points must be called with a number greater than 1."))
    (case spacing 
      (:parametric (let ((u1 0.0) (u2 1.0))
                     (let ((parameter-list (list-of-numbers u1 u2 (/ (- u2 u1) (1- number)))))
                       (mapcar #'(lambda(parameter) (the (point parameter))) parameter-list))))))
   
   
   ("3D Point. Returns the point on this Bezier curve corresponding to the given <tt>parameter</tt>,
     which should be between 0 and 1.

:arguments (parameter \"Number. Curve parameter, between zero and one (0 and 1).\")"
    point (parameter)
    (let ((coefficients (the coefficients)))
      (add-vectors (scalar*vector (* parameter parameter parameter) (elt coefficients 0))
                   (add-vectors (scalar*vector (* parameter parameter) (elt coefficients 1))
                                (add-vectors (scalar*vector parameter (elt coefficients 2))
                                             (elt coefficients 3))))))

   
   ("List of 3D points. Returns points of intersection in the Z plane between this Bezier curve 
and the infinite line containing point <tt>point</tt> and direction <tt>vector</tt>. Use the 
between? function if you wish to establish whether the point is contained in a particular line 
segment.

:arguments (point \"3D Point. Any point in the line to be intersected.\"
            vector \"3D Vector. The direction of the line to be intersected.\")

:&key ((accuracy (* 10.0 double-float-epsilon)) \"Number. Target accuracy.\"
      (maximum-number-of-iterations 30) \"Integer. Maximum iterations of polynomial solver.\")"

    line-intersection-2D
    (point vector &key (accuracy (* 10.0 double-float-epsilon))
           (maximum-number-of-iterations 30))
    (let* ((coefficients (the coefficients))
           (cross (cross-vectors *nominal-z-vector* vector))
           (p2 (translate-along-vector point vector (length-vector vector)))
           (cl (- (* (get-x point) (get-y p2))
                  (* (get-x p2) (get-y point))))
           (rotated-bezier (make-array 
                            (list 4)
                            :initial-contents (list (dot-vectors cross (elt coefficients 0))
                                                    (dot-vectors cross (elt coefficients 1))
                                                    (dot-vectors cross (elt coefficients 2))
                                                    (+ (dot-vectors cross (elt coefficients 3)) cl)))))
      (let ((parameters (get-zeroes-in-interval rotated-bezier 0 1 :accuracy accuracy
                                                :maximum-number-of-iterations maximum-number-of-iterations)))
        (mapcar #'(lambda(parameter)
                    (the (point parameter))) parameters))))
   

   
   ("List of 3D points. Returns points of intersection in the Z plane between this Bezier curve and 
the circle in the Z plane with center <tt>center</tt> and radius <tt>radius</tt>.

:arguments (center \"3D Point. The center of the circle to be intersected.\"
            radius \"Number. The radius of the circle to be intersected.\")

:&key ((accuracy (* 10.0 double-float-epsilon)) \"Number. Target accuracy.\"
      (maximum-number-of-iterations 30) \"Integer. Maximum iterations of polynomial solver.\")"
    circle-intersection-2D
    (center radius &key (accuracy (* 10.0 double-float-epsilon))
            (maximum-number-of-iterations 30))
    (let* ((rr (* radius radius))
           (coefficients (the coefficients))
           (c3 (aref coefficients 0))
           (c2 (aref coefficients 1))
           (c1 (aref coefficients 2))
           (c0 (aref coefficients 3))
           (polynomial 
            (make-array (list 7)
                        :initial-contents (list (+ (* (get-x c3) (get-x c3) rr)
                                                   (* (get-y c3) (get-y c3) rr))
                                                (twice (+ (* (get-x c3) (get-x c2) rr)
                                                          (* (get-y c3) (get-y c2) rr)))
                                                (+ (twice (+ (* (get-x c3) (get-x c1) rr)
                                                             (* (get-y c3) (get-y c1) rr)))
                                                   (* (get-x c2) (get-x c2) rr)
                                                   (* (get-y c2) (get-y c2) rr))
                                                (+ (twice (* (get-x c3) rr
                                                             (- (get-x c0) (get-x center))))
                                                   (twice (* (get-y c3) rr
                                                             (- (get-y c0) (get-y center))))
                                                   (twice (+ (* (get-x c2) (get-x c1) rr)
                                                             (* (get-y c2) (get-y c1) rr))))
                       
                                                (+ (twice (* (get-x c2) rr
                                                             (- (get-x c0) (get-x center))))
                                                   (twice (* (get-y c2) rr
                                                             (- (get-y c0) (get-y center))))
                                                   (* (get-x c1) (get-x c1) rr)
                                                   (* (get-y c1) (get-y c1) rr))
                                                (+ (twice (* (get-x c1) rr
                                                             (- (get-x c0) (get-x center))))
                                                   (twice (* (get-y c1) rr
                                                             (- (get-y c0) (get-y center)))))

                                                (+ (* (get-x c0) (get-x c0) rr)
                                                   (- (twice (* (get-y c0) (get-y center) rr)))
                                                   (- (twice (* (get-x c0) (get-x center) rr)))
                                                   (* (get-y c0) (get-y c0) rr)
                                                   (* (get-x center) (get-x center) rr)
                                                   (* (get-y center) (get-y center) rr)
                                                   (- (* rr rr)))))))

        (mapcar #'(lambda(param)
                    (the (point param)))
                (get-zeroes-in-interval polynomial 0 1 :accuracy accuracy 
                                        :maximum-number-of-iterations maximum-number-of-iterations))))))


