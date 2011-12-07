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


;;
;; FLAG -- efficiency!!!
;;


(defun array-to-3d-vector (array)
  "3D Vector. Returns a 3D-Vector of double-floats built from a 3-by-1 Lisp array of numbers.

:arguments (array \"3-by-1 Lisp array of numbers\")"
  
  (make-vector (aref array 0 0) (aref array 0 1) (aref array 0 2)))

(defun 3d-vector-to-array (vector)
  "3-by-1 Lisp array of double-floats. Returns a 3-by-1 Lisp array of double-float numbers 
built from a 3D-Vector of double-floats. This can be useful for example for multiplying
a GDL 3d-point (which is a 1-d vector) by a 3x3 matrix represented as a 2D Lisp array.

:arguments (vector \"3D-Vector of double-floats (e.g. created with make-vector macro)\")"
  
  (make-array (list 1 3) :initial-contents (list (list (get-x vector) (get-y vector) (get-z vector)))))


;;
;; FLAG -- handle points of 2 and 4 dimensions.
;;


(defun get-x (point) "Double-float number. Returns X component of point or vector

:arguments (point \"2D, 3D, or 4D point\")"

       (svref point 0))

(defun get-u (point) "Double-float number. Returns U component of 2D parameter value.

:arguments (point \"2D point\")"
       (get-x point))


(defun get-v (point) "Double-float number. Returns V component of 2D parameter value.

n:arguments (point \"2D point\")"
       (get-y point))


(defun %get-x% (point) 
  (coerce (svref point 0) 'single-float))


(defun get-y (point) "Double-float number. Returns Y component of point or vector

:arguments (point \"2D, 3D, or 4D point\")" 
       (svref point 1))

(defun %get-y% (point) 
  (coerce (svref point 1) 'single-float))

(defun get-z (point) "Double-float number. Returns Z component of point or vector

:arguments (point \"3D or 4D point\")" 
       (svref point 2))

(defun %get-z% (point) 
  (coerce (svref point 2) 'single-float))

(defun get-w (quaternion) "Double-float number. Returns W component of point or vector

:arguments (quaternion \"4D point, Quaternion, or Axis-Angle style rotation spec\")" 
       (svref quaternion 3))

(defun %get-w% (point) 
  (coerce (svref point 3) 'single-float))

(defun subtract-vectors  (v1 v2)
  "Vector. Return a new vector, the result of affine vector subtraction.

:arguments (v1 \"2D, 3D, or 4D Vector\"
            v2 \"2D, 3D, or 4D Vector\")"
  (ecase (array-dimension v1 0)
    (2 (make-vector (- (svref v1 0) (svref v2 0))
                    (- (svref v1 1) (svref v2 1))))
    (3 (make-vector (- (svref v1 0) (svref v2 0))
                    (- (svref v1 1) (svref v2 1))
                    (- (svref v1 2) (svref v2 2))))
    (4 (make-vector (- (svref v1 0) (svref v2 0))
                    (- (svref v1 1) (svref v2 1))
                    (- (svref v1 2) (svref v2 2))
                    (- (svref v1 3) (svref v2 3))))))


(defun add-vectors  (v1 v2)
  "Vector. Return a new vector, the result of affine vector addition.

:arguments (v1 \"2D, 3D, or 4D Vector\"
            v2 \"2D, 3D, or 4D Vector\")"
  (ecase (array-dimension v1 0)
    (2 (make-vector (+ (svref v1 0) (svref v2 0))
                    (+ (svref v1 1) (svref v2 1))))
    (3 (make-vector (+ (svref v1 0) (svref v2 0))
                    (+ (svref v1 1) (svref v2 1))
                    (+ (svref v1 2) (svref v2 2))))
    (4 (make-vector (+ (svref v1 0) (svref v2 0))
                    (+ (svref v1 1) (svref v2 1))
                    (+ (svref v1 2) (svref v2 2))
                    (+ (svref v1 3) (svref v2 3))))))


(defun 3d-distance (point-1 point-2)
  "Number. The three-dimensional distance from 
point-1 to point-2.


:arguments (point-1 \"3D point\"
            point-1 \"3D point\")"
  
  (declare ;;(type (array double-float) point-1 point-2)
           (optimize (speed 3) (compilation-speed 0) 
                     (safety 0) (debug 0)))
  
  (cond ((and (= (array-dimension point-1 0) 3)
              (= (array-dimension point-2 0) 3))
         (let ((dx (- (svref point-1 0) (svref point-2 0)))
               (dy (- (svref point-1 1) (svref point-2 1)))
               (dz (- (svref point-1 2) (svref point-2 2))))
           (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
        ((and (= (array-dimension point-1 0) 2)
              (= (array-dimension point-2 0) 2))
         (let ((dx (- (svref point-1 0) (svref point-2 0)))
               (dy (- (svref point-1 1) (svref point-2 1))))
           (sqrt (+ (* dx dx) (* dy dy)))))
        (t (error "Invalid arguments ~s and ~s passed to 3d-distance~%" point-1 point-2))))



(defun scalar*vector (scalar vector)
  "Vector. Returns result of multiplying the scalar number by the vector

:arguments (scalar \"Number\"
            vector \"2D, 3D, or 4D Vector\")"

  (ecase (array-dimension vector 0) 
    (2 (make-array 2 :initial-contents (list (* scalar (svref vector 0))
                                             (* scalar (svref vector 1)))))
    (3 (make-array 3 :initial-contents (list (* scalar (svref vector 0))
                                             (* scalar (svref vector 1))
                                             (* scalar (svref vector 2)))))
    (4 (make-array 4 :initial-contents (list (* scalar (svref vector 0))
                                             (* scalar (svref vector 1))
                                             (* scalar (svref vector 2))
                                             (* scalar (svref vector 3)))))))



;;
;; FLAG consider replacing these matrix functions with standard ones.
;;

(defun scalar*matrix (scalar matrix)
  "Lisp Array. Returns result of multiplying the scalar number by the matrix.

:arguments (scalar \"Number\"
            matrix \"Lisp Array of Numbers\")"

  (let ((n (array-dimension matrix 0))
        (m (array-dimension matrix 1)))
    (let ((return-matrix (make-array (list n m))))
      (dotimes (i n return-matrix)
        (dotimes (j m)
          (setf (aref return-matrix i j)
            (* (aref matrix i j) scalar)))))))


(defun add-matrices (&rest matrices)
  "Lisp Array. Adds two matrices element-by-element.

:rest (matrics \"Lisp Arrays of same dimensions\")"
  
  (let ((n (array-dimension (first matrices) 0))
        (m (array-dimension (first matrices) 1)))
    (mapc #'(lambda(dimension rank)
              (when (some #'(lambda(matrix)
                              (/= (array-dimension matrix rank) dimension))
                          (rest matrices))
                (error "add-matrices given matrices with incompatible dimensions: ~s~%" matrices)))
          (list n m) (list 0 1))
    (let ((return-matrix (make-array (list n m) :initial-element 0d0)))
      (dotimes (i n return-matrix)
        (dotimes (j m)
          (dolist (matrix matrices)
            (setf (aref return-matrix i j)
              (+ (aref return-matrix i j)
                 (aref matrix i j)))))))))





(defun matrix*vector (matrix vector)
  "Lisp array. Multiplies matrix by column vector of compatible dimension. 

:arguments (matrix \"Lisp Array of Numbers\"
            vector \"Vector\")"
  
  (when (not (and (= (array-rank vector) 1)
                  (= (array-dimension matrix 1) (length vector))))
    (error "matrix*vector given arguments of incompatible length: ~s and ~s~%" matrix vector))
  (let ((m (array-dimension matrix 0))
        (n (array-dimension matrix 1)))
    (let ((result-vector 
           (if (= m 3) (make-vector 0 0 0)
             (make-array m :initial-element 0d0))))
      (dotimes (i m result-vector)
        (dotimes (j n)
          (setf (aref result-vector i)
            (+ (aref result-vector i) 
               (* (aref vector j)
                  (aref matrix i j)))))))))


(defun transpose-matrix (matrix)
  "Lisp array. Transposes rows and columns of <b>matrix</b>.

:arguments (matrix \"Lisp Array\")"
  
  (let ((m (array-dimension matrix 0))
        (n (array-dimension matrix 1)))
    (let ((return-matrix (make-array (list n m))))
      (dotimes (i m return-matrix)
        (dotimes (j n return-matrix)
          (setf (aref return-matrix j i)
            (aref matrix i j)))))))



(defun transform-3d-vector (vector transform)
  (matrix*vector (matrix:transpose-matrix transform) vector))


                 
  

(defun multiply-matrices (matrix-1 matrix-2)
  "Lisp Array. Multiplies compatible-size matrices according to normal matrix math.

:arguments (matrix-1 \"Lisp Array of Numbers\"
            matrix-2 \"Lisp Array of Numbers\")"
  
  (let (1d-result?)  
    (when (= (length (array-dimensions matrix-1)) 1)
      (setq 1d-result? t)
      (setq matrix-1 (3d-vector-to-array matrix-1)))
  
    (when (= (length (array-dimensions matrix-2)) 1)
      (setq matrix-2 (3d-vector-to-array matrix-2)))
  
    (when (/= (array-dimension matrix-1 1)
              (array-dimension matrix-2 0))
      (error "multiply-matrices given arguments of incompatible lengths: ~s and ~s~%" 
             matrix-1 matrix-2))
    
    (let ((result
           (let ((n (array-dimension matrix-1 0))
                 (k (array-dimension matrix-1 1))
                 (p (array-dimension matrix-2 1)))
             (let ((array (make-array (list n p))))
               (dotimes (i n array)
                 (dotimes (j p)
                   (setf (aref array i j)
                     (let ((sum 0))
                       (dotimes (s k sum)
                         (setq sum (+ sum (* (aref matrix-1 i s)
                                             (aref matrix-2 s j)))))))))))))
      
      (if 1d-result? (array-to-3d-vector result) result))))


(defun dot-vectors (vector-1 vector-2)
  "Number. Returns the dot product of vector-1 and vector-2.

:arguments (vector-1 \"2D, 3D, or 4D Vector\"
            vector-2 \"2D, 3D, or 4D Vector\")"
  
  (apply #'+ (mapcar #'* (coerce vector-1 'list) (coerce vector-2 'list))))




(defun rh-rule-cross (vector-1 axis-1 vector-2 axis-2)
  
  (multiple-value-bind (vector-1 axis-1)
      (unitize-with-axis vector-1 axis-1)
    
    (multiple-value-bind (vector-2 axis-2)
        (unitize-with-axis vector-2 axis-2)
  
      (let ((nominal (cross-vectors vector-1 vector-2))
            (axes (list :right :rear :top :right)))

        (let ((reverse? (not (eql (nth (1+ (position axis-1 axes)) axes) axis-2))))
          (if reverse? (reverse-vector nominal) nominal))))))



(defun alignment (axis1 vector1 &optional (axis2 (if (member axis1 (list :top :bottom)) :rear :top))
                                          (vector2 (case axis2 
                                                     (:rear (if (or (same-direction-vectors? vector1 (make-vector 0 1 0))
                                                                    (same-direction-vectors? vector1 (make-vector 0 -1 0)))
                                                                (make-vector 0 0 1)
                                                              (make-vector 0 1 0)))
                                                     (:top (if (or (same-direction-vectors? vector1 (make-vector 0 0 1))
                                                                    (same-direction-vectors? vector1 (make-vector 0 0 -1)))
                                                                (make-vector 0 1 0)
                                                              (make-vector 0 0 1)))))

                                          (axis3 (first (set-difference (list :right :rear :top)
                                                                        (mapcar #'normalize-axis
                                                                                (list axis1 axis2)))))
                                          (vector3 (unitize-vector 
                                                    (rh-rule-cross (orthogonal-component vector2 vector1) axis2
                                                                   vector1 axis1))))
  "3x3 Orthonormal Rotation Matrix. Constructs a rotation 
matrix from the given axes and vectors. Up to three
pairs of axis and vector can be given. 

If only one pair is given, then the orthogonal component of its vector with respect to
the other two global axes is used.

If a second pair is given, then the orthogonal component of its vector 
with respect to the first vector is used. 

A third pair is only required if a left-handed coordinate system
is desired (right-handed is the default). The third vector will 
always be converted to the cross of the first two, unless it is 
given as the reverse of this, which will force a left-handed 
coordinate system.

Axes are direction keywords which can be one of:

   <ul><li><tt>:right</tt></li>
       <li><tt>:left</tt></li>
       <li><tt>:rear</tt></li>
       <li><tt>:front</tt></li>
       <li><tt>:top</tt></li>
       <li><tt>:bottom</tt></li></ul>

The second axis keyword, if given, must be orthogonal to the first, and the third, 
if given, must be orthogonal to the first two.

:arguments (axis-1 \"Direction Keyword\"
            vector1 \"3D Vector\")
:&optional (axis-2 \"Direction Keyword\"
            vector2 \"3D Vector\"
            axis-3 \"Direction Keyword\"
            vector3 \"3D Vector\")"

  (multiple-value-bind (vector-1 axis-1) (unitize-with-axis vector1 axis1)
    (multiple-value-bind (vector-2 axis-2) (unitize-with-axis vector2 axis2)
      (setq vector-2 (unitize-vector (orthogonal-component vector-2 vector-1)))
      (multiple-value-bind (vector-3 axis-3) (unitize-with-axis vector3 axis3)
        (let ((vector-3-nominal (unitize-vector (rh-rule-cross vector-1 axis-1 vector-2 axis-2))))
          (setq vector-3
            (if (> (angle-between-vectors vector-3 vector-3-nominal) pi/2)
                (reverse-vector vector-3-nominal) vector-3-nominal))
          (let ((vector-axis-list (list axis-1 vector-1 axis-2 vector-2 axis-3 vector-3)))
            (make-transform (list (coerce (getf vector-axis-list :right) 'list)
                                  (coerce (getf vector-axis-list :rear) 'list)
                                  (coerce (getf vector-axis-list :top) 'list)))))))))




(defun normalize-color (triplet &key (tolerance 0.1))
  (if (every #'(lambda(num)(< num tolerance)) triplet)
      (list 1 1 1) triplet))

(defun normalize-axis (axis)
  (ecase axis
    ((:left :right) :right)
    ((:top :bottom) :top)
    ((:front :rear) :rear)))



(defun unitize-with-axis (vector axis)
  (ecase axis
    ((:right :rear :top) (values (unitize-vector vector) axis))
    ((:left :front :bottom) (values (unitize-vector (reverse-vector vector))
                                    (ecase axis
                                      (:left :right) (:front :rear) (:bottom :top))))))


(defun make-transform (list-of-lists)
  "Lisp array. Builds a matrix from <b>list-of lists</b>.

:arguments (list-of-lists \"List of lists of numbers\")"
  
  (make-array (list (length list-of-lists)
                    (length (first list-of-lists)))
              :initial-contents (mapcar #'(lambda(outer)
                                            (mapcar #'(lambda(inner)
                                                        (coerce inner 'double-float))
                                                    outer))
                                        list-of-lists)))

(defun angle-between-vectors-d (vector-1 vector-2 &optional reference-vector negative?)
  "Number. Returns the angle in degrees between <tt>vector-1</tt> and <tt>vector-2</tt>. 
   If no <tt>reference-vector</tt> is given, the smallest possible angle is returned. 
   If a <tt>reference-vector</tt> is given, computes according to the right-hand rule. 
   If <tt>negative?</tt> is specified as non-NIL,  returns a negative number for angle 
   if it really is negative according to the right-hand rule.

:arguments (vector-1 \"3D Vector\"
            vector-2 \"3D Vector\")
:&optional ((reference-vector NIL) \"3D Vector\"
            (negative? nil) \"Boolean\")"

  (if reference-vector
      (let ((angle (radians-to-degrees
                    (angle-between-vectors vector-1 vector-2 reference-vector))))
        (if negative?
            (if (> angle 180) (- angle 360) angle)
          angle))
    (radians-to-degrees
     (angle-between-vectors vector-1 vector-2))))

(defun angle-between-vectors (vector-1 vector-2 &optional reference-vector &key (epsilon *zero-epsilon*) -ve)
  "Number. Returns the angle in radians between <b>vector-1</b> and <b>vector-2</b>. 
   If no <b>reference-vector</b> given, the smallest possible angle is returned. 
   If a <b>reference-vector</b> is given, computes according to the right-hand rule. 
   If <b>-ve</b> is given,  returns a negative number for angle if it really is 
   negative according to the right-hand rule.

:arguments (vector-1 \"3D Vector\"
            vector-2 \"3D Vector\")
:&optional ((reference-vector nil) \"3D Vector\")
:&key      ((epsilon *zero-epsilon*) \"Number. Determines how small of an angle is considered to be zero.\"
            (-ve nil) \"Boolean\")"
  (let ((vector-1 (unitize-vector vector-1))
        (vector-2 (unitize-vector vector-2)))
    
    (let ((dot-vector (dot-vectors vector-1 vector-2)))
      (when (> dot-vector 1.0d0) (setq dot-vector 1.0d0))
      (when (< dot-vector -1.0d0) (setq dot-vector -1.0d0))
      (let ((smallest-angle (acos dot-vector)))
        (when (< smallest-angle epsilon) (setq smallest-angle 0.0d0))
        (let ((try (if (and reference-vector
                            (minusp (dot-vectors reference-vector (cross-vectors vector-1 vector-2))))
                       (- (twice pi) smallest-angle)
                     smallest-angle)))
          (if (and -ve (> try pi)) (- try (twice pi)) try))))))


(defun unitize-vector  (vector)
  "Unit Vector. Returns the normalized unit-length vector corresponding to <b>vector</b>.

:arguments (vector \"3D Vector\")"
  
  (when (and *zero-vector-checking?* (zero-vector? vector))
    (error "~s has Euclidean length zero; cannot unitize." vector))
  
  (if (= (length-vector vector) 1.0)
      vector
    (scalar*vector (/ (length-vector vector)) vector)))



(defun orthogonal-component (vector reference-vector)
  "3D Unit Vector. Returns the unit vector orthogonal to <b>reference-vector</b> which
is as close as possible to <b>vector</b>.

:arguments (vector \"3D Vector\"
            reference-vector \"3D Vector\")"
  (unitize-vector
   (if (zerop (dot-vectors vector reference-vector)) vector
     (let ((normal (cross-vectors vector reference-vector)))
       (if (zero-vector? normal) +nominal-origin+
         (cross-vectors reference-vector normal))))))


(defun parallel-vectors? (vector-1 vector-2 &key (tolerance *zero-epsilon*))
    "Boolean. Returns non-nil iff <b>vector-1</b> and <b>vector-2</b> are pointing in the 
same direction or opposite directions. 

:arguments (vector-1 \"3D Vector\"
            vector-2 \"3D Vector\")
:&key ((tolerance *zero-epsilon*) \"Number\")"
  (or (same-direction-vectors? vector-1 vector-2 :tolerance tolerance)
      (same-direction-vectors? vector-1 (reverse-vector vector-2) :tolerance tolerance)))
  

(defun same-direction-vectors? (vector-1 vector-2 &key (tolerance *zero-epsilon*))
  "Boolean. Returns non-NIL iff <b>vector-1</b> and <b>vector-2</b> are pointing in the 
same direction.

:arguments (vector-1 \"3D Vector\"
            vector-2 \"3D Vector\")
:&key ((tolerance *zero-epsilon*) \"Number\")"
            
  (and (not (or (zero-vector? vector-1) (zero-vector? vector-2)))
       (coincident-point? (unitize-vector vector-1) (unitize-vector vector-2) :tolerance tolerance)))

(defun reverse-vector   (vector)
  "Vector. Return the vector pointing in the opposite direction.

:arguments (vector \"2D, 3D, or 4D Vector\")"
  
  (scalar*vector -1d0 vector))

(defun cross-vectors (vector-1 vector-2)
  "3D Vector. Returns the cross product of vector-1 and vector-2. According to
the definition of cross product, this resultant vector should be orthogonal 
to both <b>vector-1</b> and <b>vector-2</b>.

:arguments (vector-1 \"3D Vector\"
            vector-2 \"3D Vector\")"
  (make-array 3 :initial-contents (list (- (* (get-y vector-1) (get-z vector-2))
                                           (* (get-y vector-2) (get-z vector-1)))
                                        (- (* (get-x vector-2) (get-z vector-1))
                                           (* (get-x vector-1) (get-z vector-2)))
                                        (- (* (get-x vector-1) (get-y vector-2))
                                           (* (get-x vector-2) (get-y vector-1))))))


(defun length-vector (vector)
  "Number. Return the vector's magnitude

:arguments (vector \"3D Vector\")"

  (3d-distance vector +nominal-origin+))


(defun zero-vector? (vector)
  "Boolean. Returns non-NIL iff the vector has zero length according to Common Lisp <tt>zerop</tt> function.

:arguments (vector \"3D Vector\")"
  
  (and (zerop (svref vector 0)) (zerop (svref vector 1)) (zerop (svref vector 2))))



(defun degree (degrees &optional (minutes 0) (seconds 0))
  "Number. Converts angle in degrees, minutes, and seconds into radians.

:arguments (degrees \"Number\")
:&optional ((minutes 0) \"Number\")
            (seconds 0) \"Number\"))"
  
  (* (/ (+ degrees (/ minutes 60) (/ seconds 3600))  180) pi))


;; FLAG -- this does not seem to be giving the precision one would
;; like.  we probably have to start declaring some types to be
;; double-float.
;;
(defun radians-to-degrees (radians)
  "Number. Converts angle in radians to degrees.

:arguments (radians \"Number\")"

  (* radians (/ 180 pi)))


(defun radians-to-grads (radians)
  "Number. Converts angle in radians to grads.

:arguments (radians \"Number\")"

  (* (radians-to-degrees radians) (/ 10 9)))


(defun rotate-vector-d (vector degrees normal)
  "Number. Rotates <b>vector</b> around <b>normal</b> by an amount of rotation 
specified by <b>degrees</b>.

:arguments (vector \"3D Vector\"
            degrees \"Number\"
            normal \"3D Vector\")"
  (rotate-vector vector (degree degrees) normal))


(defun translate-along-vector (point vector distance &optional unit?)
  "3D Point. Returns a new point which is <b>point</b> translated 
along <b>vector</b> by <b>distance</b>

:arguments (point \"3D Point\"
            vector \"3D Vector\"
            distance \"Number\")"

  (add-vectors point (scalar*vector distance (if unit? vector (unitize-vector vector)))))

;;
;; FLAG -- figure out how to access documentations for methods
;;         e.g. for following three.
;;
(defmethod direction-vector ((line list))
  "3D Vector. Gives the direction of <b>line</b>, which is a line segment represented
in the form of a list of two 3D Points.

:arguments (line \"list of two 3D Points.\")"

  (subtract-vectors (second line) (first line)))

(defmethod line-length ((line list))
  "Number. Gives the length of <b>line</b>, which is a line segment represented
in the form of a list of two 3D Points.

:arguments (line \"list of two 3D Points.\")"
  (3d-distance (first line) (second line)))

(defmethod mid-point ((line list))
  "3D Point. Gives the midpoint of <b>line</b>, which is a line segment represented
in the form of a list of two 3D Points.

:arguments (line \"list of two 3D Points.\")"
  (translate-along-vector (first line)
                          (direction-vector line)
                          (half (line-length line))))



(defun array-to-list (array &optional (decimal-places 2))
  "List. Converts <b>array</b> to a list.

:arguments (array \"Lisp Array of Numbers\")
:&optional ((decimal-places 2) \"Integer. Numbers will be rounded to this many decimal places.\")"

  (if array
      (if (or (> (length array) 3)
              (not (numberp (elt array 0))))
          (coerce array 'list)
        (remove nil
                (list (number-round (get-x array) decimal-places)
                      (number-round (get-y array) decimal-places)
                      (if (get-z array)
                          (number-round (get-z array) decimal-places)))))))




(defun coincident-point? (point-1 point-2 &key (rank (length point-1)) (tolerance *zero-epsilon*))
  "Boolean. Returns non-NIL iff the distance between <b>point-1</b> and <b>point-2</b> 
is less than <b>tolerance</b>.

:arguments (point-1 \"3D Point\"
            point-2 \"3D Point\")
:&key ((tolerance *zero-epsilon*) \"Number\")"
  (if (< rank (max (length point-1) (length point-2)))
      (let ((result t))
        (dotimes (n rank result)
          (when (not (< (abs (- (svref point-1 n) (svref point-2 n))) tolerance))
            (setq result nil))))
    (< (3d-distance point-1 point-2) tolerance)))


(defun projected-vector (vector plane-normal)
  "3D Vector. Returns result of projecting <b>vector</b> onto the plane whose normal
is <b>plane-normal</b>.

:arguments (vector \"3D Vector\"
            plane-normal \"3D Vector\")"
  (subtract-vectors vector (scalar*vector
                            (dot-vectors vector (unitize-vector plane-normal))
                            (unitize-vector plane-normal))))

(defun rotate-point-d (point center normal &key arc-length angle)
  "3D Point. Returns the 3D Point resulting from rotating <b>point</b> about 
<b>center</b> in the plane defined by <b>normal</b>. The rotation can specified 
either by an arc length (<b>arc-length</b>) or an angle in degrees (<b>angle</b>). 
A second value is returned, which is the resulting angle of rotation in degrees (this 
is of possible use if <b>arc-length</b> is used to specify the rotation).

:arguments (point \"3D Point\"
            center \"3D Point\"
            normal \"3D Vector\")
:&key ((arc-length nil) \"Number\"
       (angle nil) \"Number\")"

  (if (coincident-point? point center)
      point
    (let* ((radius (3d-distance point center))
           (alpha (if angle
                      angle
                    (div (* arc-length 180)
                         (* pi radius))))
           (vec (rotate-vector-d (subtract-vectors point center)
                                 alpha
                                 normal)))
      (values (translate-along-vector center vec radius) alpha))))


(defun rotate-point (point center normal &key (arc-length nil) (angle nil))
  "3D Point. Returns the 3D Point resulting from rotating <b>point</b> about 
<b>center</b> in the plane defined by <b>normal</b>. The rotation can specified 
either by an arc length (<b>arc-length</b>) or an angle in radians (<b>angle</b>). 
A second value is returned, which is the resulting angle of rotation in radians (this 
is of possible use if <b>arc-length</b> is used to specify the rotation).

:arguments (point \"3D Point\"
            center \"3D Point\"
            normal \"3D Vector\")
:&key ((arc-length nil) \"Number\"
       (angle nil) \"Number\")"
  (if (coincident-point? point center)
      point
    (let* ((radius (3d-distance point center))
           (alpha (if angle angle
                    (/ (* arc-length pi)
                       (* pi radius))))
           (vec (rotate-vector (subtract-vectors point center) alpha normal)))
      (values (translate-along-vector center vec radius) alpha))))




;;
;; Uses Rodrigues' Rotation Formula.
;;
(defun rotate-vector (vector angle normal)
  "Number. Rotates <b>vector</b> around <b>normal</b> by an amount of rotation 
specified by <b>angle</b>, which is an angle measured in radians.

:arguments (vector \"3D Vector\"
            angle \"Number\"
            normal \"3D Vector\")"
  (let* ((normal (unitize-vector normal))
         (w1 (svref normal 0))
         (w2 (svref normal 1))
         (w3 (svref normal 2))
         (w-matrix (let ((array (make-array (list 3 3) :initial-element 0d0)))
                     (setf (aref array 0 1) (- w3)
                           (aref array 0 2) w2
                           (aref array 1 0) w3
                           (aref array 1 2) (- w1)
                           (aref array 2 0) (- w2)
                           (aref array 2 1) w1) array))
         (w-squared (matrix:multiply-matrix w-matrix w-matrix)))
    (let ((transform (matrix:add-matrix
                      +identity-3x3+
                      (scalar*matrix (sin angle) w-matrix)
                      (scalar*matrix (- 1 (cos angle)) w-squared))))
      (matrix*vector transform vector))))



(defun inter-circle-sphere (circle-center circle-radius circle-plane-normal 
                            sphere-center sphere-radius positive-angle? &key (tolerance *zero-epsilon*))
  
  "3D Point or NIL. Returns point of intersection between the circle described by <b>circle-center</b>,
<b>circle-radius</b>, and <b>circle-plane-normal</b>, and the sphere described by <b>sphere-center</b>
and <b>sphere-radius</b>. Iff the circle and sphere do not intersect at all, NIL is returned.

:arguments (circle-center \"3D Point\"
            circle-radius \"Number\"
            circle-plane-normal \"3D Vector\"
            sphere-center \"3D Point\"
            sphere-radius \"Number\"
            positive-angle? \"Boolean. Controls which of two intersection points is returned\")
:&key ((tolerance *zero-epsilon*) \"Controls how close the entities must come to touching to be
                                    considered as intersecting.\")"
  
  (let* ((vector-1 circle-plane-normal)
         (sphere-circle-center (inter-line-plane 
                                sphere-center circle-plane-normal circle-center circle-plane-normal))
         (vector-2 (subtract-vectors circle-center sphere-circle-center))
         (sphere-circle-radius (3d-distance sphere-circle-center 
                                            (inter-line-sphere sphere-circle-center vector-2
                                                               sphere-center sphere-radius vector-2)))
         
         (transform (alignment :top vector-1
                               :rear (if (or (same-direction-vectors? vector-1 *nominal-z-vector*)
                                             (same-direction-vectors? vector-1 *nominal-z-vector-r*))
                                         *nominal-y-vector* *nominal-z-vector*)))
         
         (inverse (cond 
                   ((same-direction-vectors? vector-1 *nominal-z-vector*)
                    #2A((1.0d0 0.0d0 0.0d0) (0.0d0 1.0d0 0.0d0) (0.0d0 0.0d0 1.0d0)))
                   ((same-direction-vectors? vector-1 *nominal-z-vector-r*)
                    #2A((1.0d0 0.0d0 0.0d0) (0.0d0 -1.0d0 0.0d0) (0.0d0 0.0d0 1.0d0)))
                   ((same-direction-vectors? vector-1 *nominal-x-vector*)
                    #2a((0.0d0 0.0d0 1.0d0) (1.0d0 0.0d0 0.0d0)(0.0d0 1.0d0 0.0d0)))
                   ((same-direction-vectors? vector-1 *nominal-x-vector-r*)
                    #2a((0.0d0 0.0d0 1.0d0) (-1.0d0 0.0d0 0.0d0)(0.0d0 1.0d0 0.0d0)))
                   ((same-direction-vectors? vector-1 *nominal-y-vector*)
                    #2A((-1.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 1.0d0)(0.0d0 1.0d0 0.0d0)))
                   ((same-direction-vectors? vector-1 *nominal-y-vector-r*)
                    #2A((1.0d0 0.0d0 0.0d0)(0.0d0 0.0d0 1.0d0) (0.0d0 1.0d0 0.0d0)))
                   (t (matrix:invert-matrix transform))))
         
         (sphere-circle-center-n (array-to-3d-vector 
                                  (multiply-matrices (3d-vector-to-array sphere-circle-center) inverse)))

         (circle-center-n (array-to-3d-vector (multiply-matrices (3d-vector-to-array circle-center) inverse))))
    
    (let ((intersect (inter-circle-circle circle-center-n circle-radius 
                                          sphere-circle-center-n sphere-circle-radius
                                          positive-angle? :tolerance tolerance)))
      (when intersect (array-to-3d-vector (multiply-matrices (3d-vector-to-array intersect) transform))))))


(defun inter-circle-circle (center-1 radius-1 center-2 radius-2 positive-angle? &key (tolerance *zero-epsilon*))
  (let ((span (3d-distance center-1 center-2)))
    (cond ((coincident-point? center-1 center-2)
           (when (near-to? radius-1 radius-2 tolerance) :infinity))
          ((> span (+ radius-1 radius-2))  nil)
          ((= span (+ radius-1 radius-2))
           (translate-along-vector center-1 (subtract-vectors center-2 center-1) radius-1))
          (t (let ((h-squared (- (^2 radius-1) 
                                 (^2 (/ (+ (^2 span) (^2 radius-1) (- (^2 radius-2))) 
                                        (twice span))))))
               (unless (minusp h-squared)
                 (let ((L1 (half (+ (/ (- (^2 radius-1) (^2 radius-2)) span) span)))
                       (h (sqrt h-squared)))
                   (let ((c1c2 (subtract-vectors center-2 center-1))
                         (top (make-vector 0 0 1)))
                     (translate-along-vector
                      (translate-along-vector center-1 c1c2 L1) 
                      (cross-vectors top (if positive-angle? c1c2 (reverse-vector c1c2))) h)))))))))
                 
;;
;; Formula from http://astronomy.swin.edu.au/pbourke/geometry/sphereline/
;;
(defun inter-line-sphere (p-line u-line center radius side-vector)
  "3D Point or NIL. Returns one point of intersection between line described by 
point <b>p-line</b> and direction-vector <b>u-line</b>, and sphere described by 
<b>center</b> and <b>radius</b>. Iff the line and sphere do not intersect at
all, NIL is returned.

:arguments (p-line \"3D Point. Any point on the line.\"
            u-line \"3D Vector. Direction of the line.\"
            center \"3D Point. Center of the sphere.\"
            radius \"Number. The radius of the sphere.\"
            side-vector \"3D Vector. Controls which of two possible intersection points is returned.\")"
  
  (let ((p-line-2 (translate-along-vector p-line u-line (length-vector u-line))))
    (let ((x1 (get-x p-line))(y1 (get-y p-line))(z1 (get-z p-line))
          (x2 (get-x p-line-2))(y2 (get-y p-line-2))(z2 (get-z p-line-2))
          (x3 (get-x center)) (y3 (get-y center)) (z3 (get-z center)))
      
      (let ((dx (- x2 x1)) (dy (- y2 y1)) (dz (- z2 z1)))
      
        (let ((a (+ (^2 dx) (^2 dy) (^2 dz)))
              (b (twice (+ (* dx (- x1 x3)) 
                           (* dy (- y1 y3))
                           (* dz (- z1 z3)))))
              (c (- (+ (^2 x3) (^2 y3) (^2 z3) (^2 x1) (^2 y1) (^2 z1))
                    (twice (+ (* x3 x1) (* y3 y1) (* z3 z1)))
                    (^2 radius))))
          (let ((key-val (- (^2 b) (* 4 a c))))
            (cond ((zerop key-val)
                   (let ((u (- (/ b (* 2 a)))))
                     (make-vector (+ x1 (* dx u))
                                  (+ y1 (* dy u))
                                  (+ z1 (* dz u)))))
                  ((plusp key-val)
                   (let ((positive-u (/ (- (sqrt key-val) b)
                                        (* 2 a)))
                         (negative-u (/ (- (- b) (sqrt key-val))
                                        (* 2 a))))
                     (let ((positive-answer 
                            (make-vector (+ x1 (* dx positive-u))
                                         (+ y1 (* dy positive-u))
                                         (+ z1 (* dz positive-u))))
                           (negative-answer 
                            (make-vector (+ x1 (* dx negative-u))
                                         (+ y1 (* dy negative-u))
                                         (+ z1 (* dz negative-u)))))
                       
                       (if (coincident-point? positive-answer negative-answer)
                           positive-answer
                         (let ((solutions-vector (subtract-vectors positive-answer negative-answer)))

                           (if (< (angle-between-vectors-d solutions-vector side-vector) 90)
                           
                               positive-answer negative-answer)))))))))))))




(defun inter-line-plane (p-line u-line p-plane u-plane)
  "3D Point or NIL. Returns one point of intersection between line described by 
point <b>p-line</b> and direction-vector <b>u-line</b>, and plane described by 
<b>p-plane</b> and <b>u-plane</b>. Iff the line and plane do not intersect at
all (i.e. they are parallel), NIL is returned.

:arguments (p-line \"3D Point. Any point on the line.\"
            u-line \"3D Vector. Direction of the line.\"
            p-plane \"3D Point. Any point on the plane.\"
            u-plane \"3D Vector. Normal of the plane.\")"
  (when (not (zerop (dot-vectors u-line u-plane)))
    (let ((w (subtract-vectors p-line p-plane))
          (n (unitize-vector u-line))
          (u (unitize-vector u-plane)))
      (let ((s (/ (- (dot-vectors u w))
                  (dot-vectors u n))))
        (translate-along-vector p-line n s)))))
  

(defmacro translate (origin &rest offsets)
  "[Macro] 3D Point. Within the context of a GDL object definition (i.e. a <tt>define-object</tt>),
translate <b>origin</b> by any number of <b>offsets</b>.

:arguments (origin \"3D Point\")
:&rest (offsets \"Plist consisting of direction keywords and numbers. A direction keyword can be
one of: <ul><li><tt>:top</tt> (or <tt>:up</tt>)</li>
            <li><tt>:bottom</tt> (or <tt>:down</tt>)</li>
            <li><tt>:left</tt></li>
            <li><tt>:right</tt></li>
            <li><tt>:front</tt></li>
            <li><tt>:rear</tt> (or <tt>:back</tt>)</li></ul>\")"

  (if (null offsets)
      origin
    `(translate (translate-along-vector ,origin 
                                        (the (:face-normal-vector (ecase ,(first offsets)
                                                                    ((:up :top) :top)
                                                                    ((:down :bottom) :bottom)
                                                                    (:left :left)
                                                                    (:right :right)
                                                                    (:front :front)
                                                                    ((:rear :back) :rear))))
                                        ,(second offsets)) ,@(rest (rest offsets)))))



(defun create-obliqueness (v1 name-v1 v2 name-v2 self)
  "3x3 Orthonormal Rotation Matrix. Gives the transform required
to be applied to the parent's orientation to achieve alignment 
indicated by the arguments. The direction keywords are the same
as those used with the GDL <tt>alignment</tt> function.

:arguments (vector-1 \"3D Vector\"
            direction-1 \"Direction Keyword\"
            vector-2 \"3D Vector\"
            direction-2 \"Direction Keyword\"
            self \"GDL object inheriting from <tt>base-object</tt>\")"
  (let* ((parent (the-object self parent))
         (parent-transform (when parent (the-object parent orientation)))
         (inverse (when parent-transform (matrix:transpose-matrix parent-transform)))
         (alignment (alignment name-v1 v1 name-v2 v2)))
    (if inverse (matrix:multiply-matrix alignment inverse) alignment)))


(defun proj-point-on-line (3d-point line-point vector)
  "3D-Point. Drops <b>3d-point</b> onto line containing <b>line-point</b> 
and whose direction-vector is <b>vector</b>.
 
:arguments (3D-point \"3D Point\"
            Line-point \"3D Point\"
            vector \"3D Unit Vector\")"

  (inter-line-plane line-point vector 3d-point vector))


(defun pythagorize (&rest numbers)
  "Number. Returns the square root of the sum of the squares of <i>numbers</i>.

:&rest (numbers \"List of Numbers\")"

  (sqrt (apply #'+ (mapcar #'^2 numbers))))


(defmacro roll (axis angle &rest other-axes-and-angles)
  "[macro] Transformation matrix. In the context of a GDL object 
definition (i.e. in a <tt>define-object</tt>), returns a transformation 
matrix based on rotation about <b>axis</b> by some <b>angle</b>. 
<b>Axis</b> is a keyword symbol, one of: 
<ul><li><tt>:lateral</tt></li>
    <li><tt>:longitudinal</tt></li>
    <li><tt>:vertical</tt></li></ul>
<b>Angle</b> is specified in radians. Any number of axis-angle 
pairs can be specified.

:arguments (axis \"Keyword Symbol\"
            angle \"Number\")
:&rest (other-axes-and-angles \"Plist made from axis keyword symbols and numbers\")"

  (if other-axes-and-angles
      `(multiply-matrices (rotation (the (:axis-vector ,axis)) ,angle)
                          (roll ,@other-axes-and-angles))
    `(rotation (the (:axis-vector ,axis)) ,angle)))


(defun rotation (vector angle)
  "3x3 orthonormal rotation matrix (as a Lisp Array of Numbers).
Returns a transformation matrix based on a rotation by <b>angle</b>,
specified in radians, about an arbitrary <b>vector</b>.

:arguments (vector \"3D Vector\"
            angle \"Number\")"

  (let* ((vector (unitize-vector vector))
         (w1 (get-x vector))
         (w2 (get-y vector))
         (w3 (get-z vector))
         (w-matrix 
          (make-array (list 3 3)
                      :initial-contents `((0d0 ,(- w3) ,w2)
                                          (,w3 0d0 ,(- w1))
                                          (,(- w2) ,w1 0d0))))
         (w-squared (multiply-matrices w-matrix w-matrix)))
    (add-matrices +identity-3x3+
                  (scalar*matrix (sin (- angle)) w-matrix)
                  (scalar*matrix (- 1 (cos (- angle))) w-squared))))


(defun transform-and-translate-point (vector transform trans-vector)
  "3D-Point. Returns the product of <b>vector</b> and <b>transform</b>, 
translated by (i.e. added to) <i>trans-vector</i>.

:arguments (vector \"3D Vector\"
            transform \"3x3 Rotation Matrix\"
            trans-vector \"3D Vector\")

:examples
<pre>
\(let ((transform (make-transform '((0.0 0.0 1.0)
                                    (0.0 1.0 0.0)
                                    (1.0 0.0 0.0))))
       (v (make-vector 1.0 2.0 3.0))
       (t-v (make-vector 3.0 0.0 0.0)))
   (transform-and-translate-point v transform t-v))

 ---> #(6.0 2.0 1.0)
</pre>"
  (add-vectors (transform-numeric-point vector transform) trans-vector))


(defun transform-numeric-point (vector transform)
  "3D-Point. Returns the product of <b>vector</b> and <i>transform</i>.

:arguments (vector \"3D Vector\"
            transform \"3x3 Rotation Matrix\")

:examples
<pre>
\(let ((transform (make-transform '((0.0 0.0 1.0)
                                    (1.0 0.0 0.0)
                                    (0.0 1.0 0.0))))
       (v (make-vector 1.0 2.0 3.0)))
   (transform-numeric-point v transform))

  ---> #(2.0 3.0 1.0)
</pre>"
  (let* ((array
          (multiply-matrices (make-array (list 1 (length vector)) 
                                         :initial-contents (list vector))
                             transform)))
    (make-vector (aref array 0 0) (aref array 0 1) (aref array 0 2))))





(defun mat (matrix index)
  (ecase index
    (0 (aref matrix 0 0)) (1 (aref matrix 0 1)) (2 (aref matrix 0 2))
    (4 (aref matrix 1 0)) (5 (aref matrix 1 1)) (6 (aref matrix 1 2))
    (8 (aref matrix 2 0)) (9 (aref matrix 2 1)) (10 (aref matrix 2 2))))

(defun normalize-quaternion (quaternion)
  (let ((factor (sqrt (+ (^2 (get-x quaternion))
                         (^2 (get-y quaternion))
                         (^2 (get-z quaternion))
                         (^2 (get-w quaternion))))))
    (make-vector (/ (get-x quaternion) factor)
                 (/ (get-y quaternion) factor)
                 (/ (get-z quaternion) factor)
                 (/ (get-w quaternion) factor))))
                         
                         
(defun quaternion-to-rotation (quaternion)
  "Euler rotation represented as a 4D Vector. Transforms <b>quaternion</b> into a 
Euler angle rotation consisting of an arbitrary axis and an angle of rotation about
that axis.

:arguments (quaternion \"Quaternion, represented as a 4D Vector\")"
  
  (when (and quaternion
             (not (and (zerop (get-x quaternion))
                       (zerop (get-y quaternion))
                       (zerop (get-z quaternion)))))
    (setq quaternion (normalize-quaternion quaternion))
    (let* ((qw (get-w quaternion))
           (qx (get-x quaternion))
           (qy (get-y quaternion))
           (qz (get-z quaternion))
           (cos-angle qw)
           (angle (twice (acos qw)))
           (sin-angle (sqrt (- 1.0 (* cos-angle cos-angle)))))
      (when (zerop sin-angle) (setq sin-angle 1))
      (let* ((vector (unitize-vector (make-vector (/ qx sin-angle)(/ qy sin-angle)(/ qz sin-angle))))
             (qx (get-x vector)) (qy (get-y vector)) (qz (get-z vector)))
        (make-vector qx qy qz angle)))))


(defun quaternion-to-matrix (quaternion)
  "3x3 Orthonormal Rotation Matrix. Transforms <b>quaternion</b> into a 3x3 rotation matrix.

:arguments (quaternion \"Quaternion, represented as a 4D Vector\")"
  (let ((x (get-x quaternion))
        (y (get-y quaternion))
        (z (get-z quaternion))
        (w (get-w quaternion)))
    (let ((xx (^2 x))
          (xy (* x y))
          (xz (* x z))
          (xw (* x w))
          (yy (^2 y))
          (yz (* y z))
          (yw (* y w))
          (zz (^2 z))
          (zw (* z w)))
      (make-array (list 3 3)
                  :initial-contents 
                  (list (list (- 1 (twice (+ yy zz))) (twice (- xy zw)) (twice (+ xz yw)))
                        (list (twice (+ xy zw)) (- 1 (twice (+ xx zz))) (twice (- yz xw)))
                        (list (twice (- xz yw)) (twice (+ yz xw)) (- 1 (twice (+ xx yy)))))))))


(defun determinant (3x3-matrix)
  (let ((mat 3x3-matrix))
    (+ (* (aref mat 0 0) (aref mat 1 1) (aref mat 2 2))
       (- (* (aref mat 0 0) (aref mat 1 2) (aref mat 2 1)))
       (- (* (aref mat 0 1) (aref mat 1 0) (aref mat 2 2)))
       (* (aref mat 0 1) (aref mat 1 2) (aref mat 2 0))
       (* (aref mat 0 2) (aref mat 1 0) (aref mat 2 1))
       (- (* (aref mat 0 2) (aref mat 1 1) (aref mat 2 0))))))
          

  

(defun make-quaternion (qx qy qz qw)
    (let ((q (make-array 4 :element-type 'double-float)))
      (setf (aref q 0) (coerce qx 'double-float)
            (aref q 1) (coerce qy 'double-float)
            (aref q 2) (coerce qz 'double-float)
            (aref q 3) (coerce qw 'double-float))
      q))



(defun matrix-to-quaternion (matrix)
  "Quaternion represented as a 4D Vector. Transforms rotation <b>matrix</b> into the corresponding quaternion.

:arguments (matrix \"3x3 Orthonormal Rotation Matrix (as a Lisp Array of Numbers)\")"
  
  (when matrix
    (let ((q (make-array 4 :element-type 'double-float))
          (i 0)
          j k
          (nxt (make-array 3 :initial-contents (list 1 2 0)))
          (tr (+ (aref matrix 0 0) (aref matrix 1 1) (aref matrix 2 2))))

      (if (> tr 0.0)
          (let* ((s (sqrt (+ tr 1.0)))
                 (w (/ s 2.0))
                 (s (/ 0.5 s)))
            
            (setf (aref q 0) (* (- (aref matrix 1 2) (aref matrix 2 1)) s)
                  (aref q 1) (* (- (aref matrix 2 0) (aref matrix 0 2)) s)
                  (aref q 2) (* (- (aref matrix 0 1) (aref matrix 1 0)) s)
                  (aref q 3) w))
        (progn
          
          (when (> (aref matrix 1 1) (aref matrix 0 0)) (setq i 1))
          (when (> (aref matrix 2 2) (aref matrix i i)) (setq i 2))
          (setq j (aref nxt i) k (aref nxt j))
          (let ((s (sqrt (+ (- (aref matrix i i) (+ (aref matrix j j) (aref matrix k k))) 1))))
            (setf (aref q i) (half s))
            (when (not (zerop s)) (setq s (/ 0.5 s)))
            (setf (aref q 3) (* (- (aref matrix j k) (aref matrix k j)) s)
                  (aref q j) (* (+ (aref matrix i j) (aref matrix j i)) s)
                  (aref q k) (* (+ (aref matrix i k) (aref matrix k i)) s)))))
      
      (make-vector (aref q 0) (aref q 1) (aref q 2) (aref q 3))

      )))
      





(defun degrees-to-radians (&rest args)
  "Number. Converts <b>degrees</b> to radians.

:arguments (degrees \"Number\")"
  
  (apply #'degree args))


(defun acosd (theta)
  "Number. Returns the arc cosine of <b>theta</b>, converted into degrees.

:arguments (theta \"Number. An angle in radians\")"

  (radians-to-degrees (acos theta)))

(defun asind (theta)
  "Number. Returns the arc sine of <b>theta</b>, converted into degrees.

:arguments (theta \"Number. An angle in radians\")"
  
  (radians-to-degrees (asin theta)))

(defun atand (theta)
  "Number. Returns the arc tangent of <b>theta</b>, converted into degrees.

:arguments (theta \"Number. An angle in radians\")"
  
  (radians-to-degrees (atan theta)))

(defun normalize-points (points)
  (let ((first (first points)))
    (values
     (mapcar #'(lambda(point) (subtract-vectors point first)) points) first)))


(defun midpoint (point1 point2)
  "3D Point. Returns the barycentric average (i.e. midpoint) 
of <b>point1</b> and <b>point2</b>.

:arguments (point1 \"3D Point\"
            point2 \"3D Point\")"

  (if (coincident-point? point1 point2) point1
      (translate-along-vector point1 (subtract-vectors point2 point1) 
                              (half (3d-distance point1 point2)))))



(defun between? (point start end)
  (same-direction-vectors? (subtract-vectors point start)
                           (subtract-vectors end point)))


(defun curve-parameter-< (param-1 param-2) (< param-1 param-2))

(defun roughly-aligned-vectors? (vector1 vector2 &optional (degree-tolerance 90))
  (< (angle-between-vectors-d vector1 vector2) degree-tolerance))


(defun distance-to-line (point line-point line-vector)
  "Number. Returns shortest distance from point to line."
  (let ((intersection (inter-line-plane line-point line-vector point line-vector)))
    (3d-distance point intersection)))


(defun equi-space-points (start end &key npoints)
  "List of points. Returns a list of equally spaced points between start and end."
  (when (< npoints 2) (error "equi-space-points needs at least two."))
  (let ((direction-vector (subtract-vectors end start))
        (distance (/ (3d-distance start end) (1- npoints)))
        (current start))
    (cons start
          (let (result)
            (dotimes (n (1- npoints) (nreverse result))
              (let ((new (translate-along-vector current direction-vector distance)))
                (push new result) (setq current new)))))))

    

(defun sort-points-along-vector (points vector)
  "List of points. Returns points in order along given vector."
  (let ((base-point (first points)))
    (let ((intersects (mapcar #'(lambda(point) 
                                  (let ((intersect (inter-line-plane base-point vector point vector)))
                                    (list point intersect (let ((raw-distance (3d-distance base-point intersect)))
                                                            (if (same-direction-vectors? (subtract-vectors intersect base-point)
                                                                                         vector)
                                                                raw-distance
                                                              (- raw-distance))))))
                              points)))
      (mapcar #'first (sort intersects #'< :key #'third)))))


(defun bounding-box-from-list (objects &key (local-objects))
  (let ((object-boxes (remove nil (mapcar #'(lambda(object) (when (typep object 'base-object)
                                                              (the-object object bounding-box))) objects)))
        (local-object-boxes (remove nil (mapcar #'(lambda(object) (when (typep object 'base-object)
                                                                    (the-object object local-box))) 
                                                local-objects))))
    (let (xmin ymin zmin xmax ymax zmax)
      (mapcar #'(lambda(box)
                  (let ((min (first box)) (max (second box)))
                    (let ((x-min (get-x min)) (y-min (get-y min)) (z-min (get-z min))
                          (x-max (get-x max)) (y-max (get-y max)) (z-max (get-z max)))
                      (when (or (null xmin) (< x-min xmin)) (setq xmin x-min))
                      (when (or (null ymin) (< y-min ymin)) (setq ymin y-min))
                      (when (or (null zmin) (< z-min zmin)) (setq zmin z-min))
                      (when (or (null xmax) (> x-max xmax)) (setq xmax x-max))
                      (when (or (null ymax) (> y-max ymax)) (setq ymax y-max))
                      (when (or (null zmax) (> z-max zmax)) (setq zmax z-max))))) 
              (append local-object-boxes object-boxes))
      (if xmin
          (list (make-point xmin ymin zmin) (make-point xmax ymax zmax))
        (list (make-point 0 0 0) (make-point 0 0 0))))))

(defun bounding-box-from-points (points)
  (let (xmin ymin zmin xmax ymax zmax)
    (mapcar #'(lambda(point)
                (let ((x (get-x point)) (y (get-y point)) (z (get-z point)))
                  (when (or (null xmin) (< x xmin)) (setq xmin x))
                  (when (or (null ymin) (< y ymin)) (setq ymin y))
                  (when (or (null zmin) (< z zmin)) (setq zmin z))
                  (when (or (null xmax) (> x xmax)) (setq xmax x))
                  (when (or (null ymax) (> y ymax)) (setq ymax y))
                  (when (or (null zmax) (> z zmax)) (setq zmax z)))) points)
    (list (make-point xmin ymin zmin) (make-point xmax ymax zmax))))


(defun remove-plist-keys (plist keys)
  (if (null keys)
      plist
    (remove-plist-keys (remove-plist-key plist (first keys)) (rest keys))))
    

(defun apply-make-point (list)
  "2D, 3D, or 4D point. This function takes a list of two, three, or four numbers rather 
than multiple arguments as with the make-point and make-vector macro. This is equivalent 
to calling the make-point or make-vector macro on the elements of this list.

:arguments (list \"List of 2, 3, or 4 numbers. The coordinates for the point.\")

"
  (make-point (first list) (second list) (third list)))

(defmacro merge-display-controls (display-controls)
  "Plist of display controls. This macro \"merges\" the given display controls list with
that coming as a trickle-down slot from the parent. It will replace any common keys and
add any new keys.

:arguments (display-controls \"Plist. The new display controls to be merged with the defaults from the parent\")

"
  (let ((keys (gensym))
        (reduced (gensym)))
    `(if (the parent)
         (let ((,keys (plist-keys ,display-controls)))
           (let ((,reduced (remove-plist-keys (the parent display-controls) ,keys)))
             (append ,display-controls ,reduced))) ,display-controls)))
       
       



;;
;; FLAG -- there is a simple formula using dot-vectors to compute this directly.
;;         (see http://mathworld.wolfram.com/HessianNormalForm.html)
;;
(defun in-halfspace? (p-plane u-plane p1 p2)
  (let ((intersect (inter-line-plane p1 (subtract-vectors p2 p1) p-plane u-plane)))
    (not (and intersect (between? intersect p1 p2)))))



