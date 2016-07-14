(in-package :geom-base)

(defun 3d-point? (point)
  "Boolean. A predicate function to check if a point is 3-dimensional."
  (3d-point-p point)
  )

(defun 3d-vector? (vector)
  "Boolean. A predicate function to check if a vector is 3-dimensional."
  (3d-vector-p vector)
  )

(deftype 3d-point ()
  "Datatype 3d-point is the result of a make-point function using 3 coordinates
  (e.g., X, Y & Z), not an instance of the define-object point"
  '(satisfies 3d-point-p)
  )

(deftype 3d-vector ()
  "Datatype 3d-vector is the result of a make-vector function using 3
  coordinates."
  '(satisfies 3d-vector-p)
  )

(defun 3d-point-p (point)
  "Boolean. 

  FUNCTION
  3d-point-p - predicate function to check if a make-point is 3D.  That is, the
  point has 3 dimensions, representing a 3-dimensional point.

  USAGE
  3d-point-p point

  DESCRIPTION
  A predicate function to check if a point is 3-dimensional.

  The function may also be accessed by calling the function 3d-point?.

  EXAMPLES
  (3d-point-p (make-point 1 2 3))
  --> t

  (3d-point-p (make-point 1 2 3 4))
  --> nil
  "
  (and (arrayp point)
       (eq (array-total-size point) 3)
       (every #'(lambda(entry) (typep entry 'double-float)) point)))

(defun 3d-vector-p (vector)
  "Boolean. 

  FUNCTION
  3d-vector-p - predicate function to check if a vector is 3D.  That is, the
  vector has 3 dimensions, representing a 3-dimensional vector. 

  USAGE
  3d-vector-p vector

  DESCRIPTION
  A predicate function to check if a vector is 3-dimensional.

  The function may also be accessed by calling the function 3d-vector?.

  EXAMPLES
  (3d-vector-p (make-vector 1 2 3))
  --> t

  (3d-vector-p (make-vector 1 2 3 4))
  --> nil
  "
  (and (arrayp vector)
       (eq (array-total-size vector) 3)
       (every #'(lambda(entry) (typep entry 'double-float)) vector)))
