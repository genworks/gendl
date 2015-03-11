(in-package :gdl-lift-tests)


(define-object angle-between-vectors-test ()

  :input-slots 
  ((vector-1 (make-vector 1 0 0))
   (vector-2 (make-vector 0 1 0))
   (vector-3 (make-vector 0 0 1)))


  :computed-slots
  ((vector-3-r (reverse-vector (the vector-3)))

   (regression-test-data (list (angle-between-vectors (the vector-1) (the vector-2))
			       (angle-between-vectors-d (the vector-1) (the vector-2))
			       (angle-between-vectors (the vector-1) (the vector-2) (the vector-3) :negative? t)
			       (angle-between-vectors-d (the vector-1) (the vector-2) (the vector-3) :negative? t)
			       (angle-between-vectors-d (the vector-1) (the vector-2) (the vector-3) t)

			       (angle-between-vectors-d (the vector-1) (the vector-2) (the vector-3) :negative? t :epsilon 0.1)
			       

			       (angle-between-vectors (the vector-1) (the vector-2) (the vector-3-r) :negative? t)
			       (angle-between-vectors-d (the vector-1) (the vector-2) (the vector-3-r) :negative? t)
			       (angle-between-vectors-d (the vector-1) (the vector-2) (the vector-3-r) t)
			       (angle-between-vectors-d (the vector-1) (the vector-2) (the vector-3-r) :negative? t :epsilon 0.1)


			       (angle-between-vectors (the vector-1) (the vector-2) (the vector-3-r) )
			       (angle-between-vectors-d (the vector-1) (the vector-2) (the vector-3-r))
			       (angle-between-vectors-d (the vector-1) (the vector-2) (the vector-3-r))
			       (angle-between-vectors-d (the vector-1) (the vector-2) (the vector-3-r) :epsilon 0.1)

			       ))))


(register-test-definition 'angle-between-vectors-test)
