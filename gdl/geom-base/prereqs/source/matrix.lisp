;;
;; Code here is from CMU AI repository.
;;
;;
;; http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/lisp/code/math/matrix/0.html
;;
;;

(eval-when (compile load eval)
  (defpackage :matrix (:export #:matrixp #:num-rows #:num-cols #:square-matrix? #:make-matrix
                               #:make-identity-matrix #:copy-matrix #:print-matrix
                               #:transpose-matrix #:multiply-matrix #:add-matrix #:subtract-matrix
                               #:invert-matrix #:solve-matrix)))

(in-package :matrix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Matrix operations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun matrixp (matrix)
  "Test whether the argument is a matrix"
  (and (arrayp matrix)
       (= (array-rank matrix) 2)))

(defun num-rows (matrix)
  "Return the number of rows of a matrix"
  (array-dimension matrix 0))

(defun num-cols (matrix)
  "Return the number of rows of a matrix"
  (array-dimension matrix 1))

(defun square-matrix? (matrix)
  "Is the matrix a square matrix?"
  (and (matrixp matrix)
       (= (num-rows matrix) (num-cols matrix))))
       
(defun make-matrix (rows &optional (cols rows))
  "Create a matrix filled with zeros.  If only one parameter is
  specified the matrix will be square."
  (make-array (list rows cols) :initial-element 0))

(defun make-identity-matrix (size)
  "Make an identity matrix of the specified size."
  (let ((matrix (make-array (list size size) :initial-element 0)))
    (dotimes (i size matrix)
      (setf (aref matrix i i) 1))))

(defun copy-matrix (matrix)
  "Return a copy of the matrix."
  (let* ((rows (num-rows matrix))
         (cols (num-cols matrix))
         (copy (make-array (list rows cols))))
    (dotimes (row rows copy)
      (dotimes (col cols)
        (setf (aref copy row col) (aref matrix row col))))))

(defun print-matrix (matrix &optional (destination t) (control-string "~20S"))
  "Print a matrix.  The optional control string indicates how each
  entry should be printed."
  (let ((rows (num-rows matrix))
        (cols (num-cols matrix)))
    (dotimes (row rows)
      (format destination "~%")
      (dotimes (col cols)
        (format destination control-string (aref matrix row col))))
    (format destination "~%")))

(defun transpose-matrix (matrix)
  "Transpose a matrix"
  (let* ((rows (num-rows matrix))
         (cols (num-cols matrix))
         (transpose (make-matrix cols rows)))
    (dotimes (row rows transpose)
      (dotimes (col cols)
        (setf (aref transpose col row)
              (aref matrix row col))))))

(defun multiply-matrix (&rest matrices)
  "Multiply matrices"
  (labels ((multiply-two (m1 m2)
             (let* ((rows1 (num-rows m1))
                    (cols1 (num-cols m1))
                    (cols2 (num-cols m2))
                    (result (make-matrix rows1 cols2)))
               (dotimes (row rows1 result)
                 (dotimes (col cols2)
                   (dotimes (i cols1)
                     (setf (aref result row col)
                           (+ (aref result row col)
                              (* (aref m1 row i)
                                 (aref m2 i col))))))))))
    (when matrices                      ; Empty arguments check
      (reduce #'multiply-two matrices))))

(defun add-matrix (&rest matrices)
  "Add matrices"
  (labels ((add-two (m1 m2)
             (let* ((rows (num-rows m1))
                    (cols (num-cols m1))
                    (result (make-matrix rows cols)))
               (dotimes (row rows result)
                 (dotimes (col cols)
                   (setf (aref result row col)
                         (+ (aref m1 row col)
                            (aref m2 row col))))))))
    (when matrices                      ; Empty arguments check
      (reduce #'add-two matrices))))

(defun subtract-matrix (&rest matrices)
  "Subtract matrices"
  (labels ((subtract-two (m1 m2)
             (let* ((rows (num-rows m1))
                    (cols (num-cols m1))
                    (result (make-matrix rows cols)))
               (dotimes (row rows result)
                 (dotimes (col cols)
                   (setf (aref result row col)
                         (- (aref m1 row col)
                            (aref m2 row col))))))))
    (when matrices                      ; Empty arguments check
      (reduce #'subtract-two matrices))))

(defun invert-matrix (matrix &optional (destructive nil))
  "Find the inverse of a matrix.  By default this operation is
  destructive.  If you want to preserve the original matrix, call this
  function with an argument of NIL to destructive.

  changed default to NIL so it is not destructive -- DJC."

  (let ((result (if destructive matrix (copy-matrix matrix)))
        (size (num-rows matrix))
        (temp 0))
    (dotimes (i size result)
      (setf temp (aref result i i))
      (dotimes (j size)
        (setf (aref result i j)
              (if (= i j)
                  (/ (aref result i j))
                (/ (aref result i j) temp))))
      (dotimes (j size)
        (unless (= i j)
          (setf temp (aref result j i)
                (aref result j i) 0)
          (dotimes (k size)
            (setf (aref result j k)
                  (- (aref result j k)
                     (* temp (aref result i k))))))))))

(defun exchange-rows (matrix row-i row-j)
  "Exchange row-i and row-j of a matrix"
  (let ((cols (num-cols matrix)))
    (dotimes (col cols)
      (rotatef (aref matrix row-i col) (aref matrix row-j col)))))


(defun eliminate-matrix (matrix rows cols)
  "Gaussian elimination with partial pivoting.  "
  ;; Evaluated for side effect.  A return value of :singular indicates the
  ;; matrix is singular (an error).
  (let ((max 0))
    (loop for i below rows
     do (setf max i)
     do (loop for j from (1+ i) below rows
         do (when (> (abs (aref matrix j i))
                     (abs (aref matrix max i)))
              (setf max j)))
     do (when (zerop (aref matrix max i))
          (return-from eliminate-matrix :singular)) ; error "Singular matrix"
     do (loop for k from i below cols   ; Exchange rows
         do (rotatef (aref matrix i k) (aref matrix max k)))
     do (loop for j from (1+ i) below rows
         do (loop for k from (1- cols) downto i
             do (setf (aref matrix j k)
                      (- (aref matrix j k)
                         (* (aref matrix i k)
                            (/ (aref matrix j i)
                               (aref matrix i i)))))
               )))
    matrix))


(defun substitute-matrix (matrix rows cols)
  (let ((temp 0.0)
        (x (make-array rows :initial-element 0)))
    (loop for j from (1- rows) downto 0
     do (setf temp 0.0)
     do (loop for k from (1+ j) below rows
         do (incf temp (* (aref matrix j k) (aref x k))))
     do (setf (aref x j) (/ (- (aref matrix j (1- cols)) temp) 
                            (aref matrix j j))))
    x))

(defun solve-matrix (matrix &optional (destructive t) print-soln)
  "Solve a matrix using Gaussian elimination
   Matrix must be N by N+1
   Assume solution is stored as the N+1st column of the matrix"
  (let ((rows (num-rows matrix))
        (cols  (num-cols matrix))
        (result (if destructive matrix (copy-matrix matrix))))
    (unless (= (1+ rows) cols)
      (error "Ill formed matrix"))      ; Cryptic error message
    (cond ((eq :singular (eliminate-matrix result rows cols)))
          (t (let ((soln (substitute-matrix result rows cols)))
               (when print-soln
                 (loop for i below rows
                  do (format t "~% X~A = ~A" i (aref soln i))))
               soln)))))

(provide :matrixops)
