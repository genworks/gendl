(in-package :gdl-user)

(defparameter *global-number* 10)

(defparameter *global-number-lock* (mp:make-process-lock :name "global-number-lock"))

(defun set-global-number (&key (value 20) (n nil))
  (mp:with-process-lock (*global-number-lock*)
    (setq *global-number* value)
    (unless (= *global-number* value)
      (error "*global-number* got reset out from under us! Should be ~a and it is now ~a~a~%" 
	     value *global-number*
	     (if n (format nil " and n is ~a." n) "")))))


(defun start-threads (&key (times 1000000))
  (mp:process-run-function "num-20" #'(lambda()(dotimes (n times) 
						 (when (zerop (mod n 10000))
						   (format t "In 20-setter, n is: ~a~%" n))
						 (set-global-number :value 20 :n n))))

  (mp:process-run-function "num-42" #'(lambda()(dotimes (n times) 
						 (when (zerop (mod n 10000))
						   (format t "In 42-setter, n is: ~a~%" n))
						 (set-global-number :value 42 :n n)))))