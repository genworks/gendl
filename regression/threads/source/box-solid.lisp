(in-package :gdl-lift-tests)

(defparameter *box-solid-lock* (mp:make-process-lock :name "box-solid-lock"))

(defparameter *collected-breps* nil)

(defparameter *collected-breps-old* nil)

(defun make-and-check-box-solid (&key (value 20) (n nil))

    (let (self)
      (mp:with-process-lock (smlib::*smlib-lock*)
	(setq self (make-object 'box-solid :length value :width value :height value))
	(the %native-brep%))
      (mp:with-process-lock (smlib::*smlib-lock*)
	(unless (near-to? (the volume) (expt value 3))
	  (error "box dimensions got reset out from under us! Should be ~a and it is now ~a~a~%" 
		 (expt value 3) (the volume)
		 (if n (format nil " and n is ~a." n) ""))))))

(defun delete-collected-breps ()
  (mp:process-run-function "delete-breps" #'(lambda()
					      (dolist (brep *collected-breps*)
						(mp:with-process-lock (smlib::*smlib-lock*)
						  (smlib::delete-iwbrep brep))))))


(defun start-box-threads (&key (times 2000))
  
  
  (let ((*geometry-kernel* (make-instance (read-from-string 
					   "smlib::geometry-kernel")
				  :iw-context (funcall (read-from-string "smlib::make-iw-context")))))
    (mp:process-run-function "box-20" #'(lambda()(dotimes (n times) 
						   (when (zerop (mod n 100))
						     (format t "In 20-setter, n is: ~a~%" n))

						   (make-and-check-box-solid :value 20 :n n)))))

  (let ((*geometry-kernel* (make-instance (read-from-string 
					   "smlib::geometry-kernel")
					  :iw-context (funcall (read-from-string "smlib::make-iw-context")))))
    (mp:process-run-function "num-42" #'(lambda()(dotimes (n times) 
						   (when (zerop (mod n 100))
						     (format t "In 24-setter, n is: ~a~%" n))
						   (make-and-check-box-solid :value 24 :n n)))))

  (let ((*geometry-kernel* (make-instance (read-from-string 
					   "smlib::geometry-kernel")
					  :iw-context (funcall (read-from-string "smlib::make-iw-context")))))
    (mp:process-run-function "num-42" #'(lambda()(dotimes (n times) 
						   (when (zerop (mod n 100))
						     (format t "In 42-setter, n is: ~a~%" n))
						   (make-and-check-box-solid :value 42 :n n))))))
