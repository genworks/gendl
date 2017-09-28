(in-package :gwl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (#+allegro 
   excl:without-redefinition-warnings 
   #-allegro progn
	   
   (defmacro with-error-handling ((&key (error? nil)
					(timeout 2) 
					(timeout-body 
					 `(warn "Timed Out after ~a Seconds" ,timeout))) 
				  &body body)
     "[Macro]. Wraps the <b>body</b> of code with error-trapping and system timeout. 
A warning is given if an error condition occurs with <b>body</b>. 

:&key ((timeout 2) \"Timeout in Seconds.\"
          timeout-body \"Body of code to evaluate if timeout occurs. 
                         Default is to print a warning and return nil.\")

:&rest (body \"Body of code to be wrapped\")"
  
     (if error? `(progn ,@body)
	 (let ((values (gensym)) (error (gensym)))
	   (let ((code `(let* ((,values (multiple-value-list (ignore-errors ,@body)))
			       (,error (second ,values)))
			  (if (and ,error (typep ,error 'error))
			      (progn (warn "~a" ,error)
				     (values nil ,error))
			      (apply #'values ,values)))))
	     (if timeout 
		 `(,(glisp:with-timeout-sym) (,timeout ,timeout-body) ,code)
		 code)))))))

(defun server-port (&optional (wserver net.aserve:*wserver*))
  (when wserver
    (let ((socket (net.aserve:wserver-socket wserver)))
      (when socket
        (glisp:local-port socket)))))

(defun announce-server-port ()
  (let ((port (server-port)))
    (when port
      (format t "~&~%***** ~%Your Webserver is running on Port ~a.~%*****~%" port))))
