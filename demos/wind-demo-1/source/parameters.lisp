(in-package :wind)


(defparameter *data-pathname* 
    (make-pathname :directory (append (butlast (pathname-directory (glisp:source-pathname)))
				      (list "data"))
		   :device (pathname-device (glisp:source-pathname))))
