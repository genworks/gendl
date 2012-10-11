(in-package :aero)


(defparameter *data-pathname* 
    (make-pathname :directory (append (pathname-directory (glisp:system-home :gdl-aero-demo-1))
				      (list "data"))
		   :device (pathname-device (glisp:system-home :gdl-aero-demo-1))))
