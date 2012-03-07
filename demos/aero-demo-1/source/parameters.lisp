(in-package :aero)


(defparameter *data-pathname* 
    (make-pathname :directory (append (pathname-directory (asdf:system-source-directory :gdl-aero-demo-1))
				      (list "data"))
		   :device (pathname-device (asdf:system-source-directory :gdl-aero-demo-1))))
