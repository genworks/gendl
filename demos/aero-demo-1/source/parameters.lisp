(in-package :aero)


(defparameter *data-pathname* 
    (make-pathname :directory (append (butlast (pathname-directory excl:*source-pathname*))
				      (list "data"))
		   :device (pathname-device excl:*source-pathname*)))
