(in-package :training-g102)

(publish :path "/training-g102"
	 :function #'(lambda(req ent)
		       (gwl-make-object req ent "training-g102:assembly")))

(print-variables *load-truename*)

(defparameter *images-path* (make-pathname :directory (append (butlast (butlast (pathname-directory *load-truename*)))
							      (list "images"))
					   :device (pathname-device *load-truename*)))
					     

(publish-directory :prefix "/g102/images/"
		   :destination (format nil "~a" *images-path*))


