(in-package :training-g108)

(publish :path "/training-g108"
	 :function #'(lambda(req ent)
		       (gwl-make-object req ent "training-g108:assembly")))

(defparameter *images-path* (merge-pathnames "g108/images/" 
					     (asdf:system-source-directory "gdl-training")))


(publish-directory :prefix "/g108/images/"
		   :destination (format nil "~a" *images-path*))


