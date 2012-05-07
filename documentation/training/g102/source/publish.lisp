(in-package :training-g102)

(publish :path "/training-g102"
	 :function #'(lambda(req ent)
		       (gwl-make-object req ent "training-g102:assembly")))

(print-variables *load-truename*)



#+nil
(defparameter *images-path* (make-pathname :directory (append (butlast (butlast (pathname-directory *load-truename*)))
							      (list "images"))
					   :device (pathname-device *load-truename*)))
					     
(defparameter *images-path* (merge-pathnames "g102/images/" (glisp:system-home :gdl-training)))


(publish-directory :prefix "/g102/images/"
		   :destination (format nil "~a" *images-path*))



(defun push! ()
  (glisp:run-shell-command "rm -r /tmp/sites/")
  (gwl:crawl "training-g102:assembly")
  (glisp:run-shell-command "mv /tmp/sites/assembly/ /tmp/sites/training-g102/")
  (glisp:run-shell-command "rsync -zav /tmp/sites/training-g102/ genworks.com:kitchen/downloads/training-g102/")
  (glisp:run-shell-command "rm -r /tmp/sites/"))
			   