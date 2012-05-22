(in-package :training-g102-tud)

(publish :path "/training-g102-tud"
	 :function #'(lambda(req ent)
		       (gwl-make-object req ent "training-g102-tud:assembly")))

(print-variables *load-truename*)

(defparameter *images-path* (merge-pathnames "g102-tud/images/" (glisp:system-home :gdl-training)))


(publish-directory :prefix "/g102-tud/images/"
		   :destination (format nil "~a" *images-path*))



(defun push! ()
  (glisp:run-shell-command "rm -r /tmp/sites/")
  (gwl:crawl "training-g102-tud:assembly")
  (glisp:run-shell-command "mv /tmp/sites/assembly/ /tmp/sites/training-g102-tud/")
  (glisp:run-shell-command "rsync -zav /tmp/sites/training-g102-tud/ genworks.com:kitchen/downloads/training-g102-tud/")
  (glisp:run-shell-command "rm -r /tmp/sites/"))
			   