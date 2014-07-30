(in-package :training-g101)

(publish :path "/training-g101"
	 :function #'(lambda(req ent)
		       (gwl-make-object req ent "training-g101:assembly")))


(publish-directory :prefix "/g101/images/"
		   :destination (format nil "~a" (translate-logical-pathname "~/genworks/gendl/documentation/training/g101/images/")))


(publish-directory :prefix "/g101/style/"
		   :destination (format nil "~a" (translate-logical-pathname "~/genworks/gendl/documentation/training/g101/style/")))


(defun make! ()
  (gwl:crawl "training-g101:assembly")
  (glisp:rsync "/tmp/sites/assembly/"
	       "genworks.com:kitchen/downloads/training-g101/"
	       :options (list "zav")))
	       
