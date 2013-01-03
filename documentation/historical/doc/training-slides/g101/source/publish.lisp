(in-package :training-g101)

(publish :path "/training-g101"
	 :function #'(lambda(req ent)
		       (gwl-make-object req ent "training-g101:assembly")))

(publish-directory :prefix "/g101/images/"
		   :destination (format nil "~a" (translate-logical-pathname "~/genworks/gwl-apps/training/g101/images/")))

(publish-directory :prefix "/g101/style/"
		   :destination (format nil "~a" (translate-logical-pathname "~/genworks/gwl-apps/training/g101/style/")))
