(in-package :training-g102)

(publish :path "/training-g102"
	 :function #'(lambda(req ent)
		       (gwl-make-object req ent "training-g102:assembly")))

(publish-directory :prefix "/g102/images/"
		   :destination (format nil "~a" (translate-logical-pathname "~/genworks/gwl-apps/training/g102/images/")))

(publish-directory :prefix "/g102/style/"
		   :destination (format nil "~a" (translate-logical-pathname "~/genworks/gwl-apps/training/g101/style/")))
