(in-package :training-g102)

(publish :path "/training-g105"
	 :function #'(lambda(req ent)
		       (gwl-make-object req ent "training-g105:assembly")))

