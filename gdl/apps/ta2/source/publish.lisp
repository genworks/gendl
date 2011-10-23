(in-package :ta2)

(publish :path "/ta2"
	 :function #'(lambda(req ent)
		       (gwl-make-object req ent "ta2:assembly")))
