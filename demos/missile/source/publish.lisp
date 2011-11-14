(in-package :missile)

(publish :path "/missile"
	 :function #'(lambda(req ent)
		       (gwl-make-object req ent "missile:assembly")))
