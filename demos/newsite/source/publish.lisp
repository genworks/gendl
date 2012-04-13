(in-package :www.genworks.com)

(let ((static (namestring 
	       (merge-pathnames "newsite/static/" 
				(asdf:system-source-directory "gdl-demos")))))
  (publish-directory :prefix "/newsite-static/"
		     :headers (list (cons :cache-control "86400"))
		     :destination static))


(publish-gwl-app "/newsite" "www.genworks.com:assembly")
(publish-gwl-app "/newsite/" "www.genworks.com:assembly")
(publish-gwl-app "/newsite.html" "www.genworks.com:assembly")
