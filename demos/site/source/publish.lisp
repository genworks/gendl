(in-package :genworks.com)


(let ((static (namestring 
	       (merge-pathnames "site/static/" 
				(asdf:system-source-directory "gdl-demos")))))
  (publish-directory :prefix "/site-static/"
		     :destination static))


(publish-gwl-app "/" "genworks.com:assembly")

#+nil
(dolist (host (list "cl-users.com" "cl-users.org" "cl-users.net"
		    "www.cl-users.com" "www.cl-users.org" "www.cl-users.net"))
  (publish-file :path "/index.html" 
		:host host
		:file (merge-pathnames "../static/aclu.html" *source-pathname*)))



