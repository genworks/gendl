
(in-package :gwl-user)

(net.aserve:shutdown)

(net.aserve:start :port 80 :listeners 50)

(publish-gwl-app "/" "genworks.com:assembly")

(publish-gwl-app "/newsite" "www.genworks.com:assembly")

(dolist (host (list "cl-foundation.com" "cl-foundation.org"
						"www.cl-foundation.com" "www.cl-foundation.org"))

			      (net.aserve:publish-directory 		
			       :prefix "/aclu_files/"
			       :host host
			       :destination (format nil "~a" 
						    (merge-pathnames "static-old/aclu_files/" 
								     glisp:*gdl-home*)))
			      (net.aserve:publish-file 
			       :path "/"
			       :host host
			       :file (format nil "~a" (merge-pathnames "static-old/aclu.html"
								       glisp:*gdl-home*))))

(net.aserve:publish-directory :prefix "/downloads/" 
			      :destination (format nil "~a" (merge-pathnames "downloads/" glisp:*gdl-home*)))

(excl.osi:setgid 1002)
(excl.osi:setuid 1002)

(setq gwl:*developing?* nil)

(format t "Now Sleeping main thread and being a webserver...~%")
(do () () (sleep 10000))


