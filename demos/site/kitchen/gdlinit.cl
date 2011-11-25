(in-package :gdl-user)

(defparameter *local-dir* (make-pathname :directory (pathname-directory *source-pathname*)
					 :device (pathname-device *source-pathname*)))


(dolist (host (list "localhost" "genworks.com" "www.genworks.com"))
  
  (net.aserve:publish :host host 
		      :path "/products/demos/index.html" 
		      :function #'(lambda(req ent) (gwl::gwl-make-object req ent "genworks.com:assembly"))
		      ;;:object-type 'genworks.com:assembly
		      )
  
  (net.aserve:publish :host host 
		      :path "/" 
		      :function #'(lambda(req ent) (gwl::gwl-make-object req ent "genworks.com:assembly"))
		      ;;:object-type 'genworks.com:assembly
		      ))

#+nil
(dolist (host (list "localhost" "genworks.com" "www.genworks.com"))
  (gwl::publish-shared :host host 
		       :path "/" 
		       ;;:function #'(lambda(req ent) (gwl::gwl-make-object req ent "genworks.com:assembly"))
		       :object-type 'genworks.com:assembly))

(dolist (host (list "localhost" "genworks.com" "www.genworks.com"))
  (gwl::publish-gwl-app "/" 
		       ;;:function #'(lambda(req ent) (gwl::gwl-make-object req ent "genworks.com:assembly"))
		       'genworks.com:assembly))


(dolist (host (list "cl-users.com" "cl-users.org" "cl-users.net"
		    "www.cl-users.com" "www.cl-users.org" "www.cl-users.net"
		    "cl-foundation.com" "cl-foundation.org"
		    "www.cl-foundation.com" "www.cl-foundation.org"))
  
  
  (net.aserve:publish-directory 
   :prefix "/aclu_files/"
   :host host
   :destination (format nil "~a"
			(or (probe-file "./static/aclu_files/")
			    (probe-file "~/genworks/gdl/dist/src/demos/site/static/aclu_files/"))))
  
  (net.aserve:publish-file :path "/"
			   :host host
			   :file (format nil "~a"
					 (or (probe-file "./static/aclu.html")
					     (probe-file "~/genworks/gdl/dist/src/demos/site/static/aclu.html"))))

  (net.aserve:publish-file :path "/googlehostedservice.html"
			   :host host
			   :file "./static/googlehostedservice.html")

  
  )


(let ((static (format nil "~a"
		      (or (probe-file "./static/")
			  (probe-file "~/genworks/gdl/dist/src/demos/site/static/")))))
  (print-variables static)
  (net.aserve:publish-directory :prefix "/site-static/"
				:destination static))

(let ((downloads "./downloads/"))
  (net.aserve:publish-directory :prefix "/downloads/"
				:destination downloads))

(let ((contracts "./contracts/"))
  (net.aserve:publish-directory :prefix "/contracts/"
				:destination contracts))


(defun start-root-gwl (&key (port 80))
  (let* ((start-port 80))
    (format t "~&~%Starting AllegroServe on ~a...~%~%" start-port)
    (net.aserve:start :port start-port 
		      :listeners 50
		      :setuid 1002 :setgid 1002)))

(when (zerop (excl.osi:getuid))
  (start-root-gwl))

(make-geometry-kernel :smlib)

(when (fboundp 'user::start-telnet-server)
  (user::start-telnet-server))


(publish-directory :prefix "/images/gwl/"
		   :destination (namestring (merge-pathname "static/gwl/images/" (translate-logical-pathname "sys:"))))

(publish-directory :prefix "/static/"
		   :destination (namestring (merge-pathname "static/" (translate-logical-pathname "sys:"))))

(publish-directory :prefix "/style/"
		   :destination (namestring (merge-pathname "static/style/" (translate-logical-pathname "sys:"))))




(do ()() (sleep 10000))
