(in-package :gdl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :gdl-build))

(define-object site-app (gdl-build:app)
  
  :computed-slots
  ((application-name "gdl-demos")
   (application-class :runtime)
   (modules :smtp :ftp)
   (post-load-form '(progn (ql:quickload :gdl-demos)
		     (ql:quickload :gdl-downloads)
		     (setf (symbol-value (read-from-string 
					  "asdf:*central-registry*")) nil)
		     ;;
		     ;; FLAG -- clear other asdf and quicklisp parameters here
		     ;;
		     ))
		     
   (restart-init-function '(lambda()
			    (setq glisp:*gdl-home* (glisp:current-directory))
			    (gdl:start-gdl :edition :runtime)
			    (setf (symbol-value (read-from-string "www.genworks.com::*smtp-server*")) "localhost")
			    (glisp:set-gs-path)
			    (net.aserve:publish-directory :prefix "/static/"
			     :destination (format nil "~a" (merge-pathnames "static/" glisp:*gdl-home*)))
			    (net.aserve:publish-directory :prefix "/site-static/"
			     :destination (format nil "~a" (merge-pathnames "site-static/" glisp:*gdl-home*)))
			    (net.aserve:publish-directory :prefix "/newsite-static/"
			     :destination (format nil "~a" (merge-pathnames "newsite-static/" glisp:*gdl-home*)))
			    (dolist (init-file (list "gdlinit.cl" ".gdlinit.cl"))
			      (dolist (directory (list glisp:*gdl-home* (user-homedir-pathname)))
				(when (probe-file (merge-pathnames init-file directory))
				  (load (merge-pathnames init-file directory)))))))
   (demo-days nil)))


(defun site-app ()
  
  (require :smtp)
  (require :ftp)
  (ql:quickload :gdl-demos)
  (ql:quickload :gdl-downloads)

  (let ((self (make-object 'site-app)))
    
    (when (probe-file (the destination-directory))
      (glisp:delete-directory-and-files (the destination-directory)))

    (the make!)

    (ensure-directories-exist (merge-pathnames "SMLib8.51/" (the destination-directory)))
    (glisp:copy-file (merge-pathnames "SMLib8.51/smlib.so" glisp:*gdl-home*)
		     (merge-pathnames "SMLib8.51/smlib.so" (the destination-directory)))
    
    (glisp:copy-file (merge-pathnames "demos/gdlinit.cl" glisp:*genworks-source-home*)
		     (merge-pathnames "gdlinit.cl" (the destination-directory)))

    (glisp:copy-directory (merge-pathnames "gdl/gwl/static/" glisp:*genworks-source-home*)
			  (merge-pathnames "static/" (the destination-directory)))
    (glisp:copy-directory (merge-pathnames "demos/site/static/" glisp:*genworks-source-home*)
			  (merge-pathnames "site-static/" (the destination-directory)))
    (glisp:copy-directory (merge-pathnames "demos/newsite/static/" glisp:*genworks-source-home*)
			  (merge-pathnames "newsite-static/" (the destination-directory)))

    
    (excl.osi:command-output (format nil "rsync -zav ~a dcooper8@genworks.com:~~/kitchen/~a/" (the destination-directory) (lastcar (pathname-directory (the destination-directory)))))
    (excl.osi:command-output "ssh dcooper8@genworks.com killall -9 gdl-demos")))


