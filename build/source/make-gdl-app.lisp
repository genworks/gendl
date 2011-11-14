(in-package :gdl-build)

(defun make-gdl-app (&rest args)
  (let ((self (apply #'make-object 'make-gdl-app args)))
    (the make!)))


(define-object make-gdl-app ()
  :input-slots
  (("String. The name which will be used for your application's executable and dxl file. 
Defaults to \"gdl-base.\""
    application-name  "gdl-base")
   
   ("Keyword symbol. Should be one of <tt>:runtime</tt>, <tt>:development</tt>, 
or <tt>:enterprise</tt>. 
Indicates which level of application should be made. Defaults to :development."
    application-class :development)
   
   ("Pathname. Indicates the directory to be created or overwritten for producing 
the distribution. 
Defaults to a directory called <tt>(the application-name)</tt> in the user 
temporary directory, returned by <tt>(glisp:temporary-folder)</tt>."
    destination-directory 
    (merge-pathnames (make-pathname :directory (list :relative (the application-name)))
                     (glisp:temporary-folder) ))
   

   (pre-load-form `(progn (load ,(merge-pathnames "load.lisp" 
						   glisp:*genworks-source-home*))
                           (setf (symbol-value (read-from-string 
						"glisp:*genworks-source-home*")) nil)
                           (setf (symbol-value (read-from-string 
						"asdf:*central-registry*")) nil)))

   (post-load-form nil)
   
   ;;
   ;; FLAG -- leave this to nil for now while we are working on the basic build. 
   ;;
   (init-file-names (list "gdlinit.cl" ".gdlinit.cl"))
   
   (modules nil)
   
   (demo-days 30)
   
   
   
   )
   
   
  :functions
  ((make!
    ()
    (when (probe-file (the destination-directory))
      (glisp:delete-directory-and-files (the destination-directory)))
    (glisp:make-gdl-app   :application-name (the application-name)
                          :destination-directory (the destination-directory)
                          :modules (the modules)
                          :pre-load-form (the pre-load-form)
			  :post-load-form (the post-load-form)
                          :application-class (the application-class)
                          :demo-days (the demo-days)
                          ;;
                          ;; FLAG -- leave this to nil for now while we are working 
			  ;;         on the basic build. 
                          ;;
                          :init-file-names (the init-file-names)
			  ;;
                          ;;:restart-init-function '(lambda() 
			  ;;     (gdl:start-gdl :edition :open-source))
			  ;;
                          ))))



(defun make-gdl-site ()
  (gdl-build:make-gdl-app :post-load-form '(progn 
					    (load "~/genworks-1581-pre-git/gdl/dist/src/demos/site/gdl-site.asd")
					    (asdf:load-system :gdl-site)  
					    (load "~/genworks-1580/gdl/apps/downloads/gdl-downloads.asd")
					    (asdf:load-system :gdl-downloads))
			  :demo-days nil))