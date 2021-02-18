(in-package :gdl)

#-allegro
(define-object gdl-app ()
  :documentation (:description "
This object serves as the driver for the build process for GDL runtime
applications. There is also an undocumented function called
<tt>make-gdl-app</tt>; in order to perform a runtime build process,
simply make an instance of this object with the appropriate input
values, and invoke <tt> (the make!)  </tt> on it, or call
<tt>make-gdl-app</tt> with the same arguments as the input-slot you
give to this object.
"
                  
			       :examples "<pre>

 (in-package :gdl-user)

 (make-gdl-app :application-name \"moon-shot\" :destination-directory  \"/tmp/moon-shot/\" 
               :overwrite? t :application-fasls (list \"/fasl-home/booster-rocket.fasl\" 
                                                      \"/fasl-home/lunar-module.fasl\"))

</pre>
"
                  
			       :author "Dave Cooper, Genworks International")



  :input-slots
  ((build-level :runtime)

   ("String. The name which will be used for your
                 application's executable and possibly image file. Defaults to
                 \"gdl-test-runtime\"."  application-name "gdl-test-runtime")



   ("Lisp Function of zero arguments. 
This function will be run in the initiating image before the build is begun."
    pre-make-function nil)


   ("Lisp Function of zero arguments. 
This function will be run in the initiating image after the build is finished."
    post-make-function nil)



   ("Lisp expression. This form will be evaluated in the image being
       built, before the loading of application-fasls begins, but
       after the GDL runtime modules are loaded.  Defaults to nil."
    pre-load-form nil)

   ("Lisp expression. This form will be evaluated in the image being
       built, after the loading of application-fasls is complete.
       Defaults to nil."
    post-load-form nil)

     
   ("Boolean. Indicates whether a build will overwrite a previously
       existing destination directory. Defaults to nil."
    overwrite? nil)


   ("Pathname. Indicates the directory to be created or overwritten
       for producing the runtime distribution.  Defaults to a
       directory called <tt>(the application-name)</tt> in the system
       temporary directory, returned by
       <tt>(glisp:temporary-folder)</tt>."
    destination-directory
    (merge-pathnames 
     (make-pathname :directory (list :relative (the application-name)))
     (glisp:temporary-folder)))


   (display-string (string-capitalize (the application-name)))

   ("Lambda expression with empty argument list or symbol naming a
       function with no arguments. This will be run when the runtime
       application starts up. The alternative to using this to achieve
       initializations is to put expressions in a gdlinit.cl or
       .gdlinit.cl in the application directory or user home
       directory. Defaults to nil."  restart-init-function nil)
     

   ("List of pathnames. This list should contain the pre-compiled
       fasls for your GDL application, in correct load order. These
       can be produced, for example, by calling
       <tt>genworks:cl-lite</tt> with the <tt>:create-fasl?</tt>
       keyword argument set to <tt>t</tt>. If you are using the ASDF
       build management system, note that ASDF3 is now capable of
       producing a single fasl file for your application including its
       ASDF/Quicklisp dependencies, using <pre>
         (asdf:operate 'asdf:monolithic-compile-bundle-op :your-application-system-name)
         (asdf:output-file 'asdf:monolithic-compile-bundle-op :your-application-system-name)
    </pre>

               See the ASDF documentation for details. "
    application-fasls nil)


   ;;
   ;; FLAG -- implement this.
   ;;
   ("Number. The size of the reserved space which will be requested
       from the OS when the produced application starts up.  Defaults
       to 800000000 (eight hundred million) bytes."  lisp-heap-size
       800000000)

   ("String. The contents of this string will be copied to a file
       gdlinit.cl and placed in the destination-directory. Default is
       empty string."
    gdlinit-content ""))

  :computed-slots
  ((destination-exe (merge-pathnames (format nil "~a~a"
					     (the application-name)
					     #+windows ".exe" #-windows "")
				     (the destination-directory)))
					      

   (parent-directory (merge-pathnames "../" (the destination-directory)))

   (load-fasls-expression (when (the application-fasls)
			    `(mapcar #'load ',(the application-fasls))))



     

   (save-appliction-and-die-form
    #+ccl
    `(progn
       (ccl:save-application (progn (ensure-directories-exist ,(the destination-exe))
				    ,(the destination-exe))
			     :prepend-kernel t

			     ;;:clear-clos-caches t
			     ;;:purify t
			       
			     :toplevel-function ,(the toplevel-function))
       (ccl:quit))

    #-ccl (error "Please implement save-application-and-die-form for ~a.~%"
		 (lisp-implementation-type)))



   (toplevel-function `(lambda ()
			 ,@(remove 
			    nil
			    `((uiop:setup-temporary-directory)
			      ;;
			      ;; FLAG -- sort out initialization so we don't have to do it surgically/internally like this.
			      ;;
			      (gendl::initialize)
			      (setq glisp:*gdl-home* glisp:*gdl-program-home*)
			      ;;
			      ;; FLAG -- supposed to come from gwl:initialize but need to customize for now
			      ;;
			      (glisp:initialize-multiprocessing)
			      (when (and (find-package :zacl) (find-package :net.aserve))
				(setq excl:*initial-terminal-io* *terminal-io*)
				(setf (symbol-value (read-from-string "net.aserve:*wserver*"))
				      (make-instance (read-from-string "net.aserve:wserver"))))
			      (setq *iid-random-state* (make-random-state t))
			      (glisp:set-settings gwl::*settings*)
			      ;;
			      ;; end of stuff from gwl:initialize
			      ;;
			      ;;
			      ;; FLAG -- make all this boilerplate into a funciton
			      ;;

			      ,(when (member (the build-level)
					     (list :runtime :pro :geom-base :gwl-graphics)) 
				 `(setq pdf::*cl-pdf-base-directory* glisp:*gdl-program-home*))
			      ,(when (member (the build-level)
					     (list :runtime :pro :geom-base :gwl-graphics))
				 `(setq pdf::*afm-files-directories*
					(list (merge-pathnames "afm/" pdf::*cl-pdf-base-directory*))))
			      ,(when (member (the build-level)
					     (list :runtime :pro :geom-base :gwl-graphics))
				 `(setq cl-typesetting-hyphen::*cl-typesetting-base-directory*
					glisp:*gdl-program-home*))
			      ,(when (member (the build-level)
					     (list :runtime :pro :geom-base :gwl-graphics))
				 `(setq cl-typesetting-hyphen::*hyphen-patterns-directory* 
					(merge-pathnames
					 "hyphen-patterns/"
					 cl-typesetting-hyphen::*cl-typesetting-base-directory*)))
			      ,(when (member (the build-level)
					     (list :runtime :pro :geom-base :gwl-graphics))
				 `(cl-typesetting::initialize!
				   :afm-files-directories pdf::*afm-files-directories*
				   :hyphen-patterns-directory 
				   cl-typesetting-hyphen::*hyphen-patterns-directory*))

			      ,(when (the restart-init-function) `(funcall #',(the restart-init-function)))
				
			      (gdl::load-gdl-init-files))))))


  :functions
  ((copy-with-warning 
    (source target &key (overwrite? t))
    (if (not (and source (probe-file source)))
	(warn "Could not find ~a.~%Not copying to ~a~%" source target)
	(progn
	  (when (and overwrite? (probe-file target))
	    (uiop:delete-directory-tree
	     target :validate #'(lambda(dir)
				  (search (namestring (the destination-directory))
					  (namestring dir)))))
	  (glisp:copy-directory source target))))

   (populate-static-folders
    ()
    (ensure-directories-exist (the destination-directory))
      

    (when (and (the gdlinit-content) (not (zerop (length (the gdlinit-content)))))
      (with-open-file (out (merge-pathnames "gdlinit.cl" (the destination-directory))
			   :direction :output :if-exists :supersede :if-does-not-exist :create)
	(write-string (the gdlinit-content) out)))
      

    (when (member (the build-level) (list :pro :runtime :gwl-graphics :gwl))
      (the (copy-with-warning (merge-pathnames "static/" glisp:*gdl-home*)
			      (merge-pathnames "static/" (the destination-directory)))))


    (when (member (the build-level) (list :pro :runtime :gwl-graphics :geom-base))
      (the (copy-with-warning (or (probe-file (merge-pathnames "afm/" glisp:*gdl-home*))
				  (when (find-package :asdf)
				    (probe-file (merge-pathnames "afm/"
								 (glisp:system-home :cl-pdf)))))
			      (merge-pathnames "afm/" (the destination-directory))))

      (the (copy-with-warning (or (probe-file (merge-pathnames "hyphen-patterns/" glisp:*gdl-home*))
				  (when (find-package :asdf)
				    (probe-file
				     (merge-pathnames "hyphen-patterns/"
						      (glisp:system-home :cl-typesetting)))))
			      (merge-pathnames "hyphen-patterns/" (the destination-directory))))))


   ("Void. Does the application build and creates or replaces 
  <tt>(the destination-directory)</tt> ."
    make!
    ()

    (when (the pre-make-function) (funcall (the pre-make-function)))
      
    (let ((load-file (merge-pathnames (format nil "~a-load.lisp" (gensym)) (glisp:temporary-folder))))
      (with-open-file (out load-file :direction :output :if-exists :supersede
			   :if-does-not-exist :create)
	(when (the pre-load-form) (print (the pre-load-form) out))
	(when (the load-fasls-expression) (print (the load-fasls-expression) out))
	(when (the post-load-form) (print (the post-load-form) out))
	(print (the save-appliction-and-die-form) out))
      
	
	
      ;;
      ;; Now fire up the appropriate executable with argument to load the above
      ;; load-file
      ;;

      #+ccl

      (multiple-value-bind
	    (output error return-code)
	  (uiop:run-program 
	   (list (first (glisp:basic-command-line-arguments))
		 "-n" "--batch" "-l" (namestring load-file) "-e" "(ccl:quit)")
	   :ignore-error-status t
	   :error-output :string
	   :output nil)
	(declare (ignore output))
	(unless (zerop return-code) (print error) (ccl:quit return-code)))
	  
	
      #+nil
      (uiop:run-program 
       (list (first (glisp:basic-command-line-arguments))
	     "-n" "--batch" "-l" (namestring load-file) "-e" "(ccl:quit)")
       :ignore-error-status t
       :error-output :string
       :output :string)

      #-ccl (error "Please implement make! for ~a." (lisp-implementation-type))

      (the populate-static-folders)
	
      (when (the post-make-function) (funcall (the post-make-function)))))))
