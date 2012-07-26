;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;

;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This source file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with this source file.  If not, see
;; <http://www.gnu.org/licenses/>.
;; 

(in-package :gdl-build)

(define-object app ()
  
  :computed-slots ((hey-now "hey now"))
  
  :input-slots
  (("String. The name which will be used for your application's executable and dxl file. 
Defaults to \"my-gdl-app\""
    application-name  "my-gdl-app")
   
   ("Keyword symbol. Should be one of <tt>:runtime</tt>, <tt>:development</tt>, 
or <tt>:enterprise</tt>. 
Indicates which level of application should be made. Defaults to :development."
    application-class :runtime)
   
   (lisp-heap-size 900000000)

   (implementation-identifier (glisp:implementation-identifier))

   ("Pathname. Indicates the directory to be created or overwritten for producing 
the distribution. 
Defaults to a directory called <tt>(the application-name)</tt> in the user 
temporary directory, returned by <tt>(glisp:temporary-folder)</tt>."
    destination-directory 
    (make-pathname :name nil :type nil 
		   :directory (append (pathname-directory (glisp:temporary-folder)) 
				      (ensure-list (the application-name)))
		   :defaults (glisp:temporary-folder)))

   (overwrite? t)

   (load-core-gdl-form `(progn (load ,(merge-pathnames "../build"
						   glisp:*genworks-source-home*))
			       #+nil
			       (setf (symbol-value (read-from-string 
						    "asdf:*central-registry*")) nil)))

   (pre-load-form (the load-core-gdl-form))

   (post-load-form '(progn (setf (symbol-value (read-from-string 
						"asdf:*central-registry*")) nil)
		     (asdf:map-systems #'asdf:clear-system)
		     (asdf:clear-output-translations)
		     (asdf:disable-output-translations)

		     ))

   
   (init-file-names nil)
   
   (restart-init-function nil)
   
   (modules nil)
   
   (demo-days 30)
   
   )
   
   
  :functions
  ((make!
    ()

    (print-variables (the init-file-names))

    (when (and (probe-file (the destination-directory)) (the overwrite?))
      (glisp:delete-directory-and-files (the destination-directory)))
    (glisp:make-gdl-app   :application-name (the application-name)
                          :destination-directory (the destination-directory)
                          :modules (the modules)
                          :pre-load-form (the pre-load-form)
			  :post-load-form (the post-load-form)
                          :application-class (the application-class)
                          :demo-days (the demo-days)
			  :lisp-heap-size (the lisp-heap-size)
                          ;;
                          ;; FLAG -- leave this to nil for now while we are working 
			  ;;         on the basic build. 
                          ;;
                          :init-file-names (the init-file-names)
			  ;;
                          :restart-init-function (the restart-init-function)
			  ;;
			  ;;
                          ))))


(defun app (&rest args)
  (let ((self (apply #'make-object 'app args)))
    (the make!)))

;; 
;; 
;;  FLAG -- this is given as an example for how one might create a
;;  standalone runtime executable. Note that its restart-init-function
;;  modifies the Quicklisp environment, so you would only do this when
;;  the user does not already have a known Quicklisp environment. 
;; 
;;
(defun gdl-runtime ()
  (let ((destination-directory 
	 (let ((implementation-identifier (glisp:implementation-identifier))
	       ;;(prefix (merge-pathnames "../../staging/" glisp:*genworks-source-home*))
	       (prefix (merge-pathnames #+mswindows "e:/staging/" #-mswindows "~/share/staging/")))
	   (merge-pathnames 
	    (make-pathname :directory 
			   (list :relative 
				 (format nil "~a-~a" implementation-identifier
					 (glisp:next-datestamp prefix implementation-identifier)))) prefix))))
    (app :application-name "gdl-runtime"
	 :application-class :runtime
	 :destination-directory destination-directory
	 :modules (list :agraph)
	 :restart-init-function '(lambda()
				  (setq glisp:*gdl-home* (glisp:current-directory))
				  (setq glisp:*genworks-source-home* (merge-pathnames "src/" glisp:*gdl-home*))
				  (setf (symbol-value (read-from-string "ql:*quicklisp-home*")) (merge-pathnames "quicklisp/" glisp:*gdl-home*))
				  (gdl:start-gdl :edition :trial)
				  (glisp:set-gs-path (merge-pathnames "gpl/gs/gs8.63/bin/gswin32c.exe" glisp:*gdl-home*))))

    destination-directory))


(defun gdl ()
  (let ((destination-directory 
	 (let ((implementation-identifier (glisp:implementation-identifier))
	       ;;(prefix (merge-pathnames "../../staging/" glisp:*genworks-source-home*))
	       (prefix (merge-pathnames #+mswindows (or (probe-file "e:/staging/")
							(probe-file "z:/staging/"))
					#-mswindows "~/share/staging/")))
	   (merge-pathnames 
	    (make-pathname :directory 
			   (list :relative 
				 (format nil "~a-~a" implementation-identifier
					 (glisp:next-datestamp prefix implementation-identifier)))) prefix))))
    (app :application-name "gdl"
	 :application-class #+64bit :runtime #-64bit :development
	 :destination-directory destination-directory
	 :modules (list #-64bit :agraph #-64bit :ide)
	 :restart-init-function '(lambda()
				  (gdl:start-gdl :edition :trial)))

    destination-directory))


(defun site ()
  (app :post-load-form 
       '(progn 
	 (require :smtp)
	 (require :ftp)
	 (load (merge-pathnames "demos/gdl-demos.asd" glisp:*genworks-source-home*))
	 (asdf:load-system :gdl-demos)  
	 (load (merge-pathnames "../downloads/gdl-downloads.asd" glisp:*genworks-source-home*))
	 (asdf:load-system :gdl-downloads)
	 (load (merge-pathnames "demos/newsite/gdl-newsite.asd" glisp:*genworks-source-home*))
	 (asdf:load-system :gdl-newsite)
	 (setf (symbol-value (read-from-string 
			      "glisp:*genworks-source-home*")) nil))
       :destination-directory 
       (merge-pathnames "~/share/staging/site/" glisp:*genworks-source-home*)
       :overwrite? t
       :application-name "genworks-site"
       :application-class :development
       :demo-days nil
       :restart-init-function '(lambda()
				(gdl:start-gdl :edition :trial)
				(setf (symbol-value (read-from-string "www.genworks.com::*smtp-server*")) "localhost")
				(glisp:set-gs-path 
				 #+mswindows (merge-pathnames "gpl/gs/gs8.63/bin/gswin32c.exe" glisp:*gdl-home*)))))

