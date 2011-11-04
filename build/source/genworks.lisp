(in-package :com.genworks.lisp)

(eval-when (compile load eval)
  (defpackage :com.genworks.lisp 
    (:use :common-lisp)
    (:export #:make-gdl-app
             #:delete-directory-and-files
             )))

#-allegro(warn "~&Please implement generate-application for the currently running lisp.~%")
(defun make-gdl-app (&rest args)
  #+allegro 
  (let ((app-name (getf args :application-name))
        (target (getf args :destination-directory))
        (modules (append (list :asdf :compftype :aserve :phtml)
                         (getf args :modules)))
        (args (normalize-generate-application-args args)))
    (apply #'excl:generate-application app-name target modules args))
  #-allegro (declare (ignore args))
  #-allegro (error "~&generate-application is not implemented for the currently running lisp.~%"))


#+allegro 
(defun normalize-generate-application-args (args)
  (let ((class (getf args :application-class)))  
    (list :runtime (case class 
                     (:development :partners)
                     (otherwise nil))
        
          :include-compiler (case class 
                              (:development t)
                              (otherwise nil))
        
          :include-devel-env (case class 
                               (:development t)
                               (otherwise nil))

          :icon-file (merge-pathnames "gdl/gwl/static/gwl/images/favicon.ico"
                                      glisp:*genworks-source-home*)
          
          :demo (getf args :demo-days)
          :init-file-names (getf args :init-file-names)
          :pre-load-form (getf args :pre-load-form)
          :post-load-form (getf args :post-load-form)
          :restart-init-function (getf args :restart-init-function)
          :purify t
          :autoload-warning nil
          :runtime-bundle t
          :suppress-allegro-cl-banner t)))






#-(or allegro (and unix lispworks)) 
  (warn "~&Please implement delete-directory-and-files for the currently running lisp.~%")
(defun delete-directory-and-files (target &key force quiet if-does-not-exist)
  #+lispworks (declare (ignore force quiet if-does-not-exist))
  #+lispworks (system:run-shell-command (format nil "rm -rf ~a" target))
  #+allegro (excl.osi:delete-directory-and-files 
             target :force force :quiet quiet :if-does-not-exist if-does-not-exist))
