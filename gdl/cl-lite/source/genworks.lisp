(in-package :com.genworks.lisp)

(eval-when (compile load eval)
  (defpackage :com.genworks.lisp 
    (:use :common-lisp)
    (:export #:*fasl-extension*
             #:concatenate-fasls
             #:directory-list
             #:file-directory-p
             #:temporary-folder
             #:temporary-file)))

(defparameter *fasl-extension*
    #+allegro excl:*fasl-default-type*
    #+lispworks compiler:*fasl-extension-string*
    #-(or allegro lispworks) (error "Need fasl extension string for the currently running lisp.~%"))


(defun concatenate-fasls (files dest)
  #-(or allegro lispworks) (error "~&Please implement concatenate-fasls for the currently running lisp.~%")

  #+allegro
  ;;
  ;; Provided by Franz:
  ;;
  ;; copy the contents of all files to the file named dest.
  ;; append .fasl to the filenames
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
    (with-open-file (p dest :direction :output
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
      (dolist (file files)
        (with-open-file (in (if (pathname-type file)
                                file
                              (format nil "~a.~a" file *fasl-extension*))
                         :element-type '(unsigned-byte 8))
          (do ((count (read-sequence buffer in) (read-sequence buffer in)))
              ((<= count 0))
            (cl:write-sequence buffer p :end count))))))

  #+lispworks
    ;; created a defsystem on the fly and use concatenate-system.
  (let ((defsys (concatenate `string
                  (format nil "(defsystem gdl::my-system () :members (")
                  (let (result
                        (bin-files (mapcar #'(lambda(bin-file)
                                               (namestring bin-file))
                                           files)))
                    (dolist (file bin-files)
                      (setq result (concatenate 'string result (format nil "~s~%" file))))
                    result)
                  (format nil "))~%"))))
    (eval (read-from-string defsys))
    (lispworks:concatenate-system dest 'my-system)))



(defun directory-list (pathspec)
  (directory pathspec #+allegro :directories-are-files #+allegro nil))

;;
;; temporary-folder is potentially platform-specific so it is defined here. 
;;
(defun temporary-folder (&key (create? t))
  (let ((folder (merge-pathnames "tmp/" (user-homedir-pathname))))
    (when create? (ensure-directories-exist folder))))

(defun temporary-file (&key (extension nil) create?)
  (let ((file (merge-pathnames (make-pathname :name (gensym) 
                                              :type extension)
                               (temporary-folder))))
    (when create? 
      (with-open-file 
          (out file :direction :output :if-does-not-exist :create :if-exists :append))) 
    ;;
    ;; FLAG -- change this to return true pathname, fix code which uses it
    ;;
    (namestring file)))


(defun file-directory-p (file)
  "Returns non-nil if the path is a directory."
  #-(or allegro lispworks) (error "Need implementation for file-directory-p for currently running Lisp.")
  (#+allegro excl:file-directory-p #+lispworks lw:file-directory-p file))

