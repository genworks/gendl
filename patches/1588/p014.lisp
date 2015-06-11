(in-package :com.genworks.lisp)

;;
;; temporary-folder is potentially platform-specific so it is defined here. 
;;

(#+allegro 
 excl:without-package-locks 
 #-allegro progn
 (#+allegro 
  excl:without-redefinition-warnings 
  #-allegro progn
  (defparameter glisp::*temporary-folder* (user-homedir-pathname))
  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'glisp::*temporary-folder* :glisp))
  (defun temporary-folder (&key (create? t))
    (let ((folder (merge-pathnames "tmp/" glisp::*temporary-folder*)))
      (when create? (ensure-directories-exist folder))))))
