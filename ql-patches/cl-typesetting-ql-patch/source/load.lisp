(in-package :cl-user)


(load (merge-pathnames 
       "quicklisp-local/cl-typesetting-20110219-svn/hyphenation-fp.lisp"
       com.genworks.lisp:*genworks-source-home*))

#+nil
(let ((fasl
       (compile-file
	(merge-pathnames
	 "quicklisp-local/cl-typesetting-20110219-svn/hyphenation-fp.lisp"
	 com.genworks.lisp:*genworks-source-home*))))
  (load fasl)
  (delete-file fasl))