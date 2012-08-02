(in-package :common-lisp-user)

(when (and (probe-file (merge-pathnames "smlib/" (make-pathname :name nil :type nil :defaults *load-truename*)))
	   (probe-file (merge-pathnames "smlib/load.lisp" *load-truename*))
	   (probe-file (merge-pathnames "smlib/smlib.fasl" *load-truename*))
	   (probe-file (merge-pathnames #+mswindows "smlib/smlib.dll" 
					#+linux "smlib/smlib.so" *load-truename*)))
  (load (merge-pathnames "smlib/load.lisp" *load-truename*)))


(when (probe-file (merge-pathnames "patches/" *load-truename*))
  (let ((patch-files 
	 (sort (remove-if-not 
		#'(lambda(path) (and (string-equal (pathname-type path) "lisp")
				     (search "patch-" (pathname-name path))))
		(directory (make-pathname :defaults (merge-pathnames "patches/" *load-truename*) :name nil :type nil)))
	       #'string-lessp :key #'file-namestring)))
    (dolist (file patch-files) 
      (without-redefinition-warnings
	(without-package-locks (load (compile-file file)))))))







	   
      