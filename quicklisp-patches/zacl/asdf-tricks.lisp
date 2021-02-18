;;;; asdf-tricks.lisp

(in-package #:zacl)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (member :asdf *features*)
    (let ((temp-file (merge-pathnames "tricks.lisp" uiop:*temporary-directory*)))
      (with-open-file (out temp-file :direction :output
			   :if-exists :supersede :if-does-not-exist :create)
	(write-string "

 (in-package :zacl) 

 (defclass zacl-reader:cl-file (asdf:cl-source-file)
   ((type :initform \"cl\")))


 (defmethod asdf:perform :around ((operation asdf:compile-op)
 				 (component zacl-reader:cl-file))
 	    (with-zacl-build-environment (call-next-method)))
" out))

      (let ((temp-fasl (compile-file temp-file)))
	(load temp-fasl)
	(mapc #'delete-file  (list temp-file temp-fasl))))))


