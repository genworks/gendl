(in-package :cffi)

(defun %exe-path% ()
  #+sbcl (make-pathname :name nil :type nil :defaults sb-ext:*core-pathname*)
  #+ccl (make-pathname :name nil :type nil :defaults (first (ccl::command-line-arguments)))
  #+allegro (translate-logical-pathname "sys:")
  #+lispworks (make-pathname :name nil :type nil :defaults (first system:*line-arguments-list*)))

(pushnew (merge-pathnames (format nil "dlls/~a/" (if (member :32-bit-target *features*) "32" "64"))
			  (%exe-path%))
	 cffi:*foreign-library-directories*)


(define-foreign-library libeay32
  (:windows "libeay32.dll"))

(use-foreign-library libeay32)
