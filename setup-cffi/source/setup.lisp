;;(when (find-package :cffi) (in-package :cffi))
#-allegro
(in-package :cffi)

#-allegro
(defun %exe-path% ()
  #+sbcl (make-pathname :name nil :type nil :defaults sb-ext:*core-pathname*)
  #+ccl (make-pathname :name nil :type nil :defaults (first (ccl::command-line-arguments)))
  #+allegro (translate-logical-pathname "sys:")
  #+lispworks (make-pathname :name nil :type nil :defaults (first system:*line-arguments-list*)))

#-allegro
(when (find-package :cffi)
  (pushnew (merge-pathnames (format nil "dlls/~a/" (if (member :32-bit-host *features*) "32" "64"))
			    (%exe-path%))
	   (symbol-value (read-from-string "cffi:*foreign-library-directories*")))

  (define-foreign-library libeay32
    (:windows "libeay32.dll"))

  (use-foreign-library libeay32))
