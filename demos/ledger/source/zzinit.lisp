(in-package :ledger)

(defvar *settings* 
  (list (list '*source-dir* *source-dir* 
	      #'(lambda()
		  (or (and *source-dir* (probe-file *source-dir*))

		      (and glisp:*gdl-program-home*
			   (probe-file (merge-pathnames "ledger-data/" glisp:*gdl-program-home*)))

		      (and glisp:*gdl-program-home*
			   (probe-file (merge-pathnames "data/" glisp:*gdl-program-home*)))
		      
		      (warn "~%Ledger data not found in source directory or parent of program directory.~%"))))))

(defun initialize ()
  (let (anything-changed?)
    (setq anything-changed? (glisp:set-settings *settings*))
    anything-changed?))


