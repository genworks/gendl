;; Copyright (c) 1987-2002 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, and to distribute modified
;; versions, provided that this complete copyright and permission notice is
;; maintained, intact, in all copies and supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

;; Support for changed definitions

(defvar fi:change-definitions-since-default
    'last-compile-or-eval
  "*The value of this variable is used as the default SINCE
argument to the changed-definition commands.  The value must be one of the
symbols file-first-read, buffer-save or last-compile-or-eval, which
correspond to SINCE arguments of 1, 2 and 3.  The meaning of each of the
values are:
  file-first-read - the changes since the file was first read by emacs are
     used,
  buffer-save - the changes since the last buffer save are used, and
  last-compile-or-eval - the changes since the last compile or eval changed
     definition command was issued.")

(defun fi::change-definition-convert-prefix-argument ()
  (list
   (if (null current-prefix-arg)
       fi:change-definitions-since-default
     (cond ((eq 1 current-prefix-arg) 'file-first-read)
	   ((eq 2 current-prefix-arg) 'buffer-save)
	   ((eq 3 current-prefix-arg) 'last-compile-or-eval)
	   ((eq 4 current-prefix-arg) 'comma-zero)
	   (t (error "prefix argument should be between 1 and 4"))))))

(defun fi:list-buffer-changed-definitions (since)
  "List the definitions in the current buffer which have been added,
deleted or changed.  See the documentation for the variable
fi:change-definitions-since-default for information on the behavior of
SINCE."
  (interactive (fi::change-definition-convert-prefix-argument))
  (fi::do-buffer-changed-definitions ':list since))

(defun fi:list-changed-definitions (since)
  "List the definitions in all buffers which have been added,
deleted or changed.  See the documentation for the variable
fi:change-definitions-since-default for information on the behavior of
SINCE."
  (interactive (fi::change-definition-convert-prefix-argument))
  (fi::do-buffer-changed-definitions ':list since t))

(defun fi:eval-buffer-changed-definitions (since)
  "Eval the definitions in the current buffer which have been added or
changed.  See the documentation for the variable
fi:change-definitions-since-default for information on the behavior of
SINCE."
  (interactive (fi::change-definition-convert-prefix-argument))
  (fi::do-buffer-changed-definitions ':eval since))

(defun fi:eval-changed-definitions (since)
  "Eval the definitions in all buffers which have been added or
changed.  See the documentation for the variable
fi:change-definitions-since-default for information on the behavior of
SINCE."
  (interactive (fi::change-definition-convert-prefix-argument))
  (fi::do-buffer-changed-definitions ':eval since t))

(defun fi:compile-buffer-changed-definitions (since)
  "Compile the definitions in the current buffer which have been added or
changed.  See the documentation for the variable
fi:change-definitions-since-default for information on the behavior of
SINCE."
  (interactive (fi::change-definition-convert-prefix-argument))
  (fi::do-buffer-changed-definitions ':compile since))

(defun fi:compile-changed-definitions (since)
  "Compile the definitions in all buffers which have been added or
changed.  See the documentation for the variable
fi:change-definitions-since-default for information on the behavior of
SINCE."
  (interactive (fi::change-definition-convert-prefix-argument))
  (fi::do-buffer-changed-definitions ':compile since t))

(defun fi:copy-buffer-changed-definitions (since)
  "Copy into the kill ring the definitions in the current buffer which have
been added or changed.  See the documentation for the variable
fi:change-definitions-since-default for information on the behavior of
SINCE."
  (interactive (fi::change-definition-convert-prefix-argument))
  (fi::do-buffer-changed-definitions ':copy since))

(defun fi:copy-changed-definitions (since)
  "Copy into the kill ring the definitions in all buffers which have been
added or changed.  See the documentation for the variable
fi:change-definitions-since-default for information on the behavior of
SINCE."
  (interactive (fi::change-definition-convert-prefix-argument))
  (fi::do-buffer-changed-definitions ':copy since t))

(defun fi:compare-source-files (new-file old-file)
  "Compare two files, NEW-FILE and OLD-FILE, listing the definitions in the
in NEW-FILE which have been added, deleted or changed with respect to
OLD-FILE."
  (interactive "fNew file: \nfOld file: ")
  (find-file new-file)
  (let ((actual-file (buffer-file-name (find-buffer-visiting new-file)))
	(xpackage (fi::package)))
    (fi::make-request (scm::list-changed-definitions
		       :transaction-directory fi:emacs-to-lisp-transaction-directory
		       :operation ':list
		       :actual-file actual-file
		       :old-file old-file
		       :new-file new-file)
      ((xpackage) (changes)
       (if changes
	   (fi::show-changes changes nil xpackage)
	 (message "There are no changes.")))
      (() (error)
       (error "Cannnot list changed definitions: %s" error)))))

;;; The guts of the problem

(defun fi::do-buffer-changed-definitions (operation since
					  &optional all-buffers)
  (message "Computing changes...")
  (let ((copy-file-name (and (eq operation ':copy)
			     (format "%s/%s.cl"
				     fi:emacs-to-lisp-transaction-directory
				     (make-temp-name "EtoL")))))
    (if all-buffers
	(let ((args nil))
	  (save-excursion
	    (dolist (buffer (buffer-list))
	      (set-buffer buffer)
	      (if (fi::check-buffer-for-changes-p since)
		  (push (fi::compute-file-changed-values-for-current-buffer
			 since)
			args))))
	  (if args
	      (apply (function fi::do-buffer-changed-definitions-1)
		     operation since
		     copy-file-name
		     (fi::transpose-list args))
	    (message "There are no changes.")))
      (if (fi::check-buffer-for-changes-p since)
	  (apply (function fi::do-buffer-changed-definitions-1)
		 operation since
		 copy-file-name
		 (fi::compute-file-changed-values-for-current-buffer since))
	(message "There are no changes.")))))

(defvar fi::unlock-file-suffix ",0")

(defun fi::check-buffer-for-changes-p (since)
  "Decide whether this buffer is worth checking for changes."
  (and (eq major-mode 'fi:common-lisp-mode)
       (buffer-file-name)
       (ecase since
	 (comma-zero
	  (file-exists-p (concat (buffer-file-name) fi::unlock-file-suffix)))
	 (file-first-read (or (buffer-modified-p) buffer-backed-up))
	 (buffer-save (buffer-modified-p))
	 (last-compile-or-eval
	  ;; Its like this buffer needs an every-modified-flag
	  t))))

(defun fi::compute-file-changed-values-for-current-buffer (since)
  (let ((actual-file (buffer-file-name))
	(old-file 
	 (case since
	   (comma-zero (concat (buffer-file-name) fi::unlock-file-suffix))
	   (t (if (and (not (eq since 'buffer-save))
		       buffer-backed-up)
		  (car (fi::find-most-recent-backup-file-name
			(buffer-file-name)))
		(buffer-file-name))))))
    (list actual-file old-file actual-file)))

(defun fi::do-buffer-changed-definitions-1 (operation since
					    copy-file-name actual-file
					    old-file new-file)
  (fi::make-request (scm::list-changed-definitions
		     :operation operation
		     :copy-file-name copy-file-name
		     :actual-file actual-file
		     :old-file old-file
		     :new-file new-file
		     :since since)
   ((operation copy-file-name) (changes)
    (if changes
	(progn
	  (if (eq operation ':copy)
	      (fi::insert-file-contents-into-kill-ring copy-file-name))
	  (fi::show-changes changes))
      (message "There are no changes.")))
   ((operation) (error)
    (error 		 
     (ecase operation
       (:copy "copy changed definitions: %s")
       (:list "Cannnot list changed definitions: %s")
       (:eval "Cannnot evaluate changed definitions: %s")
       (:compile "Cannnot compile changed definitions: %s"))
     error))))

(defun fi::show-changes (changes &optional buffer-name xpackage)
  (lep:display-some-definitions (or xpackage (fi::package))
				changes
				(list 'lep::find-buffer-definition)
				(or buffer-name "*changes*")))
