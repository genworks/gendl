;; Copyright (c) 1987-2002 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, and to distribute modified
;; versions, provided that this complete copyright and permission notice is
;; maintained, intact, in all copies and supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

(defun fi:lisp-arglist (string)
  "Dynamically determine, in the Common Lisp environment, the arglist for
STRING.  fi:package is used to determine from which Common Lisp package the
operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
  (interactive (fi::get-default-symbol "Arglist for" t t))
  (fi::make-request (lep::arglist-session :fspec string)
    ;; Normal continuation
    (() (what arglist)
     (fi:show-some-text nil "%s's arglist: %s" what arglist))
    ;; Error continuation
    ((string) (error)
     (fi::show-error-text "Cannot get the arglist of %s: %s" string error))))

;; The implementation of fi::arglist-lisp-space-1 is from Bill Clementson
;; (Bill_Clementson@jdedwards.com), who says it is an adaptation of ILISP
;; code.  The idea for fi:auto-arglist-pop-up-style came from Steve Haflich
;; (smh@franz.com).

(defun fi::arglist-lisp-space-1 ()
  (let* ((old-point (point))
	 (last-char
	  (progn (ignore-errors (backward-char))
		 (unless (eql (point) old-point)
		   (buffer-substring-no-properties old-point (point)))))
	 (string
	  (buffer-substring-no-properties old-point
					  (progn
					    (goto-char old-point)
					    (ignore-errors
					     (backward-sexp))
					    (point))))
	 (prefix-char 
	  (let ((save (ignore-errors
		       (goto-char old-point)
		       (backward-sexp)
		       (backward-char)
		       (point))))
	    (when save
	      (buffer-substring-no-properties save (1+ save)))))
	 (double-quote-pos (and string (string-match "\"" string)))
	 (paren-pos (and string
			 (string-match "(" string)))
	 (symbol-with-package
	  (unless (eql paren-pos 0)
	    (if (and double-quote-pos (eql double-quote-pos 0)
		     string (ignore-errors (elt string 2)))
		(substring string 1 -1)
	      string)))
	 (symbol symbol-with-package))
    (flet ((no-arglist-output-p ()
	     (or (and last-char 
		      (or
		       ;; don't do silly things after comment character
		       (equal last-char ";")
		       ;; do something only if directly after a sexp.
		       (equal last-char " ")))
		 ;; could be something like #+foo, #-foo, or #:foo, any of
		 ;; which is likely to lose.
		 (and string (string-match "^#" string))
		 double-quote-pos ;; there is no output  for strings only.
		 (not (and symbol (stringp symbol) (> (length symbol) 0)))
		 (string-match "^\. " symbol)
		 (string-match "^\\\\" symbol))))
      (goto-char old-point)
      (unless (no-arglist-output-p)
	;; only output for functions within brackets; too much lisp-traffic!
	(when (equal prefix-char "(")
	  (setq string (fi::normalize-symbol-package string))
	  (fi::make-request (lep::arglist-session :fspec string)
	    ;; Normal continuation
	    (() (what arglist)
	     (let ((fi:pop-up-temp-window-behavior
		    fi:auto-arglist-pop-up-style))
	       (fi:show-some-text nil "%s's arglist: %s" what arglist)))
	    ;; Error continuation
	    ((string) (error)
	     (fi::show-error-text "")))))))
  (self-insert-command (prefix-numeric-value current-prefix-arg)))

(defun fi:lisp-apropos (string &optional regexp)
  "In the Common Lisp environment evaluate lisp:apropos on STRING.
With prefix arg REGEXP, STRING is an ACL regular expression for which
matches are sought.  fi:package is used to determine from which Common Lisp
package the operation is done.  In a subprocess buffer, the package is
tracked automatically.  In source buffer, the package is parsed at file
visit time.

ACL regular expressions differ from those in Emacs.  See the ACL
documentation for more information."
  (interactive
   (list (car (fi::get-default-symbol
	       (if current-prefix-arg "Apropos (regexp)" "Apropos")
	       nil))
	 (if current-prefix-arg t nil)))
  (fi::make-request
   (lep::apropos-session :string string :regexp regexp)
   ;; Normal continuation
   (() (text)
    (fi:show-some-text nil text))
   ;; Error continuation
   ((string) (error)
    (fi::show-error-text "error during apropos of %s: %s" string error))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Metadot implementation

(defvar fi:maintain-definition-stack t
  "*If non-nil, then maintain a stack of definitions found by various
source code finding functions (fi:lisp-find-definition,
fi:edit-generic-function-methods, etc).  When you find a definition for
a name and there are multiple definitions, fi:lisp-find-next-definition is
used to step through the list of definitions.  If, in the middle of
stepping through some definitions, another find definition command is
executed, then the previous definitions are pushed onto a stack and can one
can resume finding these definitions after the current ones are
exhausted.")

(defvar lep::meta-dot-what nil)

(defun lep::meta-dot-what ()
  (if fi:maintain-definition-stack
      (car lep::meta-dot-what)
    lep::meta-dot-what))

(defvar lep::meta-dot-string nil)

(defun lep::meta-dot-string ()
  (if fi:maintain-definition-stack
      (car lep::meta-dot-string)
    lep::meta-dot-string))

(defvar lep::meta-dot-from-fspec nil)

(defun lep::meta-dot-from-fspec ()
  (if fi:maintain-definition-stack
      (car lep::meta-dot-from-fspec)
    lep::meta-dot-from-fspec))

(defvar lep::meta-dot-session nil)

(defun lep::meta-dot-session ()
  (if fi:maintain-definition-stack
      (car lep::meta-dot-session)
    lep::meta-dot-session))

(defvar lep::show-def-marker-ring (make-ring 16))

(defun fi:pop-definition-mark ()
  "Pop back to where the find definition was last invoked."
  (interactive)
  (if (ring-empty-p lep::show-def-marker-ring)
      (error "No previous locations for show-found-definition invocation"))
  (let ((marker (ring-remove lep::show-def-marker-ring 0)))
    (fi::switch-to-buffer (or (marker-buffer marker)
			      (error "The marked buffer has been deleted")))
    (goto-char (marker-position marker))
    (set-marker marker nil nil)))

(defun fi:lisp-find-definition (tag &optional next)
  "Find TAG using information in the Common Lisp environment, in the current
window.  With prefix arg NEXT, find the next occurance of the last tag.
fi:package is used to determine from which Common Lisp package the
operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
  (interactive
   (if current-prefix-arg
       '(nil t)
     (list (car (fi::get-default-symbol "Lisp locate source" t t))
	   nil)))
  (if next
      (fi:lisp-find-next-definition)
    (fi::lisp-find-definition-common tag nil)))


(defun fi:lisp-find-definition-other-window (tag &optional next)
  "Find TAG in the other window using information in the Common Lisp
environment, in the current window.  With prefix arg NEXT, find the next
occurance of the last tag. fi:package is used to determine from which
Common Lisp package the operation is done. In a subprocess buffer, the
package is tracked automatically.  In source buffer, the package is parsed
at file visit time."
  (interactive
   (if current-prefix-arg
       '(nil t)
     (list (car (fi::get-default-symbol "Lisp locate source other window"
					t t))
	   nil)))
  (if next
      (fi:lisp-find-next-definition)
    (fi::lisp-find-definition-common tag t)))

(defun fi::lisp-find-definition-common (something other-window-p
					&optional what from-fspec)
  (when (not (fi::lep-open-connection-p))
    (error "connection to ACL is down--can't find tag"))
  (message "Finding %s for %s..." (or what "definition") something)
  (fi::push-metadot-session
   (or what "definition")
   something
   from-fspec
   (fi::make-complex-request
    (scm::metadot-session
     :package (fi::string-to-keyword (fi::package))
     :type t				; used to be (or type t), but
					; `type' is not bound in this
					; context
     :fspec something)
    ((something other-window-p what from-fspec)
     (pathname point n-more)
     (fi::show-found-definition (if (symbolp something)
				    (symbol-name something)
				  something)
				pathname
				point n-more other-window-p
				(eq 0 n-more))
     (if (= 0 n-more) (fi::pop-metadot-session)))
    (() (error)
	(when (fi::pop-metadot-session)
	  (fi::show-error-text "%s" error))))))

(defun fi::ensure-translated-pathname (pathname)
  (if (position ?: pathname)
      (or (ignore-errors (fi::translate-putative-logical-pathname pathname))
	  pathname)
    pathname))

(defun fi::translate-putative-logical-pathname (pathname)
  (fi:eval-in-lisp
   ;; It's important to use %S instead of %s, so that \'s are properly
   ;; handled.  See discussion in bug8953.
   "cl:(ignore-errors (namestring (translate-logical-pathname \"%S\")))"
   pathname))

(defun fi:lisp-find-next-definition ()
  "Continue last tags search, started by fi:lisp-find-definition.
fi:package is used to determine from which Common Lisp package the
operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
  (interactive)
  (message "Finding next %s..." (lep::meta-dot-what))
  (if (not (lep::meta-dot-session)) (error "No more definitions"))
  (fi::make-request-in-existing-session
   (lep::meta-dot-session)
   (:next)
   (() (pathname point n-more)
       (fi::show-found-definition (lep::meta-dot-string) pathname point n-more
				  nil (eq 0 n-more)))
   (() (error)
       (when (fi::pop-metadot-session)
	 (fi::show-error-text "%s" error)))))

(defvar session) ;; bad name, but changing it is too complicated right now

(defun scm::make-and-initialize-metadot-session
    (something &optional what from-fspec)
  (fi::push-metadot-session (or what "definition") something from-fspec
			    session)
  (fi::modify-session-continuation
   session
   (list (function (lambda (pathname point n-more)
		     (fi::show-found-definition (lep::meta-dot-string)
						pathname point n-more
						(eq 0 n-more)))))
   (list (function (lambda (error something)
		     (when (fi::pop-metadot-session)
		       (fi::show-error-text "%s: %s" something error))))
	 (lep::meta-dot-string))))
(fset 'excl.scm::make-and-initialize-metadot-session
      (symbol-function 'scm::make-and-initialize-metadot-session))

(defun fi::pop-metadot-session ()
  ;; return `t' if we are `done' (nothing left to do)
  (cond (fi:maintain-definition-stack
	 (let ((old-what (car lep::meta-dot-what))
	       (old-string (car lep::meta-dot-string)))
	   (setq lep::meta-dot-what (cdr lep::meta-dot-what))
	   (setq lep::meta-dot-string (cdr lep::meta-dot-string))
	   (setq lep::meta-dot-from-fspec (cdr lep::meta-dot-from-fspec))
	   (lep::kill-session (car lep::meta-dot-session))
	   (setq lep::meta-dot-session (cdr lep::meta-dot-session))
	   (when lep::meta-dot-session
	     (message "done with %ss of %s; more %ss of %s..."
		      old-what old-string
		      (car lep::meta-dot-what)
		      (car lep::meta-dot-string)))
	   (not (car lep::meta-dot-session))))
	(t
	 (if lep::meta-dot-session (lep::kill-session lep::meta-dot-session))
	 (setq lep::meta-dot-what nil)
	 (setq lep::meta-dot-string nil)
	 (setq lep::meta-dot-from-fspec nil)
	 (setq lep::meta-dot-session nil)
	 t)))

(defun fi::push-metadot-session (what string from-fspec session)
  (cond (fi:maintain-definition-stack
	 (setq lep::meta-dot-what (cons what lep::meta-dot-what))
	 (setq lep::meta-dot-string (cons string lep::meta-dot-string))
	 (setq lep::meta-dot-from-fspec
	   (cons from-fspec lep::meta-dot-from-fspec))
	 (setq lep::meta-dot-session (cons session lep::meta-dot-session)))
	(t
	 (fi::pop-metadot-session)
	 (setq lep::meta-dot-what what)
	 (setq lep::meta-dot-string string)
	 (setq lep::meta-dot-from-fspec from-fspec)
	 (setq lep::meta-dot-session session))))

(defun fi::reset-metadot-session ()
  (setq lep::meta-dot-what nil)
  (setq lep::meta-dot-string nil)
  (setq lep::meta-dot-from-fspec nil)
  (setq lep::meta-dot-session nil))

(defun fi::show-found-definition (thing pathname point n-more
				  &optional other-window-p pop-stack)
  (if pathname
      (if (equal pathname "top-level")
	  (message
	   "%s was defined somewhere at the top-level, %d more definitions"
	   thing n-more)
	(let ((mess "")
	      (xb nil)
	      (pathname (fi::ensure-translated-pathname pathname)))
	  (when fi:filename-frobber-hook
	    (setq pathname (funcall fi:filename-frobber-hook pathname)))
	  (ring-insert lep::show-def-marker-ring (point-marker))
	  (setq xb (get-file-buffer pathname))
	  (if other-window-p
	      (find-file-other-window pathname)
	    (find-file pathname))
	  (if xb (set-mark (point)))
	  (if (null point)
	      (progn
		(setq mess
		  (fi::double-char-in-string
		   ?%
		   (format "The definition of %s is somewhere in this file! "
			   thing)))
		(goto-char (point-min)))
	    (progn
	      (goto-char (1+ point))
	      (if (not xb) (set-mark (point)))))
	  (cond ((eq n-more 0)
		 (if (lep::meta-dot-from-fspec)
		     (message (concat mess "%ss of %s")
			      (lep::meta-dot-what) (lep::meta-dot-from-fspec))
		   (message (concat mess "No more %ss of %s")
			    (lep::meta-dot-what) thing)))
		(n-more
 		 (message (concat mess "%d more %ss of %s")
			  n-more
			  (lep::meta-dot-what)
			  (or (lep::meta-dot-from-fspec) thing))))
	  (when pop-stack (fi::pop-metadot-session))))
    (message "cannot find file for %s" thing)))

(defun scm::return-buffer-status (pathname write-if-modified)
  "This returns information about the status of the buffer: whether it
exists, if it is modified, last tick (when implemented), and optionally
return the pathname of temp file."
  (let ((buffer (get-file-buffer pathname)))
    (if buffer
	(list ':exists
	      (buffer-modified-p buffer)
	      (and write-if-modified
		   (or (not (integerp write-if-modified))
		       (not (fboundp 'buffer-modified-tick))
		       (not (equal (buffer-modified-tick) write-if-modified)))
		   (buffer-modified-p buffer)
		   (save-excursion
		     (set-buffer buffer)
		     (let* ((file (concat
				   fi:emacs-to-lisp-transaction-directory
					  (make-temp-name "/foo")))
			    (buffer (get-file-buffer file)))
		       (when buffer (kill-buffer buffer))
		       (write-region (point-min) (point-max) file nil
				     'no-message)
		       file)))
	      (lep::buffer-modified-tick))
      (list ':does-not-exist))))
(fset 'excl.scm::return-buffer-status
      (symbol-function 'scm::return-buffer-status))

(defun scm::signal-transaction-file-error (pathname)
  (fi:note "
Can't find transaction file %s in %s, which is the directory that
Emacs and Lisp use to communicate.  Most likely Emacs and Lisp are running
on different machines.  Please check the value of the Emacs variable
fi:emacs-to-lisp-transaction-directory.
The value of this Emacs variable should be a string which names a directory
which is accessible from the machines on which Emacs and Lisp are running.
Put something like this form in your ~/.emacs file:

  (setq fi:emacs-to-lisp-transaction-directory (expand-file-name \"~/tmp\"))

before the load of fi-site-init.  Don't forget to make sure ~/tmp exists,
since the Emacs-Lisp interface will not create it."
	    pathname fi:emacs-to-lisp-transaction-directory)
  nil)
(fset 'excl.scm::signal-transaction-file-error
      (symbol-function 'scm::signal-transaction-file-error))

(defun lep::buffer-modified-tick ()
  "Get the buffer tick if it is supported"
  (and (fboundp 'buffer-modified-tick) (buffer-modified-tick)))

;; XEmacs 21 pre-release versions don't have this, so protect ourselves
(condition-case ()
    (require 'sendmail) ;; for mail-to
  (error nil)) 

(defun fi:bug-report ()
  "Create a mail buffer which contains information about the Common Lisp
environment in which the bug occurs.  A :zoom and other related information
is obtained from the \"Initial Lisp Listener\".  See M-x mail for more
information on how to send the mail."
  (interactive)
  (fi::make-request
      (lep::bug-report-session
       :process-name
       (fi::read-lisp-process-name "Process for stack :zoom: "))
    ;; Normal continuation
    (() (error-message stack lisp-info)
      (mail)
      (mail-to)
      (insert "support@franz.com")
      ;;(mail-subject)
      ;;(insert "Bug-report")
      (goto-char (point-max))
      (save-excursion
	(insert "\n")
	(when (and error-message (not (string= "" error-message)))
	  (insert "------------------------------\n\n")
	  (insert error-message))
        (insert "<<Please enter any comments or explanations here>>\n\n")
	(insert "\n------------------------------\n\n")
	(insert stack)
	(insert "\n------------------------------\n\n")
	(insert lisp-info)
	(insert "\n------------------------------\n\n")
	(insert (format "Emacs version: %s\n"
			(emacs-version)))
	(insert (format "Emacs-Lisp interface version: %s\n\n"
			fi:emacs-lisp-interface-version))
	(insert (format "load-path: %s\n\n" load-path))
	(let* ((file (fi::find-path load-path "fi-site-init.el"))
	       (dir (when file
		      (file-name-directory file))))
	  (if dir
	      (progn
		(insert (format "Contents of %s directory:\n" dir))
		(call-process "ls" nil t nil "-la"
			      (expand-file-name dir))
		(insert "\n"))
	    (insert (format "Could not find fi-site-init.el\n")))
	  (insert "\n")))
      (message "Please enter a descriptive Subject: line")
      (mail-subject))
    ;; Error continuation
    (() (error)
      (fi::show-error-text "Cannot do a backtrace because: %s" error))))

;;; Macroexpansion and walking

(defun fi:lisp-macroexpand ()
  "Print the macroexpansion of the form at the point.
fi:package is used to determine from which Common Lisp package the
operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
  (interactive)
  (message "Macroexpanding...")
  (fi::lisp-macroexpand-common 'lisp:macroexpand-1 "macroexpand"))

(defun fi:lisp-macroexpand-recursively (arg)
  "Print the full, recursive macroexpansion the form at the point.
With prefix arg, recursively macroexpand the code as the compiler
would.  (The compiler simulation is approximate only and does not
preserve the precise semantics of the form.)  fi:package is used to
determine from which Common Lisp package the operation is done.  In a
subprocess buffer, the package is tracked automatically.  In source
buffer, the package is parsed at file visit time."
  (interactive "P")
  (message "Recursively macroexpanding...")
  (fi::lisp-macroexpand-common
   (if arg
       'excl::compiler-walk
     (if (and fi::lisp-version
	      (consp fi::lisp-version)
	      (>= (car fi::lisp-version) 6))
	 'excl::walk-form
       'clos::walk-form))
   "walk"))

(defun fi::lisp-macroexpand-common (expander type)
  (fi::make-request
      (lep::macroexpand-session
       :expander expander :package (fi::string-to-keyword (fi::package))
       :form (let ((start (condition-case ()
			      (fi::find-other-end-of-list)
			    (error nil))))
	       (fi::defontify-string
		   (if start
		       (buffer-substring start (point))
		     (read-string (format "form to %s: " type))))))
    (() (expansion)
     (fi:show-some-text (fi::package) expansion))
    (() (error)
     (fi::show-error-text "Cannot macroexpand: %s" error))))


;;; Symbol completion

(defun fi:lisp-complete-symbol ()
  "Perform completion on the Common Lisp symbol preceding the point.  That
symbol is compared to symbols that exist in the Common Lisp environment.
If the symbol starts just after an open-parenthesis, then only symbols (in
the Common Lisp) with function definitions are considered.  Otherwise all
symbols are considered.  fi:package is used to determine from which Common
Lisp package the operation is done.  In a subprocess buffer, the package is
tracked automatically.  In source buffer, the package is parsed at file
visit time.

Abbreviations are also expanded.  For example, in the initial `user'
package, which inherits symbols from the `common-lisp' package, ``m-p-d-''
will expand to ``most-positive-double-float''.  The hyphen (-) is a
separator that causes the substring before the hyphen to be matched at the
beginning of words in target symbols."
  (interactive)
  (let* ((end (point))
	 xpackage real-beg
	 (beg (save-excursion
		(backward-sexp 1)
		(while (= (char-syntax (following-char)) ?\')
		  (forward-char 1))
		(setq real-beg (point))
		(let ((opoint (point)))
		  (if (re-search-forward ":?:" end t)
		      (setq xpackage
			(concat
			 ":"
			 (fi::defontify-string
			     (buffer-substring opoint (match-beginning 0)))))))
		(point)))
	 (pattern (fi::defontify-string (buffer-substring beg end)))
	 (functions-only (if (eq (char-after (1- real-beg)) ?\() t nil))
	 (downcase (and (eq ':upper fi::lisp-case-mode)
			(not (fi::all-upper-case-p pattern))))
	 (xxalist (fi::lisp-complete-1 pattern xpackage functions-only))
	 temp
	 (package-override nil)
	 (xalist
	  (if (and xpackage (cdr xxalist))
	      (fi::package-frob-completion-alist xxalist)
	    (if (and (not xpackage)
		     ;; current package of buffer is not the same as the
		     ;; single completion match
		     (null (cdr xxalist)) ;; only one
		     (setq temp (fi::extract-package-from-symbol
				 (cdr (car xxalist))))
		     (not
		      (string= (fi::full-package-name
				(or (fi::package) "cl-user"))
			       (fi::full-package-name temp))))
		(progn
		  (setq package-override t)
		  xxalist)
	      xxalist)))
	 (alist (if downcase
		    (mapcar 'fi::downcase-alist-elt xalist)
		  xalist))
	 (completion
	  (when alist
	    (let* ((xfull-package-name
		    (if (string= ":" xpackage)
			"keyword"
		      (when xpackage
			(fi::full-package-name xpackage))))
		   (full-package-name
		    (when xfull-package-name
		      (if downcase
			  (downcase xfull-package-name)
			xfull-package-name))))
	      (when (or full-package-name package-override)
		(setq pattern
		  (format "%s::%s" full-package-name pattern)))
	      (try-completion pattern alist)))))

    (cond ((eq completion t)
	   (message "Completion is unique."))
	  ((and (null completion) (null alist))
	   (message "Can't find completion for \"%s\"" pattern)
	   (ding))

;;;; the next three clauses are for abbrev expansion:

	  ((and (null completion) alist (null (cdr alist)))
	   (delete-region real-beg end)
	   (insert (cdr (car alist))))

	  ((and (null completion) alist)
	   ;; pattern is something like l-a-comp.  The next hack is to turn
	   ;; this into something like list-all-comp...
	   (delete-region real-beg end)
	   (insert (fi::abbrev-to-symbol pattern alist))

	   (message "Making completion list...")
	   (with-output-to-temp-buffer "*Completions*"
	     (display-completion-list (mapcar 'cdr alist)))
	   (message "Making completion list...done"))

	  ((and (cdr alist)
		(or
		 ;; there is a match, but there are other possible
		 ;; matches
		 (string= pattern completion)
		 ;; more than one choice, so insert what completion we have
		 ;; and give the choices to the user
		 (not (assoc pattern alist))))
	   (if xpackage
	       (delete-region real-beg end)
	     (delete-region beg end))
	   (insert completion)
	   (message "Making completion list...")
	   (with-output-to-temp-buffer "*Completions*"
	     (display-completion-list
	      (mapcar 'cdr alist))))
;;;;

	  ((null (string= pattern completion))
	   (let ((new (cdr (assoc completion alist))))
	     (if new
		 (progn
		   (delete-region real-beg end)
		   (insert new))
	       (delete-region beg end)
	       (insert completion)))
	   (if (not (cdr alist))
	       (message "Completion is unique.")))
	  (t
	   (message "Making completion list...")
	   (with-output-to-temp-buffer "*Completions*"
	     (display-completion-list
	      (mapcar 'cdr alist)))
	   (message "Making completion list...done")))))

(defun fi::extract-package-from-symbol (s)
  (when (and s (string-match fi::*package-prefix* s))
    (substring s (match-beginning 1) (match-end 1))))

(defun fi::full-package-name (s)
  (fi:eval-in-lisp
   "cl:(let ((p (find-package :%s)))
        (or (and excl:*print-nickname* (car (package-nicknames p)))
	    (package-name p)))"
   s))

(defvar fi::inside-lisp-complete-1 nil)

(defun fi::lisp-complete-1 (pattern xpackage functions-only
			    &optional ignore-keywords)
  (unless fi::inside-lisp-complete-1	;return nil on recursion
    (let ((fi::inside-lisp-complete-1 t))
      (condition-case nil
	  (let ((completions
		 (car (lep::eval-session-in-lisp
		       'lep::list-all-completions-session
		       ':pattern (fi::frob-case-to-lisp pattern)
		       ':buffer-package (fi::string-to-keyword (fi::package))
		       ':package (progn
				   (if (equal ":" xpackage)
				       (setq xpackage "keyword"))
				   (intern (fi::frob-case-to-lisp xpackage)))
		       ':functions-only-p (intern
					   (fi::frob-case-to-lisp
					    functions-only))
		       ':ignore-keywords (intern
					  (fi::frob-case-to-lisp
					   ignore-keywords))))))
	    (fi::lisp-complete-2 completions xpackage))
	(quit
	 (fi:eval-in-lisp
	  "(when (fboundp 'lep::kill-list-all-completions-session)
     (lep::kill-list-all-completions-session))"))))))

(defun fi::lisp-complete-2 (completions &optional dont-strip-package)
  (if (consp completions)
      (apply 'list
	     (mapcar
	      (function
	       (lambda (x)
		 (let* ((whole-name (if (symbolp x) (symbol-name x) x))
			(name (if dont-strip-package
				  whole-name
				(progn
				  (string-match "^\\(.*::?\\)?\\(.*\\)$"
						whole-name)
				  (substring whole-name
					     (match-beginning 2)
					     (match-end 2))))))
		   (cons name whole-name))))
	      completions))))

(defun lep::find-file (filename)
  (list (find-file (fi::ensure-translated-pathname filename))))

(defun lep::display-string-in-buffer (string buffer)
  "Display a string in buffer"
  (fi:lisp-push-window-configuration)
  (switch-to-buffer (get-buffer-create buffer))
  (erase-buffer)
  (insert string)
  (goto-char (point-min))
  (fi::ensure-minibuffer-visible))

(defun lep::write-string-to-hidden-buffer (string buffer)
  "Like lep::display-string-in-buffer, but don't display the buffer."
  (save-excursion
    (set-buffer (get-buffer-create buffer))
    (erase-buffer)
    (insert string)
    (goto-char (point-min))))

(defun lep::prompt-for-values (what prompt options)
  (fi::ensure-minibuffer-visible)
  (list (case what
	  (:symbol
	   (let* ((string (read-string
			   prompt (fi::getf-property options ':initial-input)))
		  (colonp (string-match ":?:" string nil))
		  (xpackage (or (fi::getf-property options ':package)
				(fi::package))))
	     ;; symbol-point
	     (if colonp
		 string
	       (if xpackage
		   (concat (fi::package) "::" string)
		 string))))
	  (:file-name (read-file-name
		       prompt
		       (fi::getf-property options ':directory)
		       (fi::getf-property options ':default)
		       (fi::getf-property options ':mustmatch)))
	  (t (read-string
		    prompt (fi::getf-property options ':initial-input))))))

(defun lep::completing-read (prompt require-match initial-input)
  (fi::ensure-minibuffer-visible)
  (list (completing-read
	 prompt
	 'lep::completing-read-complete
	 nil
	 require-match
	 initial-input)))

(defun lep::completing-read-complete (pattern predicate what)
  (let* ((inhibit-quit nil)
	 (matchp (string-match ":?:" pattern))
	 (xpackage (and matchp (substring pattern 0 matchp)))
	 (string (if matchp
		     (substring pattern (match-end 0))
		   pattern))
	 (package-prefix (and xpackage
			      (substring pattern matchp (match-end 0))))
	 (alist
	  (fi::lisp-complete-2
	   (car
	    (lep::make-request-in-session-and-wait
	     session
	     ':complete
	     (fi::frob-case-to-lisp string)
	     (and xpackage
		  (intern
		  (fi::frob-case-to-lisp
		   (concat ":"
			   (if (equal "" xpackage)
			       "keyword"
			     xpackage)))))))))
	 (completion (and alist (try-completion string alist))))
    (when (and xpackage (stringp completion))
      (setq completion (concat xpackage package-prefix completion)))
    (ecase what
      ((nil) completion)
      ((t) (mapcar (function cdr) alist))
      (lambda (eq completion t)))))

(defun lep::buffer-region (buffer start end)
  (set-buffer buffer)
  (list (fi::defontify-string
	    (buffer-substring (or start (point-min)) (or end (point-max))))))

(defun fi:kill-definition (do-kill)
  "Insert a form to kill, or undefine, the definition that starts at the
point.  The undefining form is inserted after the form to undefine.
With prefix arg DO-KILL, then actually undefine the form in the Common Lisp
environment instead of inserted the undefining form.  fi:package is used to
determine from which Common Lisp package the operation is done.  In a
subprocess buffer, the package is tracked automatically.  In source buffer,
the package is parsed at file visit time."
  (interactive "P")
  (message "Killing definition...")
  (fi::make-request
   (lep::undefine-reply :buffer (buffer-name)
			:start-point (point)
			:end-point (save-excursion
				     (forward-sexp)
				     (point))
			:doit do-kill)
   ((do-kill) (form)
    (unless do-kill
      (end-of-defun)
      (save-excursion
	(insert form)
	(insert "\n")))
    (message "Killing definition...done."))
   (() (error)
    (fi::show-error-text "Cannot kill current definition: %s" error))))


(defun fi:toggle-trace-definition (string)
  "Dynamically toggle, in the Common Lisp environment, tracing for STRING.
If tracing is turned on, then it will be turned off for STRING.  If it is
turned off, then it will be turned on for STRING.  With a prefix arg, cause
the debugger to be invoked, via a call to BREAK, when the function is called.
fi:package is used to determine from which Common Lisp package the
operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
  (interactive (fi::get-default-symbol "(un)trace" t t))
  (fi::make-request
   (lep::toggle-trace :fspec string :break current-prefix-arg)
   ;; Normal continuation
   (() (what tracep)
    (message (if tracep "%s is now traced" "%s is now untraced")
	     what))
   ;; Error continuation
   ((string) (error)
	     (fi::show-error-text "Cannot (un)trace %s: %s" string error))))

(defun fi:trace-definition (break)
  "Traces the definition that begins at point. This is especially useful
for tracing methods."
  (interactive "P")
  (message "Tracing definition...")
  (fi::make-request
   (lep::trace-definition-reply :buffer (buffer-name)
				:start-point (point)
				:break break
				:end-point (save-excursion
					     (forward-sexp)
					     (point)))
   (() (form)
    (message "Tracing definition...done."))
   (() (error)
    (fi::show-error-text "Cannot trace current definition: %s" error))))

(defun fi::substr-sexp ()
  ;; Moves point!
  (let ((start (point)))
    (forward-sexp 1)
    (buffer-substring start (point))))

(defun fi:trace-definer (current-prefix-arg)
  "Dynamically toggle, in the Common Lisp environment, tracing for the
function defined by the top-level form around the cursor position.  The
form can be a defun, defgeneric, defmethod, define-compiler-macro, or
deftype.  The defmethod case is most useful, as the function spec for
the particular method is extracted from the qualifiers and specializers.
If tracing is already turned on, then it will be turned off.  With a
prefix arg, cause the debugger to be invoked via a call to BREAK when
the function is called.  fi:package is used to determine from which
Common Lisp package the operation is done.  In a subprocess buffer, the
package is tracked automatically.  In source buffer, the package is
parsed at file visit time."
  (interactive "P")
  (save-excursion
    (let (definer spec name qualifiers specializers)
      (forward-char 1)
      (beginning-of-defun)
      (unless (looking-at "(def")
	(error "Can't parse a top-level defining form"))
      (forward-char 1)			;open paren
      (setq definer (fi::substr-sexp))
      (setq name (fi::substr-sexp))
      (cond ((fi::string-equal-nocase definer "defmethod")
	     (loop as subform = (read-from-string (fi::substr-sexp))
		   as next = (car subform)
		   while (symbolp next)
		   collect next into quals
		   finally do (setq qualifiers (apply 'concat (mapcar 'symbol-name quals)))
		   (setq specializers
		     (loop for spec in next
			   until (member spec '(&optional &rest &key &aux &allow-other-keys))
			   collect (if (atom spec) 't (cadr spec)))))
	     (setq spec
	       (concat "(method " name " "
		       qualifiers " "
		       (format "%S" specializers)
		       " )")))
	    ((or (fi::string-equal-nocase definer "defun")
		 (fi::string-equal-nocase definer "defmacro")
		 (fi::string-equal-nocase definer "defgeneric"))
	     (setq spec name))
	    ((fi::string-equal-nocase definer "deftype")
	     (setq spec (format "(excl::deftype-expander %s)" name)))
	    ((fi::string-equal-nocase definer "define-compiler-macro")
	     (setq spec (format "(:property %s excl::.compiler-macro.)" name)))
	    (t (error "Can't trace a %s" definer)))
      (fi::make-request
	  (lep::toggle-trace :fspec (fi::defontify-string spec) :break current-prefix-arg)
	;; Normal continuation
	(() (what tracep)
	 (message (if tracep "%s is now traced" "%s is now untraced")
		  what))
	;; Error continuation
	((spec) (error)
	 (fi::show-error-text "Cannot (un)trace %s: %s" spec error))))))

;;;; list and edit somethings

(defun fi:list-who-calls (&optional fspec)
  "List all the callers of FSPEC.  `List' means to show them in a buffer in
definition mode.  The source for each definition can be easily found via
key bindings in definition mode.  The default FSPEC is taken from the text
surrounding the point.  fi:package is used to determine from which Common
Lisp package the operation is done.  In a subprocess buffer, the package is
tracked automatically.  In source buffer, the package is parsed at file
visit time."
  (interactive (fi::get-default-symbol "List who calls" nil nil))
  ;; Since this takes a while, tell the user that it has started.
  (message "Finding callers of %s..." fspec)
  (lep::list-fspecs-common fspec
			   'lep::who-calls
			   "Cannot find the callers: %s"
			   "caller"))

(defun fi:list-who-is-called-by (fspec)
  "List all the functions called by FSPEC.  `List' means to show them in a
buffer in definition mode.  The source for each definition can be easily
found via key bindings in definition mode.  The default FSPEC is taken from
the text surrounding the point.  fi:package is used to determine from which
Common Lisp package the operation is done.  In a subprocess buffer, the
package is tracked automatically.  In source buffer, the package is parsed
at file visit time."
  (interactive (fi::get-default-symbol "List who is called by" t t))
  (message "Finding who is called by %s..." fspec)
  (lep::list-fspecs-common fspec
			   'lep::who-is-called-by
			   "Cannot find who is called by: %s"
			   "callee"))

(defun fi:list-generic-function-methods (&optional fspec)
  "List all the generic function methods of FSPEC.  `List' means to show
them in a buffer in definition mode.  The source for each definition can be
easily found via key bindings in definition mode.  The default FSPEC is
taken from the text surrounding the point.  fi:package is used to determine
from which Common Lisp package the operation is done.  In a subprocess
buffer, the package is tracked automatically.  In source buffer, the
package is parsed at file visit time."
  (interactive (fi::get-default-symbol "List generic function methods of" t t))
  ;; Since this takes a while, tell the user that it has started.
  (message "Finding generic function methods of %s..." fspec)
  (lep::list-fspecs-common fspec
			   'scm::generic-function-methods-function-specs
			   "Cannot find the generic function methods: %s"
			   "generic function method"))


(defun fi:edit-who-calls (fspec)
  "Edit all the callers of the function named by FSPEC.
Use ``\\<fi:common-lisp-mode-map>\\[fi:lisp-find-next-definition]'' to find the next definition, if there is one."
  (interactive (fi::get-default-symbol "Edit who calls" nil nil))
  (message "Editing callers...")
  (lep::edit-somethings fspec 'lep::who-calls nil "caller"))

(defun fi:edit-who-is-called-by (fspec)
  "Edit all functions called by FSPEC.
Use ``\\<fi:common-lisp-mode-map>\\[fi:lisp-find-next-definition]'' to find the next definition, if there is one."
  (interactive (fi::get-default-symbol "Edit who is called by" t t))
  (message "Editing callees...")
  (lep::edit-somethings fspec 'lep::who-is-called-by nil "callee"))

(defun fi:edit-generic-function-methods (fspec)
  "Edit all the methods of the generic function named by FSPEC.
Use ``\\<fi:common-lisp-mode-map>\\[fi:lisp-find-next-definition]'' to find
the next definition, if there is one."
  (interactive (fi::get-default-symbol "Edit generic function methods of" t t))
  (message "Editing generic function methods...")
  (lep::edit-somethings fspec
			'scm::generic-function-methods-function-specs
			nil
			"generic function method"))


(defun lep::list-fspecs-common (fspec function msg &optional what)
  (fi::make-request
      ;;fi::frob-case-to-lisp removed - 18jan94 smh
      (lep::list-fspecs-session :function function :fspec fspec)
    ((fspec fi:package what) (the-definitions)
     (lep:display-some-definitions fi:package
				   the-definitions
				   (list 'lep::find-a-definition what fspec)))
    ((msg) (error)
     (fi::show-error-text msg error))))

(defun lep::find-a-definition (string type list-buffer what from-fspec)
  (fi::lisp-find-definition-common string t what from-fspec))

(defun lep::edit-somethings (fspec generator &optional other-window-p what)
  (fi::push-metadot-session
   (or what "definition")
   fspec
   nil
   (fi::make-complex-request
    (scm::edit-sequence-session
     :generator generator
     :package (fi::string-to-keyword (fi::package))
     :fspec fspec)
    ((other-window-p fspec what) (pathname point n-more)
     (fi::show-found-definition fspec pathname point n-more other-window-p)
     (if (or (null n-more) (= 0 n-more)) (fi::pop-metadot-session)))
    ((fspec) (error)
     (when (fi::pop-metadot-session)
       (error "Cannot edit %s: %s" fspec error))))))

;;; describing something

(defun fi:describe-symbol (fspec)
  "Dynamically, in the Common Lisp environment, describe the symbol named
by FSPEC.
fi:package is used to determine from which Common Lisp package the
operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
  (interactive (fi::get-default-symbol "Describe symbol" nil nil))
  (lep::describe-something fspec 'identity))

(defun fi:describe-class (fspec)
  "Dynamically, in the Common Lisp environment, describe the class named by
FSPEC.
fi:package is used to determine from which Common Lisp package the
operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
  (interactive (fi::get-default-symbol "Class name" nil t))
  (lep::describe-something fspec 'common-lisp:find-class))


(defun fi:describe-function (fspec)
  "Dynamically, in the Common Lisp environment, describe the function named
by FSPEC.
fi:package is used to determine from which Common Lisp package the
operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
  (interactive (fi::get-default-symbol "Function spec" t t))
  (lep::describe-something fspec 'fdefinition))


(defun lep::describe-something (fspec function)
  (fi::make-request
   (lep::describe-something-session
    :fspec fspec :function function)
   ;; Normal continuation
   (() (what)
    (fi:show-some-text nil what))
   ;; Error continuation
   ((fspec) (error)
    (fi::show-error-text "Cannot describe %s: %s" fspec error))))


;;; Function documentation

(defun fi:lisp-function-documentation (symbol)
  "Dynamically, in the Common Lisp environment, determine the function
documentation for SYMBOL.
fi:package is used to determine from which Common Lisp package the
operation is done.  In a subprocess buffer, the package is tracked
automatically.  In source buffer, the package is parsed at file visit
time."
  (interactive (fi::get-default-symbol "Function documentation for symbol"
				       nil t))
  (fi::make-request
      (lep::function-documentation-session :package (fi::package)
					   :fspec symbol)
    ;; Normal continuation
    ((symbol) (documentation)
     (if documentation
	 (fi:show-some-text (fi::package) documentation)
       (fi::show-error-text "There is no documentation for %s" symbol)))
    ;; Error continuation
    ((symbol) (error)
     (fi::show-error-text "Cannot find documentation for %s: %s"
			  symbol error))))

(defun fi:compile-file (file)
  "Compile FILE without loading the result into the Lisp environment."
  (interactive "fFile to compile:")
  (fi::compile-or-load-file file ':compile))

(defun fi:compile-and-load-file (file)
  "Compile FILE and load the result of this compilation into the Lisp
environment."
  (interactive "fFile to compile and load:")
  (fi::compile-or-load-file file ':compile-and-load))

(defun fi:load-file (file)
  "Load FILE into the Lisp environment."
  (interactive "fFile to load:")
  (fi::compile-or-load-file file ':load))

(defun fi::compile-or-load-file (file operation)
  (let* ((compilep nil)
	 (msg-start
	  (format "%s %s..."
		  (cond ((eq ':load operation) "Loading")
			((eq ':compile operation)
			 (setq compilep t)
			 "Compiling")
			(t
			 (setq compilep t)
			 "Compiling and loading"))
		  (file-name-nondirectory file))))
    (fi::note-background-request compilep)
    (message "%s" msg-start)
    (fi::make-request
	(lep::compile/load-file-request :pathname file :operation operation
					:warnings-buffer "*ACL Warnings*")
      ((compilep msg-start) (res)
       (fi::note-background-reply (list compilep))
       (message "%sdone." msg-start))
      ((compilep) (error)
       (fi::note-background-reply (list compilep))
       (fi::show-error-text "Could not :%s" error)))))


(defun fi:list-undefined-functions ()
  "Using the cross referencing database in the Lisp environment and
inverse-definition mode, find and display all the functions which are
called but not defined.  See the documentation for
fi:inverse-definition-mode for more information on finding the callers of
the undefined functions.  See the Allegro CL variable
EXCL:*RECORD-XREF-INFO*."
  (interactive)
  (message "Finding undefined functions...")
  (fi::make-request
      (lep::list-undefined-functions-session)
    ((fi:package) (undeffuncs)
      (message "Finding undefined functions...done.")
      (lep:display-some-inverse-definitions
       fi:package
       undeffuncs
       (list 'lep::edit-undefined-function-callers)))
    (() (error)
      (fi::show-error-text "error: %s" error))))

(defun lep::edit-undefined-function-callers (fspec &rest ignore)
  (lep::edit-somethings fspec 'lep::who-calls t))

(defvar fi::list-unused-functions-ignored-packages
    nil ;; '(:comp :r)
  )

(defun fi:list-unused-functions ()
  "Using the cross referencing database in the Lisp environment and
definition mode, find and display all the functions which are
defined but not called.  See the documentation for fi:definition-mode for
more information on finding the definitions of the unused functions.  See
the Allegro CL variable EXCL:*RECORD-XREF-INFO*."
  (interactive)
  (message "Finding unused functions...")
  (fi::make-request
      (lep::list-unused-functions-session
       :ignore-packages fi::list-unused-functions-ignored-packages)
    ((fi:package) (funcs)
      (message "Finding unused functions...done.")
      (lep:display-some-definitions
       fi:package
       funcs
       (list 'lep::find-a-definition "unused function" nil)))
    (() (error)
      (fi::show-error-text "error: %s" error))))

(defun lep::eval-from-lisp (string)
  (list (eval (car (read-from-string string)))))

(defun fi:gdb ()
  "Run GDB on the Lisp image in the *common-lisp* buffer."
  (interactive)
  ;; Ask the lisp for its pathname and pid
  (fi::make-request (lep::gdb-session)
    ;; Normal continuation
    (() (filename pid symbol-file)
     (message "%s %d" filename pid)
     (gdb (concat "gdb " filename))	;gdb command changed between Emacs 18 and 19.
     (process-send-string (get-buffer-process (current-buffer))
		  (format "attach %d\n" pid))
     (unless (string= "" symbol-file)
       (process-send-string (get-buffer-process (current-buffer))
		    (format "symbol-file %s\n" symbol-file))))
    ;; Error continuation
    (() (error)
     (fi::show-error-text "Cannot get pathname and pid: %s" error))))
