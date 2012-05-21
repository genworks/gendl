;; Copyright (c) 1987-2002 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, and to distribute modified
;; versions, provided that this complete copyright and permission notice is
;; maintained, intact, in all copies and supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

;; Create a mode in which each line is a definition and . on that
;; definition brings up the definition in another window

(defvar lep::definition-mode-saved-window-configuration nil)
(defvar lep::inverse-definition-mode-saved-window-configuration nil)

(defvar fi:definition-mode-map nil)
(defvar fi:inverse-definition-mode-map nil)

(defvar fi:definition-mode-mouse-map nil)
(defvar fi:inverse-definition-mode-mouse-map nil)

(defvar fi:definition-mode-hook nil
  "*A hook run from fi:definition-mode.")
(defvar fi:inverse-definition-mode-hook nil
  "*A hook run from fi:inverse-definition-mode.")

(defvar lep::definitions)
(defvar lep::definition-types)
(defvar lep::definition-other-args)
(defvar lep::definition-finding-function)
(defvar lep::inverse-definitions)

(defun fi:definition-mode ()
  "A major mode for viewing definitions of objects defined in the Common
Lisp environment.  The definitions are put in a buffer called
*definitions*, and each line contains the name and type of the definition.
The type is one of:

	:operator	for functions, methods, generic functions
				and macros,
	:type		for classes (types),
	:setf-method	for setf methods, or
	:variable	for constants and variables.

Definition mode is used by other tools, such as the changed-definition
commands, fi:list-who-calls as well as fi:list-buffer-definitions.

The keymap for this mode is bound to fi:definition-mode-map:

<font face=\"Courier New\">\\{fi:definition-mode-map}</font>
Entry to this mode runs the fi:definition-mode-hook."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:definition-mode)
  (setq mode-name "Definition Mode")

  (make-local-variable 'truncate-lines)
  (setq truncate-lines t)
  (fi::definition-mode-fix-buffer)

  (make-local-variable 'lep::definitions)
  (make-local-variable 'lep::definition-types)
  (make-local-variable 'lep::definition-other-args)
  (make-local-variable 'lep::definition-finding-function)
  (make-local-variable 'lep::inverse-definitions)

  (setq lep::inverse-definitions nil)	;In fsf Emacs a local var remains
					;unbound unless explicitly set.

  (if (null fi:definition-mode-map)
      (let ((map (make-keymap)))
	(define-key map "\C-_"  'fi:definition-mode-undo)
	(define-key map "."     'fi:definition-mode-goto-definition)
	(define-key map "\r"    'fi:definition-mode-goto-definition)
	(define-key map "\C-c"  (make-sparse-keymap))
	(define-key map "\C-c." 'fi:definition-mode-goto-definition)
	(define-key map "n"     'fi:definition-mode-goto-next)
	(define-key map "p"     'fi:definition-mode-goto-previous)
	(define-key map "t"     'fi:definition-mode-toggle-trace)
	(define-key map "q"     'fi:definition-mode-quit)
	(setq fi:definition-mode-map map)))

  (use-local-map fi:definition-mode-map)

;;;; epoch specific:
;;;  (when (fboundp 'create-mouse-map)
;;;    (if fi:definition-mode-mouse-map
;;;      nil
;;;    (setq fi:definition-mode-mouse-map (create-mouse-map))
;;;    (define-mouse fi:definition-mode-mouse-map mouse-left mouse-down
;;;		  'lep::dmode-mouse-select)))
;;;  (when fi:definition-mode-mouse-map
;;;    (use-local-mouse-map fi:definition-mode-mouse-map))

  (run-hooks 'fi:definition-mode-hook))

(defun fi:inverse-definition-mode ()
  "A major mode for viewing inverse definitions of objects defined in the
Common Lisp environment.  The definitions are put in a buffer called
*inverse-definitions*, and each line contains the name and type of the
definition.  As definition-mode shows definitions and provides bindings to
go to the source of the definitions, inverse-definition-mode shows
definitions and provides ways to find the users of the definitions. 

The type is one of:

	:operator	for functions, methods, generic functions
				and macros,
	:type		for classes (types),
	:setf-method	for setf methods, or
	:variable	for constants and variables.

Inverse definition mode is used by other tools, such as
fi:list-undefined-functions.

The keymap for this mode is bound to fi:inverse-definition-mode-map:

<font face=\"Courier New\">\\{fi:inverse-definition-mode-map}</font>
Entry to this mode runs the fi:inverse-definition-mode-hook."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:inverse-definition-mode)
  (setq mode-name "Inverse Definition Mode")

  (make-local-variable 'truncate-lines)
  (setq truncate-lines t)
  (fi::definition-mode-fix-buffer)

  (make-local-variable 'lep::definitions)
  (make-local-variable 'lep::definition-types)
  (make-local-variable 'lep::definition-other-args)
  (make-local-variable 'lep::definition-finding-function)
  (make-local-variable 'lep::inverse-definitions)

  (setq lep::inverse-definitions t)

  (if (null fi:inverse-definition-mode-map)
      (let ((map (make-keymap)))
	(define-key map "\C-_"  'fi:definition-mode-undo)
	(define-key map "."     'fi:definition-mode-goto-definition)
	(define-key map "\r"    'fi:definition-mode-goto-definition)
	(define-key map "\C-c"  (make-sparse-keymap))
	(define-key map "\C-c." 'fi:definition-mode-goto-definition)
	(define-key map "n"     'fi:definition-mode-goto-next)
	(define-key map "c"     'fi:inverse-definition-who-calls)
	(define-key map "p"     'fi:definition-mode-goto-previous)
	(define-key map "q"     'fi:inverse-definition-mode-quit)
	(setq fi:inverse-definition-mode-map map)))

  (use-local-map fi:inverse-definition-mode-map)

;;;; epoch specific:
;;;  (when (fboundp 'create-mouse-map)
;;;    (if fi:inverse-definition-mode-mouse-map
;;;      nil
;;;    (setq fi:inverse-definition-mode-mouse-map (create-mouse-map))
;;;    (define-mouse fi:inverse-definition-mode-mouse-map mouse-left mouse-down
;;;  		'lep::dmode-mouse-select)))
;;;  (when fi:inverse-definition-mode-mouse-map
;;;    (use-local-mouse-map fi:inverse-definition-mode-mouse-map))

  (run-hooks 'fi:inverse-definition-mode-hook))

(defun fi::definition-mode-fix-buffer ()
  (let ((buffer-read-only nil))
    (fi:map-lines
     (function
      (lambda ()
	(when (looking-at "[ \t]")
	  (delete-char -1)
	  (delete-horizontal-space)
	  (insert " ")))))))

(defun fi:definition-mode-undo ()
  "Perform the undo in the dmode buffer.  This has the effect of pop'ing
back to the previous contents of the definition-mode buffer."
  (interactive)
  (let ((buffer-read-only nil))
    (undo)))

(defun fi:list-buffer-definitions ()
  "List the definition for all the objects in the current buffer.  That is,
use the current buffer and display all the definitions contained in it."
  (interactive)
  (let ((buffer (current-buffer)))
    (fi::make-request
     (scm::file-definitions-session
      :pathname (buffer-file-name buffer))
     ;; Normal continuation
     ((buffer fi:package) (the-definitions)
      (lep:display-some-definitions 
       fi:package
       the-definitions
       (list 'lep::find-buffer-definition buffer)))
     ;; Error continuation
     ((buffer) (error)
      (message "Cannot find the definitions of buffer %s: %s"
	       buffer error)))))

;;;; epoch specific:
;;;(defun lep::dmode-mouse-select (info)
;;;  (pop-to-buffer (second info))
;;;  (goto-char (car info))
;;;  (beginning-of-line)
;;;  (fi:definition-mode-goto-definition))

(defun fi:definition-mode-quit ()
  "Quit definition mode and restore the window configuration as it was
before definition mode was entered."
  (interactive)
  (bury-buffer)
  (set-window-configuration lep::definition-mode-saved-window-configuration))

(defun fi:inverse-definition-mode-quit ()
  "Quit inverse-definition mode and restore the window configuration as it
was before inverse-definition mode was entered."
  (interactive)
  (bury-buffer)
  (set-window-configuration
   lep::inverse-definition-mode-saved-window-configuration))

(defun fi:definition-mode-goto-definition ()
  "Find the definition associated with the entry on the current line.  This
uses the same mechanism as fi:lisp-find-definition, using dynamic
information in the Common Lisp environment."
  (interactive)
  (message "Finding%s definition..."
	   (if lep::inverse-definitions " inverse" ""))
  (let* ((n (count-lines (point-min)
			 (save-excursion (beginning-of-line) (point))))
	 (buffer (current-buffer))
	 (def (nth n lep::definitions))
	 (other (nth n lep::definition-other-args))
	 (type (nth n lep::definition-types)))
    (when (and (not (equal type '(nil))) lep::definition-finding-function)
      (apply (car lep::definition-finding-function)
	     def type buffer
	     (append other (cdr lep::definition-finding-function))))))

(defun fi:inverse-definition-who-calls ()
  "Find the callers of a function in inverse definition mode, displaying
the results in definition mode."
  (interactive)
  (let* ((n (count-lines (point-min)
			 (save-excursion (beginning-of-line) (point))))
	 (def (nth n lep::definitions))
	 (type (nth n lep::definition-types)))
    (when (not (equal type '(nil)))
      (fi:list-who-calls def))))

(defun fi:definition-mode-toggle-trace ()
  "Toggle tracing for the definition under the point.  This is equivalent
to fi:toggle-trace-definition."
  (interactive)
  (let* ((n (count-lines (point-min)
			 (save-excursion (beginning-of-line) (point))))
	 (def (nth n lep::definitions))
	 (type (nth n lep::definition-types)))
    (when (not (equal type '(nil)))
      (fi:toggle-trace-definition def))))

(defun fi:definition-mode-goto-next ()
  "Find the definition on the next line.  Equivalent to ``\\<global-map>\\[next-line]''
followed by \
``\\<fi:definition-mode-map>\\[fi:definition-mode-goto-definition]'' \
in definition mode."
  (interactive)
  (forward-line 1)
  (fi:definition-mode-goto-definition))

(defun fi:definition-mode-goto-previous ()
  "Find the definition on the previous line.  Equivalent to ``\\<global-map>\\[previous-line]''
followed by \
``\\<fi:definition-mode-map>\\[fi:definition-mode-goto-definition]'' \
in definition mode."
  (interactive)
  (forward-line -1)
  (fi:definition-mode-goto-definition))

(defun lep::find-buffer-definition (string type list-buffer buffer)
  (unless (bufferp buffer) (setq buffer (find-file-noselect buffer)))
  (fi::make-request
   (scm::find-buffer-definition-session
    :pathname (buffer-file-name buffer) 
    :fspec string
    :type type
    :package (save-excursion (set-buffer buffer)
			     (fi::string-to-keyword (fi::package))))
   ;; Normal continuation
   ((string list-buffer) (pathname point n-more)
    (fi::show-found-definition string pathname point n-more t)
    (recenter 0)
    (switch-to-buffer-other-window list-buffer))
   ;; Error continuation
   ((string buffer) (error)
    (error "Cannot find the definition of %s in %s: %s"
	   string buffer error))))

(defun lep:display-some-definitions (xpackage buffer-definitions
				     fn-and-arguments
				     &optional buffer-name)
  (let ((buffer (get-buffer-create (or buffer-name "*definitions*"))))
    (fi::goto-definitions-buffer
     buffer
     'lep::definition-mode-saved-window-configuration)
    (save-excursion
      (set-buffer buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq truncate-lines t)		;smh 22jul94
      (mapc (function (lambda (x) 
			(princ (car x) (current-buffer))
			(unless (equal '(nil) (second x))
			  (insert ", ")
			  (princ (second x) (current-buffer)))
			(when (third x)	;smh 22jul94
			  (insert ", ")
			  (princ (third x) (current-buffer)))
			(insert "\n")))
	    buffer-definitions)
      (fi:definition-mode)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (setq lep::definitions (mapcar 'car buffer-definitions))
      (setq lep::definition-types (mapcar 'second buffer-definitions))
      (setq lep::definition-other-args (mapcar 'third buffer-definitions))
      (setq lep::definition-finding-function fn-and-arguments)
      (setq fi:package xpackage)
      (goto-char (point-min)))))

(defun lep:display-some-inverse-definitions (xpackage buffer-definitions
					     fn-and-arguments
					     &optional buffer-name)
  (let ((buffer (get-buffer-create (or buffer-name "*inverse-definitions*"))))
    (fi::goto-definitions-buffer
     buffer
     'lep::inverse-definition-mode-saved-window-configuration)
    (save-excursion
      (set-buffer buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (mapc '(lambda (x) 
	      (princ (car x) (current-buffer))
	      (unless (equal '(nil) (second x))
		(insert ", ")
		(princ (second x) (current-buffer)))
	      (insert "\n"))
	    buffer-definitions)
      (fi:inverse-definition-mode)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (setq lep::definitions (mapcar 'car buffer-definitions))
      (setq lep::definition-types (mapcar 'second buffer-definitions))
      (setq lep::definition-other-args (mapcar 'third buffer-definitions))
      (setq lep::definition-finding-function fn-and-arguments)
      (setq fi:package xpackage)
      (goto-char (point-min)))))

(defun fi::goto-definitions-buffer (buffer config-var)
  (unless (eq buffer (current-buffer))
    (set config-var (current-window-configuration))
    (fi::display-pop-up-window buffer)))
