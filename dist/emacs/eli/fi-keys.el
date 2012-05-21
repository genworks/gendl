;; Copyright (c) 1987-2002 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, and to distribute modified
;; versions, provided that this complete copyright and permission notice is
;; maintained, intact, in all copies and supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

(cond ((or (eq fi::emacs-type 'xemacs19)
	   (eq fi::emacs-type 'xemacs20))
       (require 'tags "etags"))
      ((or (eq fi::emacs-type 'emacs19)
	   (eq fi::emacs-type 'emacs20))
       (require 'etags "etags"))
      (t (require 'tags)))

(defvar fi:subprocess-super-key-map nil
  "Used by fi:subprocess-superkey as the place where super key bindings are
kept.  Buffer local.")

(make-variable-buffer-local 'fi:subprocess-super-key-map)

(defvar fi:superkey-shadow-universal-argument t
  "*If non-nil, then make C-u a superkey in subprocess modes, otherwise
leave it alone.")

(defvar fi:find-tag-lock t
  "*If non-nil, then the first time find-tag or find-tag-other-window are
executed a buffer will be displayed explaining the method for finding
Lisp definitions.")

(defvar fi:check-unbalanced-parentheses-when-saving t
  "*If non-nil, for the Lisp editing modes (Common Lisp, Emacs Lisp, and
Franz Lisp) check for unbalanced parentheses before writing the file.
If the value is T, then ask whether or not the file should be written ``as
is'' if there are too many or few parens--answering no leaves the point at
the place of error.  If the value is 'warn, then a warning is issued and
the file is written.")

(defvar fi:define-global-lisp-mode-bindings t
  "*This variable is obsolete.  Use fi:legacy-keybindings instead.")

(defvar fi:legacy-keybindings t
  "*If non-nil then define the global, legacy keybindings, which in some
cases are in violation of the Elisp major mode conventions outlined in the
Emacs Lisp Manual.  For compatibility reasons the value of this variable is
`t' by default.")

(defvar fi:menu-bar-single-item t
  "*If non-nil then put a single item onto the menu-bar.  Otherwise, the
sub-menus in the single menu are put onto the menu-bar.  This variable is
ignored in all but XEmacs and Emacs 21 and later.")

(defvar fi:arglist-on-space t
  "*If non-nil, then bind SPC to a function that retrieves arglist
information and displays it according to the value of the variable
fi:auto-arglist-pop-up-style.")

(defvar fi:use-web-documentation t
  "*If non-nil, then fi:lisp-function-documentation is replaced with
fi:manual for looking up documentation in the Allegro CL documentation
set.")


;;;;
;;; Key defs
;;;;

(defvar fi::.defkey-marker. (cons 'not-a 'keymap))

(defun fi::defkey (map key func condition)
  (when condition
    (cond ((and (consp map) (eq fi::.defkey-marker. (car map)))
	   (dolist (m (cdr map)) (define-key m key func)))
	  (t (define-key map key func)))))

(defun fi::initialize-mode-map (mode-map-sym &optional mode-super-map-sym type)
  (when (null (symbol-value mode-map-sym))
    (let* ((ext fi:legacy-keybindings)
	   (arglist fi:arglist-on-space)
	   (elsrc (eq major-mode 'fi:emacs-lisp-mode))
	   (clsrc (eq major-mode 'fi:common-lisp-mode))
	   (flsrc (eq major-mode 'fi:franz-lisp-mode))
	   (src (or elsrc clsrc flsrc))
	   (subproc (not src))
	   (listener (memq type '(sub-lisp tcp-lisp)))
	   (clistener (and listener
			   (not (eq major-mode 'fi:inferior-franz-lisp-mode))))
	   (flistener (and listener
			   (eq major-mode 'fi:inferior-franz-lisp-mode)))
	   (non-lisp-subproc (and subproc (not listener)))
	   
	   (super (and subproc
		       mode-super-map-sym
		       fi:subprocess-enable-superkeys))
	   (lisp (memq major-mode '(fi:common-lisp-mode
				    fi:inferior-common-lisp-mode
				    fi:lisp-listener-mode
				    fi:franz-lisp-mode
				    fi:inferior-franz-lisp-mode)))
	   (clisp (memq major-mode '(fi:common-lisp-mode
				     fi:inferior-common-lisp-mode
				     fi:lisp-listener-mode)))
	   (indent (and (or src lisp) fi:lisp-do-indentation))
	   (rlogin (memq type '(telnet rlogin)))
	   (shell (eq type 'shell))
	   (sub-lisp (eq type 'sub-lisp))
	   (tcplisp (eq type 'tcp-lisp))
	   (remote
	    (and sub-lisp
		 (or fi::lisp-is-remote
		     ;; fi::common-lisp-connection-type might be bound
		     ;; differently at connection startup time, so we'll just
		     ;; always use the backdoor approach.
		     (null fi::common-lisp-connection-type)
		     t)))
	   (universal
	    (and super
		 (or (not (eq 'universal-argument
			      (lookup-key global-map "\C-u")))
		     fi:superkey-shadow-universal-argument)))
	   (comint (and (or (eq fi::emacs-type 'emacs19)
			    (eq fi::emacs-type 'emacs20))
			(boundp 'comint-mode-map)
			comint-mode-map))
	   
	   (map (make-keymap))
	   (smap (make-sparse-keymap))
	   (cmap (make-keymap))
	   (xmap (make-sparse-keymap))
	   (emap (make-sparse-keymap))
	   (csmaps (list fi::.defkey-marker. cmap smap)))
      
      (when sub-lisp
	(let ((i (1+ ?\C-z))
	      (l ;; use this instead of (length map)
	       128))
	  ;; For now we can't implement raw mode and also work with wnn
	  ;; kanji input translation.  Until we figure out a good solution,
	  ;; raw mode will be disabled in Mule.
	  (unless (or (boundp 'nemacs-version) (boundp 'mule-version))
	    (while (< i l)
	      (define-key map (char-to-string i) 'fi:self-insert-command)
	      (setq i (1+ i)))))
	;; fix C-_
	(define-key map (char-to-string 31) nil))
      
;;;; main map
      (fi::defkey map "\C-a" 'fi:subprocess-beginning-of-line subproc)
      (fi::defkey map "\C-d" 'fi:subprocess-superkey super)
      (fi::defkey map "\C-c" cmap t)
      (fi::defkey map "\C-i" 'fi:lisp-indent-line (and (or elsrc lisp) indent))
      (fi::defkey map "\C-i" 'lisp-indent-line (and (or elsrc lisp)
						    (not indent)))
      (fi::defkey map "\C-m" 'fi:subprocess-send-input non-lisp-subproc)
      (fi::defkey map "\C-m" (if listener
				 'fi:inferior-lisp-newline
			       'fi:lisp-mode-newline)
		  (not non-lisp-subproc))
      (fi::defkey map "\C-o" 'fi:subprocess-superkey super)
      (fi::defkey map "\C-u" 'fi:subprocess-superkey universal)
      (fi::defkey map "\C-w" 'fi:subprocess-superkey super)
      (fi::defkey map "\C-x" xmap (and subproc lisp ext))
      (fi::defkey map "\C-z" 'fi:subprocess-superkey super)
      (fi::defkey map "\C-\\" 'fi:subprocess-superkey super)
      (fi::defkey map "\C-?" 'backward-delete-char-untabify t)
      (fi::defkey map "\e" emap t)
      (fi::defkey map ";" 'fi:lisp-semicolon indent)
      (fi::defkey map " " 'fi:arglist-lisp-space (and clisp arglist))
      (fi::defkey map "!" 'fi:shell-mode-bang
		  (and fi:shell-mode-use-history shell))
      (when comint
	(fi::defkey map [menu-bar] (lookup-key comint-mode-map [menu-bar]) t))

;;;; ESC map
      (fi::defkey emap "\C-i" 'fi:lisp-complete-symbol clisp)
      (fi::defkey emap "\C-m" 'fi:inferior-lisp-input-sexp (and ext listener))
      (fi::defkey emap "\C-q" (if indent 'fi:indent-sexp 'indent-sexp)
		  (or src listener))
      (fi::defkey emap "\C-x" 'fi:lisp-eval-or-compile-defun clsrc)
      (fi::defkey emap "\C-x" 'eval-defun elsrc)
      (fi::defkey emap "A" 'fi:lisp-arglist (and ext clisp))
      (fi::defkey emap "C" 'fi:list-who-calls (and ext clisp))
      (fi::defkey emap "D" 'fi:describe-symbol (and ext clisp))
      (fi::defkey emap "F" (if fi:use-web-documentation
			       'fi:manual
			     'fi:lisp-function-documentation)
		  (and ext clisp))
      (fi::defkey emap "M" 'fi:lisp-macroexpand (and ext clisp))
      (fi::defkey emap "T" 'fi:toggle-trace-definition (and ext clisp))
      (fi::defkey emap "W" 'fi:lisp-macroexpand-recursively (and ext clisp))
      
;;;; C-x map
      (fi::defkey xmap "\C-m" 'fi:inferior-lisp-input-list
		  (and subproc lisp ext))

;;;; super map
      (fi::defkey csmaps "\C-\\" 'fi:rlogin-send-quit rlogin)
      (fi::defkey csmaps "\C-\\" 'fi:subprocess-quit shell)
      (fi::defkey csmaps "\C-\\" 'fi:subprocess-quit sub-lisp)
      (fi::defkey csmaps "\C-\\" 'fi:tcp-lisp-listener-kill-process tcplisp)
      (fi::defkey csmaps "\C-d" 'fi:remote-lisp-send-eof (and sub-lisp remote))
      (fi::defkey csmaps "\C-d" 'fi:rlogin-send-eof rlogin)
      (fi::defkey csmaps "\C-d" 'fi:subprocess-send-eof
		  (or (and sub-lisp (not remote)) shell))
      (fi::defkey csmaps "\C-d" 'fi:tcp-lisp-listener-send-eof tcplisp)
      (fi::defkey csmaps "\C-o" 'fi:subprocess-send-flush subproc)
      (fi::defkey csmaps "\C-u" 'fi:subprocess-kill-input subproc)
      (fi::defkey csmaps "\C-w" 'fi:subprocess-backward-kill-word subproc)
      (fi::defkey smap "\C-z" 'fi:rlogin-send-stop rlogin)
      (fi::defkey smap "\C-z" 'fi:subprocess-suspend shell)
      
;;;; C-c map
      ;; NOTE: C-\ is defined above
      (fi::defkey cmap "\C-a" 'fi:lisp-arglist clisp)
      (fi::defkey cmap "\C-b" 'fi:lisp-eval-or-compile-current-buffer clsrc)
      (fi::defkey cmap "\C-c" 'fi:interrupt-listener clistener)
      (fi::defkey cmap "\C-c" 'fi:rlogin-send-interrupt rlogin)
      (fi::defkey cmap "\C-c" 'fi:subprocess-interrupt (or flistener shell))
      ;; NOTE: C-d is defined above
      (fi::defkey cmap "\C-e" 'fi:end-of-defun (or lisp src))
      (fi::defkey cmap "\C-f" (if fi:use-web-documentation
				  'fi:manual
				'fi:lisp-function-documentation)
		  clisp)
      (fi::defkey cmap "\C-f" 'fi:telnet-start-garbage-filter non-lisp-subproc)
      ;; NOTE: don't use C-g (it's for quit)
      ;; NOTE: don't use C-h (it's for help)
      (fi::defkey cmap "\C-i" 'fi:lisp-complete-symbol clisp)
      (fi::defkey cmap "\C-j" 'fi:toggle-to-lisp clisp)
      (fi::defkey cmap "\C-k" 'fi:subprocess-kill-output subproc)
      (fi::defkey cmap "\C-l" 'fi:list-input-ring subproc)
;;;;This binding is too dangerous.  All it takes is this sequence to really
;;;;screw someone (happened to me many times):
;;;;1. Hold C-c so the keyboard repeats it, sending repeated ^C's to a the
;;;;   subprocess
;;;;2. Let go of C-c... which might have a pending C-c, since C-cC-c sends
;;;;   a single ^C.
;;;;3. Type ENTER to see if the process is responding... now everything
;;;;   in the region will be sent to the subshell... ouch.
;;;      (fi::defkey cmap "\C-m" 'fi:subprocess-input-region
;;;		  (and subproc (not clisp)))
      (fi::defkey cmap "\C-m" 'fi:lisp-macroexpand clisp)
      (fi::defkey cmap "\C-n" 'fi:push-input subproc)
      ;; NOTE: C-o is defined above
      (fi::defkey cmap "\C-p" 'fi:pop-input subproc)
      (fi::defkey cmap "\C-q" (if indent 'fi:indent-sexp 'indent-sexp)
		  (or lisp src))
      (fi::defkey cmap "\C-r" 'fi:re-search-backward-input subproc)
      (fi::defkey cmap "\C-r" 'fi:lisp-eval-or-compile-region clsrc)
      (fi::defkey cmap "\C-s" 'fi:re-search-forward-input subproc)
      (fi::defkey cmap "\C-s" 'fi:lisp-eval-or-compile-last-sexp clsrc)
      (fi::defkey cmap "\C-t" 'fi:toggle-trace-definition
		  (and (not ext) clisp))
      (fi::defkey cmap "\C-t" 'fi:trace-definer (and ext clisp))
      ;; NOTE: C-u is defined above
      (fi::defkey cmap "\C-v" 'fi:subprocess-show-output subproc)
      ;; NOTE: C-w is defined above
      (fi::defkey cmap "\C-x" 'eval-defun elsrc)
      (fi::defkey cmap "\C-x" 'fi:lisp-eval-or-compile-defun clsrc)
      (fi::defkey cmap "\C-y" 'fi:kill-definition clisp)
      (fi::defkey cmap "\C-z" 'fi:list-who-calls clisp)
      (fi::defkey cmap "\C-z" 'fi:rlogin-send-stop rlogin)
      (fi::defkey cmap "\C-z" 'fi:subprocess-suspend shell)

      ;; These are OK to define for all, but strictly speaking are reserved
      ;; for minor modes and if someone uses a minor mode they might get
      ;; stepped on...
      (fi::defkey cmap " " 'fi:lisp-delete-pop-up-window clisp)
      (fi::defkey cmap "%" 'fi:extract-list (or src lisp))
      (fi::defkey cmap "&" 'fi:scan-stack clistener)
      (fi::defkey cmap "*" 'fi:pop-definition-mark clisp)
      (fi::defkey cmap "," 'fi:lisp-find-next-definition clisp)
      (fi::defkey cmap "-" 'fi:log-functional-change src)
      (fi::defkey cmap "=" 'fi:lisp-sync-current-working-directory listener)
      (fi::defkey cmap "=" 'fi:shell-sync-current-working-directory
		  non-lisp-subproc)
      (fi::defkey cmap "." 'fi:lisp-find-definition clisp)
      (fi::defkey cmap ";" 'fi:comment-region (or lisp src))
      (fi::defkey cmap "(" 'fi:lisp-macroexpand-recursively clisp)
      (fi::defkey cmap "<" 'fi:previous-top-level-form src)
      (fi::defkey cmap ">" 'fi:next-top-level-form src)
      (fi::defkey cmap "]" 'fi:super-paren (or src listener))
      (fi::defkey cmap "?" 'fi:lisp-apropos clisp)
      (fi::defkey cmap "^" 'fi:center-defun src)
      (fi::defkey cmap "4" (make-sparse-keymap) clisp)
      (fi::defkey cmap "4." 'fi:lisp-find-definition-other-window clisp)

      ;; These are contrary to the coding conventions, so must be `ext'.
      ;; Each of the functions bound to these keys should be available
      ;; elsewhere.
      (fi::defkey cmap "A" 'fi:lisp-arglist (and clisp ext))
      (fi::defkey cmap "C" 'fi:list-who-calls (and clisp ext))
      (fi::defkey cmap "D" 'fi:describe-symbol (and clisp ext))
      (fi::defkey cmap "F" (if fi:use-web-documentation
			       'fi:manual
			     'fi:lisp-function-documentation)
		  (and clisp ext))
      (fi::defkey cmap "K" 'fi:kill-definition (and clisp ext))
      (fi::defkey cmap "L" 'fi:toggle-to-lisp (and clisp ext))
      (fi::defkey cmap "M" 'fi:lisp-macroexpand (and clisp ext))
      (fi::defkey cmap "S" 'fi:scan-stack (and clisp ext))
      (fi::defkey cmap "T" 'fi:toggle-trace-definition (and clisp ext))
      (fi::defkey cmap "W" 'fi:lisp-macroexpand-recursively (and clisp ext))

      (fi::defkey cmap "a" 'fi:lisp-arglist (and clisp ext))
      (fi::defkey cmap "c" 'fi:list-who-calls (and clisp ext))
      (fi::defkey cmap "d" 'fi:describe-symbol (and clisp ext))
      (fi::defkey cmap "f" (if fi:use-web-documentation
			       'fi:manual
			     'fi:lisp-function-documentation)
		  (and clisp ext))
      (fi::defkey cmap "i" 'fi:insert-arglist (and clisp ext))
      (fi::defkey cmap "k" 'fi:kill-definition (and clisp ext))
      (fi::defkey cmap "l" 'fi:toggle-to-lisp (and clisp ext))
      (fi::defkey cmap "m" 'fi:lisp-macroexpand (and clisp ext))
      (fi::defkey cmap "s" 'fi:scan-stack (and clistener ext))
      (fi::defkey cmap "t" 'fi:toggle-trace-definition (and clisp ext))
      (fi::defkey cmap "w" 'fi:lisp-macroexpand-recursively (and clisp ext))
      
      (set mode-map-sym map)
      (when mode-super-map-sym
	(set mode-super-map-sym smap))
      
      t))
  (setq fi:subprocess-super-key-map (symbol-value mode-super-map-sym)))

;;;;;;;;;;;;;;;;;;;;; inferior lisp mode related functions

(defvar fi:lisp-mode-auto-indent t
  "*If non-nil, then the command bound to \\r, fi:lisp-mode-newline, will
indent, newline and indent.  It does this by funcalling the value bound to
indent-line-function.")

(defun fi:lisp-mode-newline ()
  "Depending on the value of fi:lisp-mode-auto-indent, either (if nil)
insert a newline at the point or (if non-nil) indent, insert a newline and
indent again."
  (interactive)
  (if fi:lisp-mode-auto-indent
      (progn
	(save-excursion (funcall indent-line-function))
	(newline 1)
	(funcall indent-line-function))
    (newline 1)))

(defvar fi:raw-mode nil
  "*If non-nil, then the inferior lisp process gets characters as they are
typed, not when a complete expressions has been entered.  This means that
no input editing of expressions can occur.  The intention of this feature
is that it be used by programs written in Common Lisp that need to read
characters without newlines after them.  See the example in the Allegro
User Guide for more information.")

(defvar fi:raw-mode-echo t
  "*If non-nil, then echo characters in the inferior lisp buffer when
fi:raw-mode is non-nil.")

(defun fi:self-insert-command (arg)
  (interactive "P")
  (cond (fi:raw-mode
	 (when fi:raw-mode-echo (insert last-input-char))
	 (let ((process (get-buffer-process (current-buffer))))
	   (process-send-string process (char-to-string last-input-char))
	   (set-marker (process-mark process) (point))))
	(t (self-insert-command (prefix-numeric-value arg)))))

(defun fi:inferior-lisp-newline ()
  "When at the end of the buffer, insert a newline into a Lisp subprocess
buffer, and if a complete form has been entered, send the input to the Lisp
subprocess.  This allows partially complete, multi-line expressions to be
edited before Lisp sees them.

If not at the end of the buffer, this function does its best to find a
complete form around the point, copy it to the end of the buffer, and send
it to the Lisp subprocess."
  (interactive)
  (when (fboundp 'fi::maybe-update-default-right-margin)
    (fi::maybe-update-default-right-margin))
  (if (eobp)
      (let ((start (marker-position
		    (process-mark (get-buffer-process (current-buffer)))))
	    (send-that-sexp t))
	(goto-char start)
	(while (and (not (eobp))
		    (condition-case ()
			(progn (forward-sexp 1) t)
		      (error (setq send-that-sexp nil))))
	  (while (looking-at ")")
	    ;; Can either signal an error or delete them silently.  Hmm,
	    ;; for now we'll signal the error:
	    ;;(delete-char 1)
	    (error "too many )'s")
	    ))
	(goto-char (point-max))
	(if send-that-sexp
	    (fi:subprocess-send-input)
	  (progn
	    (newline)
	    (funcall indent-line-function))))

    ;;NOT AT THE END OF THE BUFFER!
    ;; find the user's input contained around the cursor and send that to
    ;; the inferior lisp
    (let ((start-of-last-prompt
	   (save-excursion
	     (or (and (re-search-backward fi::prompt-pattern nil t)
		      (point))
		 (point-max))))
	  start end)
      (if (or (and (bolp) (looking-at "("))
	      (re-search-backward "^(" start-of-last-prompt t)
	      (prog1 (re-search-backward fi::prompt-pattern nil t)
		(goto-char (match-end 0))))
	  (progn
	    (setq start (point))
	    (let* ((eol (save-excursion (end-of-line) (point)))
		   (state (save-excursion (parse-partial-sexp start eol)))
		   (depth (car state)))
	      (if (zerop depth)
		  (setq end eol)
		(setq end
		  (condition-case ()
		      (save-excursion
			(if (< depth 0)
			    (up-list (- depth))
			  (goto-char eol)
			  (up-list depth))
			(point))
		    (error nil))))

	      (if (or (null end) (= end (point-max)))
		  (progn
		    (goto-char (point-max))
		    (fi:inferior-lisp-newline))
		(fi:subprocess-input-region start end))))
	(error "couldn't find start of input")))))

(defun fi:subprocess-input-region (start end)
  "Send the region defined by the point and mark to the Lisp subprocess.
When called from a program give START and END buffer positions."
  (interactive "r")
  (let* ((process (get-buffer-process (current-buffer)))
	 (string (buffer-substring start end)))
    (goto-char (point-max))
    (setq start (point))
    (setq fi::last-input-start (point))
    (insert string)
    (if (not (bolp)) (insert "\n"))
    (setq end (point))
    (setq fi::last-input-end (point))
    ;; bug10814: behave like fi:subprocess-send-input; from rgr, 29-May-01.
    (fi::subprocess-watch-for-special-commands)
    (process-send-region process start end)
    (fi::input-ring-save fi::last-input-start (1- fi::last-input-end))
    (set-marker (process-mark process) (point))))

(defun fi:inferior-lisp-input-sexp (&optional arg)
  "Send the s-expression after the point to the Lisp subprocess.
With prefix arg, ARG, send that many s-expressions."
  (interactive "p")
  (fi:inferior-lisp-send-input arg 'sexp))

(defun fi:inferior-lisp-input-list (&optional arg)
  "Send the list before the point to the Lisp subprocess.
With prefix arg, ARG, send that many lists."
  (interactive "p")
  (fi:inferior-lisp-send-input arg 'lists))

(defun fi:inferior-lisp-send-input (arg type)
  "Send ARG, which is an s-expression, to the Lisp subprocess. TYPE
must be either 'sexps or 'lists, specifying whether lists or
s-expressions should be parsed (internally, either `(scan-sexps)' or
`(scan-lists)' is used). If at the end of buffer, everything typed since
the last output from the Lisp subprocess is collected and sent to the Lisp
subprocess.  With an argument, only the specified number of s-expressions
or lists from the end of the buffer are sent. If in the middle of the
buffer, the current s-expression(s) or list(s) is(are) copied to the end of
the buffer and then sent. An argument specifies the number of s-expressions
or lists to be sent. If s-expressions are being parsed,the cursor
follows a closing parenthesis, the preceding s-expression(s) is(are)
processed.  If the cursor is at an opening parenthesis, the following
s-expression(s) is(are) processed.  If the cursor is at a closing
parenthesis, the preceding s-expression(s) is(are) processed.  Otherwise,
the enclosing s-expression(s) is(are) processed.  If lists are being
parsed, the enclosing list is processed."
  (if (and (eobp) (null arg))
      (progn
	(setq fi::last-input-start
	  (marker-position
	   (process-mark (get-buffer-process (current-buffer)))))
	(insert "\n")
	(funcall indent-line-function)
	(setq fi::last-input-end (point)))

    ;; we are in the middle of the buffer somewhere and need to collect
    ;; and s-exp to re-send
    ;; we grab everything from the end of the current line back to the end
    ;; of the last prompt
    ;;
    (let ((exp-to-resend "")
	  (start-resend (point)))
      (if (null arg) (setq arg 1))
      (if (equal type 'sexp)
	  (setq exp-to-resend
	    (buffer-substring
	     (setq start-resend
	       (save-excursion
		 (cond
		   ((= (preceding-char) ?\)) (scan-sexps (point) (- arg)))
		   ((= (following-char) ?\() (point))
		   ((= (following-char) ?\))
		    (forward-char 1) (scan-sexps (point) (- arg)))
		   ((not (memq (char-syntax (preceding-char)) '(?w ?_)))
		    (point))
		   (t (scan-sexps (point) (- arg))))))
	     (save-excursion
	       (cond
		((= (preceding-char) ?\)) (point))
		((= (following-char) ?\() (scan-sexps (point) arg))
		((= (following-char) ?\)) (forward-char 1) (point))
		((not (memq (char-syntax (following-char)) '(?w ?_)))
		 (point))
		(t (scan-sexps (point) arg))))))
	(setq exp-to-resend
	  (buffer-substring
	   (setq start-resend (scan-lists (point) (- arg) 1))
	   (scan-lists (point) arg 1))))
      (if (eobp)
	  (progn
	    (insert "\n")
	    (funcall indent-line-function)
	    (setq fi::last-input-start start-resend)
	    (setq fi::last-input-end (point-max)))
	(progn
	  (goto-char (point-max))
	  (setq fi::last-input-start (point))
	  (insert exp-to-resend)
	  (if (not (bolp)) (insert "\n"))
	  (setq fi::last-input-end (point))))))
  (let ((process (get-buffer-process (current-buffer))))
    (process-send-region process fi::last-input-start fi::last-input-end)
    (fi::input-ring-save fi::last-input-start (1- fi::last-input-end))
    (set-marker (process-mark process) (point))))

;;;;;;;;;;;;;;;;;;;;; general subprocess related functions

(defun fi:subprocess-superkey (prefix)
  "This function implements superkeys in subprocess buffers.  Any key which
is bound to this function is, by definition, a superkey.  A superkey is
treated specially when at the end of a subprocess buffer, but has its
normal, global, binding when used elsewhere in the buffer.
The key takes its binding from the fi:subprocess-super-key-map keymap,
which is a buffer local variable."
  (interactive "P")
  (let ((key (this-command-keys)))
    (when prefix
      ;; Remove prefix digits
      (setq key (subseq key (+ 1 (length (format "%d" prefix))))))
    (if (eobp)
	(fi::subprocess-reprocess-keys fi:subprocess-super-key-map key)
      (fi::subprocess-reprocess-keys (current-global-map) key))))

(defun fi::subprocess-reprocess-keys (&optional map key)
  "Reprocess KEY or the last key sequence (which may be incomplete) in MAP.
This is used to reprocess a key sequence as if it were seen in another
context, e.g. to process global bindings of keys from a subprocess
buffer (in fi:shell-mode or fi:inferior-lisp-mode) when some keys are hit
other than at the end of the buffer."
  (if (null map) (setq map (current-global-map)))
  (let* ((last-key (if key
		       (if (integerp key)
			   (char-to-string key)
			 key)
		     (this-command-keys)))
	 (last-binding (lookup-key map last-key)))
    (while (keymapp last-binding)
      (setq last-binding
	(lookup-key last-binding
		    (setq last-key (char-to-string (read-char))))))
    (if (commandp last-binding)
	(call-interactively last-binding)
      (ding))))

(defun fi:subprocess-beginning-of-line (arg)
  "Moves point to beginning of line, just like (beginning-of-line ARG),
except that if the pattern at the beginning of the line matches the
current subprocess prompt pattern, this function skips over it.
Successive calls to this function toggle between the real beginning of line
and the beginning of input (after the prompt).  If there is no prompt on
the line, then successive calls toggle between the real beginning of line
and the first non-whitespace character.

With argument ARG non nil or 1, move forward ARG - 1 lines first."
  (interactive "p")
  (let ((old (point))
	(bol (bolp))
	new)
    (beginning-of-line arg)
    (cond ((looking-at fi::prompt-pattern)
	   (if (= old (second (setq new (match-data 0))))
	       (goto-char (first new))
	     (goto-char (second new))))
	  (bol (while (looking-at "[ \t]") (forward-char))))))

(defun fi:subprocess-backward-kill-word (arg)
  "Kill previous word in current subprocess input line.  This function
takes care not to delete past the beginning of the the current input.
With argument ARG, kill that many words."
  (interactive "p")
  (save-restriction
    (narrow-to-region
     (marker-position (process-mark (get-buffer-process (current-buffer))))
     (point))
    (backward-kill-word arg)))

(defun fi:subprocess-send-input ()
  "Send input to the subprocess.  At end of buffer, send all text after
last output as input to the subshell, including a newline inserted at the
end.  When not at end, copy current line to the end of the buffer and
send it,after first attempting to discard any prompt at the beginning of
the line by matching the regexp that is the value of
the buffer-local fi::prompt-pattern, which is initialized by each
subprocess mode."
  (interactive)
  (if fi::shell-completions-window (fi::shell-completion-cleanup))
  (end-of-line)
  (if (eobp)
      (progn
	(setq fi::last-input-start
	  (marker-position
	   (process-mark (get-buffer-process (current-buffer)))))
	(if (and (on-ms-windows) (not *on-windows-nt*))
	    (insert "\n\r")
	  (insert "\n"))
	(setq fi::last-input-end
	  (if (and (on-ms-windows) (not *on-windows-nt*))
	      (1- (point))
	    (point))))
    (let ((max (point)))
      (beginning-of-line)
      (re-search-forward fi::prompt-pattern max t))
    (let ((copy (buffer-substring (point)
				  (progn (forward-line 1) (point)))))
      (goto-char (point-max))
      (setq fi::last-input-start (point))
      (insert copy)
      (setq fi::last-input-end (point))))
  (fi::subprocess-watch-for-special-commands)
  (let ((process (get-buffer-process (current-buffer))))
    (process-send-region process fi::last-input-start fi::last-input-end)
    (fi::input-ring-save fi::last-input-start (1- fi::last-input-end))
    (when (and (on-ms-windows) (not *on-windows-nt*))
      (delete-char -1))
    (set-marker (process-mark process) (point))))

;;;;
;;;;

(defun fi::to-process-group-p ()
  (memq major-mode '(fi:rlogin-mode fi:shell-mode fi:telnet-mode)))

(defun fi::tcp-simulate-special-char (function)
  (let* ((proc (get-buffer-process (current-buffer)))
	 (item (or (assq proc fi::tcp-listener-table) '(nil . 1))))
    (if item
	(fi:eval-in-lisp-asynchronous
	 (format "(lep::tcp-simulate-special-char #'%s %d)\n"
		 function (cdr item)))
      (error "can't find generation number for %s" (process-name proc)))))

(defun fi:subprocess-interrupt ()
  "Send a kill (SIGINT) signal to the current subprocess."
  (interactive)
  (interrupt-process nil (fi::to-process-group-p)))

(defun fi:rlogin-send-interrupt ()
  "Send an interrupt (SIGINT) to the process running as in the current
subprocess buffer."
  (interactive)
  (process-send-string (get-buffer-process (current-buffer)) "\C-c"))

(defun fi:interrupt-listener (select)
  "Interrupt a Lisp listener (send a SIGINT).  Without a prefix argument,
interrupt the Lisp process tied to the buffer in which this function is
invoked.  With a prefix argument, the listener in which this is invoked
asks which Lisp process to interrupt and does a lisp:break on that process."
  (interactive (list current-prefix-arg))
  (cond (select
	 (let ((process (get-buffer-process (current-buffer))))
	   ;; we use INTERRUPT-PROCESS instead of FI::INTERRUPT-SELECT
	   ;; because the latter depends on multiprocessing being on and
	   ;; working--if the lisp is in a wedged state, then the former
	   ;; has a better chance of getting through.
	   (if (eq 'run (process-status process))
	       (interrupt-process nil (fi::to-process-group-p))
	     (fi::tcp-simulate-special-char "lep::interrupt-select"))))
	(t (fi::tcp-simulate-special-char "lep::interrupt-tcp-listener"))))

;;(defun fi:tcp-lisp-listener-interrupt-process ()
;;  "Simulate sending a SIGINT to the Lisp Listener pseudo-process,
;;meaning interrupt the Common Lisp thread associated with the Lisp Listener
;;buffer.  It is not a real process because it is a network connection to the
;;Common Lisp UNIX process, and since there is no tty control there is no
;;character which is interpreted as `interrupt'.  The fake `interrupt' is
;;simulated by doing a mp:process-interrupt on the Common Lisp process tied
;;to the Lisp Listener buffer via the backdoor."
;;  (interactive)
;;  (fi::tcp-simulate-special-char "lep::interrupt-tcp-listener"))

(defun fi:subprocess-send-eof ()
  "Send an EOF (end of file) to the current subprocess."
  (interactive)
  (process-send-eof))

(defun fi:remote-lisp-send-eof ()
  "Simulate sending an EOF to a Lisp subprocess that was started on a
remote machine (with respect to the machine on which emacs is running).
The remote process was most likely started with `rsh', and sending an EOF
to a remote process started in this way closes down the pipe.  The fake EOF
is done by doing a debugger:debug-pop on the \"Initial Lisp Listener\"
process via the backdoor."
  (interactive)
  (fi:eval-in-lisp
   ;; we know we can use "Initial Lisp Listener" because this function will
   ;; only be invoked in the subprocess buffer were the lisp was started
   "(debugger:debug-pop
      (mp::process-name-to-process \"Initial Lisp Listener\"))\n"))

(defun fi:tcp-lisp-listener-send-eof ()
  "Simulate sending an EOF on the Lisp Listener pseudo-process.  It is not
a real process because it is a network connection to the Common Lisp
UNIX process, and EOF has no meaning (out-of-band data is not handled in
either Emacs or Common Lisp, at this time).  The fake EOF is simulated by
doing a debugger:debug-pop on the Common Lisp process tied to the Lisp
Listener buffer via the backdoor."
  (interactive)
  (fi::tcp-simulate-special-char "debugger:debug-pop"))

(defun fi:rlogin-send-eof ()
  "Send an EOF to the process running as in the remote shell in the current
subprocess buffer."
  (interactive)
  (process-send-string (get-buffer-process (current-buffer)) "\C-d"))

(defun fi:subprocess-kill ()
  "Send a kill (SIGKILL) signal to the current subprocess."
  (interactive)
  (kill-process nil (fi::to-process-group-p)))

(defun fi:tcp-lisp-listener-kill-process ()
  "Simulate sending a SIGQUIT to the Lisp Listener pseudo-process,
meaning kill the Common Lisp thread associated with the Lisp Listener
buffer.  It is not a real process because it is a network connection to the
Common Lisp UNIX process, and since there is no tty control there is no
character which is interpreted as `quit'.  The fake `quit' is simulated by
doing a mp:process-kill on the Common Lisp process tied to the Lisp
Listener buffer via the backdoor."
  (interactive)
  (fi::tcp-simulate-special-char "mp:process-kill"))

(defun fi:subprocess-quit ()
  "Send a quit (SIGQUIT) signal to the current subprocess."
  (interactive)
  (quit-process nil (fi::to-process-group-p)))

(defun fi:rlogin-send-quit ()
  "Send a quit (SIGQUIT) to the process running as in the current
subprocess buffer."
  (interactive)
  (process-send-string (get-buffer-process (current-buffer)) "\C-\\"))

(defun fi:subprocess-suspend ()
  "Suspend the current subprocess."
  (interactive)
  (stop-process nil t))

(defun fi:rlogin-send-stop ()
  "Send a stop (SIGSTOP) signal to the process running as in the current
subprocess buffer."
  (interactive)
  (process-send-string (get-buffer-process (current-buffer)) "\C-z"))

(defun fi:subprocess-kill-output ()
  "Kill all output from the subprocess since the last input.  This is a
convenient way to delete the output from the last command."
  (interactive)
  (goto-char (point-max))
  (kill-region fi::last-input-end (point))
  (insert "[output flushed]\n")
  (set-marker (process-mark (get-buffer-process (current-buffer))) (point)))

(defun fi:subprocess-send-flush ()
  "Send the `flush output' character (^O) to current subprocess."
  (interactive)
  (process-send-string (get-buffer-process (current-buffer)) "\C-o"))

(defun fi:subprocess-show-output ()
  "Display the start of this batch of shell output at top of window.
Also move the point there.  Repeated executions toggle whether the
previous input is also displayed."
  (interactive)
  (set-window-start (selected-window)
		    (if (eq (window-start) fi::last-input-end)
			(save-excursion (goto-char fi::last-input-start)
					(beginning-of-line)
					(point))
		      fi::last-input-end))
  (goto-char fi::last-input-end))

(defun fi:subprocess-kill-input ()
  "Kill all input since the last output by the current subprocess."
  (interactive)
  (kill-region (process-mark (get-buffer-process (current-buffer)))
	       (point)))

(defun fi:lisp-sync-current-working-directory ()
  "Sychronize the current working directory in Emacs and Lisp, by making
Lisp conform to the value of default-directory."
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (format "(excl::chdir \"%s\")\n" default-directory)))

(defun fi:shell-sync-current-working-directory ()
  "Sychronize the current working directory in Emacs and the shell, by
making the shell conform to the value of default-directory."
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (format "cd %s\n" default-directory)))

(defun fi:log-functional-change ()
  "Indicate that a function has changed by putting in a descriptive message
at the head of the function."
  (interactive)
  (let* ((case-fold-search t)
	 (bof
	  (cond ((string-match "lisp" mode-name)
		 'fi:beginning-of-defun)
		((string= mode-name "C")
		 '(lambda () (re-search-backward "^{" nil t)))
		(t (error "can't handle this mode: %s" mode-name)))))
    (if (not (funcall bof)) (error "no function to annotate")))
  (end-of-line)
  (newline)
  (funcall indent-line-function)
  (insert-string (concat (format "%s- " (fi::log-comment-start))
			 (current-time-string) " by "
			 (user-login-name) " - "))
  (save-excursion
    (forward-line)
    (beginning-of-line)
    (insert (format "%s\n" comment-end))
    (forward-line -1)
    (funcall indent-line-function)))

(defun fi::log-comment-start ()
  (let ((comment-prefix ";;"))
    (if (and (boundp 'fi:lisp-comment-indent-specification)
	     fi:lisp-comment-indent-specification)
	(let ((list fi:lisp-comment-indent-specification)
	      (done nil)
	    
	      (res ""))
	  (while (and list (not done))
	    (if (eq t (car list)) (setq done t))
	    (setq res (concat res comment-prefix))
	    (setq list (cdr list)))
	  res)
      comment-prefix)))

(defun fi:beginning-of-defun (&optional arg)
  "Move the point to the start of the current top-level form.
With argument ARG, do it that many times.  For when called from a program,
return t, unless beginning of the buffer is reached before finding the
target."
  (interactive "p")
  (or (looking-at "^\\s(")
      (if fi:subprocess-mode
	  (goto-char (process-mark (get-buffer-process (current-buffer))))
	(progn
	  (and arg (< arg 0) (forward-char 1))
	  (and (re-search-backward "^\\s(" nil 'move (or arg 1))
	       (progn (beginning-of-line) t))))))

(defun fi:end-of-defun ()
  "Put the point at the end of the current top-level form."
  (interactive)
  (if fi:subprocess-mode
      (goto-char (point-max))
    (when (fi:beginning-of-defun 1)
      (forward-sexp 1))))

(defun fi:super-paren ()
  "Insert a sufficient number of parenthesis to complete the enclosing
form.  If there are too many parens delete them.  The form is also indented."
  (interactive)
  (save-restriction
    (narrow-to-region (point)
		      (save-excursion (fi:beginning-of-defun) (point)))
    (let ((res (parse-partial-sexp (point-min) (point))))
      (when (nth 3 res) (error "Inside a string!"))
      (when (nth 4 res) (error "Inside a comment!"))
      (when (nth 5 res) (error "After a quote!")))
    (let (p)
      (while (progn (setq p (point))
		    (fi:beginning-of-defun)
		    (condition-case nil (progn (forward-sexp 1) nil)
		      (error t)))
	(goto-char p)
	(insert ")"))
      (unless (eq p (point))
	(error "Extra text after completed form.")
;;;bug14287: don't delete the extra stuff... that's very unfriendly.
;;;	(delete-region p (point))
	)))
  (fi:beginning-of-defun)
  (if fi:lisp-do-indentation
      (fi:indent-sexp)
    (indent-sexp))
  (forward-sexp 1))

(defun fi:find-unbalanced-parenthesis ()
  "Verify that parentheses in the current Lisp source buffer are balanced.
If they are not, position the point at the first syntax error found."
  (interactive)
  (let ((saved-point (point))
	(lpar (string-to-char "("))
	(rpar (string-to-char ")"))
	(comment-start-char (string-to-char comment-start))
	(cond-symbol-chars "-+:a-zA-Z.0-9"))
    (goto-char (point-min))
    (while (and (not (eobp))
		(let ((done nil)
		      (char nil))
		  (while (and (not (eobp)) (not done))
		    (skip-chars-forward "\f\n\t ")
		    (setq char (char-after (point)))
		    (cond ((eq ?\\ char)
			   (forward-char 2))
			  ((eq comment-start-char char)
			   (end-of-line))
			  ((eq ?\" char)
			   (forward-sexp 1))
			  ((eq ?# char)
			   (forward-char 1)
			   (setq char (char-after (point)))
			   (cond ((eq ?| char) (fi::gobble-comment))
				 ((or (eq ?+ char) (eq ?- char))
				  (skip-chars-forward cond-symbol-chars))
				 (t (forward-sexp 1))))
			  (t (setq done t))))
		  t))
      (let ((char (char-after (point))))
	(cond ((or (eq char lpar)
		   (eq (char-after (1- (point))) ?\n))
	       (condition-case ()
		   (forward-sexp 1)
		 (error (error "Missing )"))))
	      ((eq char rpar)
	       (error "Extra )"))
	      (t
	       ;; don't know what it is, but hey, try and forward over it
	       (forward-sexp 1)))))
    (goto-char saved-point))
  (if (interactive-p) (message "All parentheses appear to be balanced."))
  t)

(defun fi::gobble-comment ()
  ;; called when two chars before point are # and |
  (while (and (progn
		(when (and (eq ?# (char-after (point)))
			   (eq ?| (char-after (1+ (point)))))
		  (forward-char 2)
		  (fi::gobble-comment))
		t)
	      (not
	       (and (eq ?| (char-after (point)))
		    (eq ?# (char-after (1+
					(point)))))))
    (forward-char 1))
  (forward-char 2))

(defun fi:check-unbalanced-parentheses-when-saving ()
  (if (and fi:check-unbalanced-parentheses-when-saving
	   (memq major-mode '(fi:common-lisp-mode fi:emacs-lisp-mode
			      fi:franz-lisp-mode)))
      (if (eq 'warn fi:check-unbalanced-parentheses-when-saving)
	  (save-excursion
	    (condition-case nil
		(progn (fi:find-unbalanced-parenthesis) nil)
	      (error
	       (message "Warning: parens are not balanced in this buffer.")
	       (ding)
	       (sit-for 2)
	       ;; so the file is written:
	       nil)))
	(condition-case nil
	    (progn (fi:find-unbalanced-parenthesis) nil)
	  (error
	   ;; save file if user types "yes":
	   (not (y-or-n-p "Parens are not balanced.  Save file anyway? ")))))))

(setq write-file-hooks
  (cons 'fi:check-unbalanced-parentheses-when-saving write-file-hooks))

(defun fi:fill-paragraph (arg)
  "Properly fill paragraphs of Lisp comments by inserting the appropriate
semicolons at the beginning of lines.  With prefix argument, ARG, justify
the paragraph as well."
  (interactive "P")
  (save-excursion
    (beginning-of-line 0)
    (if (re-search-forward "\\(^[ \t]*[;]+[ ]+\\)" nil t)
	(let ((fill-prefix (buffer-substring (match-beginning 1)
					     (match-end 1))))
	  (fill-paragraph arg))
      (fill-paragraph arg)))
  ;; We return `t' because we did all the work:
  t)

(defun fi:extract-list (arg)
  "Take the list after the point and remove the surrounding list.  With
argument ARG do it that many times."
  (interactive "p")
  (let ((string (progn
		  (mark-sexp 1)
		  (buffer-substring (point) (fi::mark)))))
    (backward-up-list (or arg 1))
    (mark-sexp 1)
    (delete-region (point) (fi::mark))
    (insert string)
    (backward-sexp 1)))

(defun fi:comment-region (&optional start end uncomment)
  "Comment all lines in the current region.  With prefix arg, uncomment the
region (it should have been commented with this function).
When calling from a program, arguments are START and END, both buffer
positions, and UNCOMMENT."
  (interactive "r\nP")
  (when (null start) (setq start (point)))
  (when (null end) (setq end (mark)))
  (save-excursion
    (if (< end start)
	(let ((x end)) (setq end start start x)))
    (let ((start (progn (goto-char (max start (point-min)))
			(skip-chars-forward " \t\n") ;skip blank lines
			(beginning-of-line)
			(point)))
	  (end (progn (goto-char (min end (point-max)))
		      (if (or (bolp)
			      (looking-at "[ \t]*$"))
			  (skip-chars-backward " \t\n")) ;skip blank lines
		      (end-of-line)
		      (point)))
	  (comment-prefix ";;;"))
      (goto-char end)
      (if (not uncomment)
	  ;;Comment Region
	  (if (string-equal comment-end "")
	      ;;When no comment-end exists, put a comment-prefix
	      ;;at the start of each line.
	      (while (and (>= (point) start)
			  (progn (beginning-of-line)
				 (insert-string comment-prefix)
				 (= (forward-line -1) 0))))
	    ;;When comment-end exists, put comment marks only at the
	    ;;beginning and end of the region, and put a comment-prefix after
	    ;;each comment-end in the region.
	    (insert-string comment-end)
	    (goto-char end)
	    (while (search-backward comment-end start 'move)
	      (replace-match (concat comment-end comment-prefix))
	      (goto-char (match-beginning 0)))
	    (insert-string comment-prefix))
	;;Uncomment Region
	(if (string-equal comment-end "")
	    (while (and (>= (point) start)
			(progn (beginning-of-line)
			       (if (looking-at (regexp-quote comment-prefix))
				   (replace-match ""))
			       (= (forward-line -1) 0))))
	  (backward-char (length comment-end))
	  (if (looking-at (regexp-quote comment-end))
	      (replace-match ""))
	  (while (re-search-backward (regexp-quote
				      (concat comment-end comment-prefix))
				     start 'move)
	    (replace-match comment-end))
	  (if (looking-at (regexp-quote comment-prefix))
	      (replace-match "")))))))

(defun fi:uncomment-region (start end)
  "Uncomment all lines in the current region (it should have been commented
with the function fi:comment-region).  When calling from a program,
arguments are START and END."
  (interactive "r\n")
  (fi:comment-region start end t))

(defun fi:lisp-delete-pop-up-window ()
  "Make the *CL-temp* buffer disappear and restore the window configuration
as it was before it was made visible."
  (interactive)
  (fi::emacs-lisp-delete-pop-up-window))

(defun fi:lisp-push-window-configuration ()
  (fi::emacs-lisp-push-window-configuration))

(defvar fi::wc-stack)
(setq fi::wc-stack nil)

(defconst fi::wc-stack-max 15)

(defun fi::emacs-lisp-push-window-configuration ()
  (let ((res (list (current-window-configuration) 'place-holder)))
    (setq fi::wc-stack (cons res fi::wc-stack))
    (if (> (length fi::wc-stack) fi::wc-stack-max)
	(setcdr (nthcdr (1- fi::wc-stack-max) fi::wc-stack) nil))
    res))

(defun fi::emacs-lisp-delete-pop-up-window ()
  (unless fi::wc-stack
    (error "The pop-up window stack is empty."))
  (let ((conf (car fi::wc-stack)))
    (setq fi::wc-stack (cdr fi::wc-stack))
    (set-window-configuration (first conf))
    (when (bufferp (second conf))
      (bury-buffer (second conf)))))

;;;(defun fi::epoch-lisp-push-window-configuration ()
;;;  (let* ((id (epoch::get-screen-id (current-screen)))
;;;	 (s (assq id fi::wc-stack)))
;;;    (if s
;;;	(let ((stack (cdr s)))
;;;	  ;; the CAR of AS is ID, the CDR the stack of wc's
;;;	  (rplacd s (cons (current-window-configuration)
;;;			   stack))
;;;	  (if (> (length stack) fi::wc-stack-max)
;;;	      (setcdr (nthcdr (1- fi::wc-stack-max) stack) nil)))
;;;      (setq fi::wc-stack
;;;	(cons (list id (current-window-configuration))
;;;	      fi::wc-stack))))
;;;  nil)

;;;(defun fi::epoch-lisp-delete-pop-up-window ()
;;;  ;; this is the same as the above, except that it keeps the stack for each
;;;  ;; screen separate.
;;;  (let* ((id (epoch::get-screen-id (current-screen)))
;;;	 (s (assq id fi::wc-stack))
;;;	 (stack (cdr s)))
;;;    (unless stack
;;;      (error "The pop-up window stack for this screen is empty."))
;;;    (let ((conf (car stack)))
;;;      (rplacd s (cdr stack))
;;;      (set-window-configuration conf))))

(defvar fi::find-tag-lock-state nil)

(defun fi::disabled-once-find-tag (&rest args)
  (interactive)
  (let ((wc (current-window-configuration)))
    (delete-other-windows)
    (switch-to-buffer "*find-tag lock*")
    (erase-buffer)
    (insert (format "You have invoked ``%s''.\n" this-command))
    (insert "
The standard keybinding for ``M-.'' invokes the Emacs tags file
facility.  In all `fi' Lisp buffer modes there is an alternate
mechanism for visiting the source for a definition based on source-file
information obtained from the Lisp environment itself: ``C-c .''
is bound to fi:lisp-find-definition and ``C-c ,'' is bound to
fi:lisp-find-next-definition.  By default, ``M-.'' and ``M-,'' are
unchanged from default Emacs and are bound to find-tag and
tags-loop-continue.

If you want ``M-.'' to find Lisp definitions in Lisp-moded buffers, then
``C-c .'' and ``M-.'' can be switched in Lisp-moded buffers.  Put the
following form in your ~/.emacs file:

	(setq fi:lisp-mode-hook
	  (function
	   (lambda ()
	     (let ((map (current-local-map)))
	       (define-key map \"\\C-c.\"	'find-tag)
	       (define-key map \"\\C-c,\"	'tags-loop-continue)
	       (define-key map \"\\e.\"	'fi:lisp-find-definition)
	       (define-key map \"\\e,\"	'fi:lisp-find-next-definition)))))

To prevent this message to appear when find-tag or find-tag-other-window
are invoked, put this form in your ~/.emacs before the LOAD of
\"fi-site-init\":

	(setq fi:find-tag-lock nil)

Type `q' to proceed.
")
    (message "Type `q' to proceed.")
    (setq fi::find-tag-lock-state (cons wc this-command))
    (use-local-map (make-keymap))
    (define-key (current-local-map) "q"
		'fi::disabled-once-find-tag-continue)
    (setq buffer-read-only t)))

(defun fi::disabled-once-find-tag-continue ()
  (interactive)
  (set-window-configuration (car fi::find-tag-lock-state))
  (fset 'find-tag (symbol-function 'saved-find-tag))
  (fset 'find-tag-other-window (symbol-function 'saved-find-tag-other-window))
  (call-interactively (cdr fi::find-tag-lock-state)))

(when fi:find-tag-lock
  (fset 'saved-find-tag (symbol-function 'find-tag))
  (fset 'saved-find-tag-other-window (symbol-function 'find-tag-other-window))
  (fset 'find-tag (symbol-function 'fi::disabled-once-find-tag))
  (fset 'find-tag-other-window (symbol-function 'fi::disabled-once-find-tag)))

(defun fi:center-defun ()
  "Put the first line of the current definition on the first line of the
window, leaving the point unchanged."
  (interactive)
  (let ((p (point)))
    (fi:beginning-of-defun)
    (recenter 0)
    (goto-char p)))

(defvar fi::toggle-to-lisp-last-lisp-buffer nil)
(defvar fi::toggle-to-lisp-common-lisp-buffer-name nil)

(defun fi:toggle-to-lisp ()
  "On each invocation, switch back and forth between the Lisp subprocess
buffer and the source buffer from which this function was invoked."
  (interactive)
  (when (and fi::toggle-to-lisp-common-lisp-buffer-name
	     (null (get-buffer fi::toggle-to-lisp-common-lisp-buffer-name)))
    (setq fi::toggle-to-lisp-common-lisp-buffer-name))
  (when (null fi::toggle-to-lisp-common-lisp-buffer-name)
    (or (and fi::common-lisp-backdoor-main-process-name
	     (setq fi::toggle-to-lisp-common-lisp-buffer-name
	       (buffer-name
		(process-buffer
		 (get-process fi::common-lisp-backdoor-main-process-name)))))
	(error "Common Lisp process is not running.")))
  (let (target-buffer)
    (cond ((memq major-mode '(fi:inferior-common-lisp-mode
			      fi:lisp-listener-mode))
	   (or (setq target-buffer fi::toggle-to-lisp-last-lisp-buffer)
	       (error "There is no previous source buffer.")))
	  (t ;; a lisp source buffer
	   (setq fi::toggle-to-lisp-last-lisp-buffer (current-buffer))
	   (setq target-buffer fi::toggle-to-lisp-common-lisp-buffer-name)))
    (funcall fi:display-buffer-function target-buffer)))

(defun fi:next-top-level-form ()
  "Move the point to the beginning of the next top-level form in the buffer."
  (interactive)
  (let ((foo) (lpar (string-to-char "(")))
    (re-search-forward
     "^(" nil nil
     (if (and (= lpar (char-after (point)))
	      (setq foo (char-after (- (point) 1)))
	      (= ?\C-j foo))
	 2 1))
    (forward-char -1)))

(defun fi:previous-top-level-form ()
  "Move the point to the beginning of the previous or current top-level
form in the buffer.  If already at the beginning of the current one, go to
the previous one."
  (interactive)
  (re-search-backward "^("))

(defvar fi:auto-arglist-pop-up-style '(minibuffer)
  "*The value of this variable is used to bind
fi:pop-up-temp-window-behavior when (\\[fi:arglist-lisp-space]) is executed.
A value of '(split . nil) is handy for insuring that the arglist
information stays around long enough to be used.")

(defun fi:arglist-lisp-space ()
  "Display the value of the argument list of a symbol followed by
SPC.  This function is intended to be bound to the SPC key so
that, after being enabled it will display the arglist or value of a
specific symbol after the symbol has been typed in followed by SPC.
See the variable `fi:auto-arglist-pop-up-style'."
  (interactive)
  (if (fi::lep-open-connection-p)
      (fi::arglist-lisp-space-1)
    (self-insert-command (prefix-numeric-value current-prefix-arg))))

;; fi::arglist-lisp-space-1 moved to fi-lep.el due to macros needed at
;; compile time.


(defun fi:insert-arglist ()
  "Insert the arglist for the symbol in the function position in the
current form."
  (interactive)
  (let* ((start (if (get-buffer-process (current-buffer))
		    (marker-position
		     (process-mark (get-buffer-process (current-buffer))))
		  (save-excursion
		    (fi:beginning-of-defun 1)
		    (point))))
	 (end (point))
	 (symbol (fi::symbol-in-function-position start end))
	 arglist
	 position-of-next)
    
    (when (and symbol
	       (setq arglist
		 (fi:eval-in-lisp
		  (format "(common-lisp:let ((common-lisp:*print-length* common-lisp:nil)) (common-lisp:princ-to-string (excl:arglist '%s)))" symbol)))
	       (setq arglist
		 (fi::prep-arglist-for-insertion
		  arglist
		  ;; Is this a macro arglist?
		  (fi:eval-in-lisp
		   (format "(common-lisp:not (common-lisp:null (common-lisp:macro-function '%s)))" symbol))))
	       (setq position-of-next (fi::next-argument-number start end)))
      (setq arglist (nthcdr (1- position-of-next) arglist))
      (save-excursion
	(while arglist
	  (let ((item (car arglist)))
	    (cond
	     ((symbolp item) (insert (symbol-name item)))
	     ((consp item) (fi::insert-list item))
	     (t (insert item))))
	  (when (cdr arglist) (insert " "))
	  (setq arglist (cdr arglist)))
	(insert ")")))))

(defun fi::symbol-in-function-position (start end)
  (save-excursion
    (condition-case nil
	(progn
	  (up-list -1)
	  (forward-char 1)
	  (fi::defontify-string (buffer-substring
				 (point)
				 (progn (forward-sexp 1) (point)))))
      (error nil))))

(defun fi::prep-arglist-for-insertion (string is-macro)
  (let ((arglist (condition-case nil
		     (car (read-from-string string))
		   (error nil))))
    (when arglist
      (fi::prep-arglist-for-insertion-1 arglist is-macro))))

(defun fi::prep-arglist-for-insertion-1 (arglist is-macro)
  ;; Transformations:
  ;;   (foo &rest args) into (foo "args...")
  ;;   (foo &key a b c) into (foo :a a :b b :c c)
  (let ((result '()) keyword-mode optional-mode)
    (while arglist
      (cond ((eq '&aux (car arglist))
	     ;; We're done.
	     (setq arglist nil))
	    ((eq '&allow-other-keys (car arglist)) ;; ignore
	     )
	    ((eq '&optional (car arglist))
	     (setq optional-mode t))
	    ((eq '&body (car arglist))
	     (push (format "%s..." (cadr arglist)) result)
	     (setq arglist (cdr arglist)))
	    ((eq '&rest (car arglist))
	     (when (not (eq '&key (caddr arglist)))
	       ;; No &key after the &rest
	       (push (format "%s..." (cadr arglist)) result))
	     (setq arglist (cdr arglist)))
	    ((eq '&key (car arglist))
	     (setq keyword-mode t)
	     (setq optional-mode nil))
	    (t
	     (let* ((keyword-prefix ":")
		    (ignore-this nil)
		    (s
		     (cond
		      ((and keyword-mode (consp (car arglist)))
		       (cond
			((consp (caar arglist))
			 ;; ((keyword-name var) init-form)
			 (setq keyword-prefix "'")
			 (caaar arglist))
			(t
			 ;; (var init-form)
			 (caar arglist))))
		      ((and is-macro (consp (car arglist)))
		       (setq ignore-this t)
		       (push (fi::prep-arglist-for-insertion-1
			      (car arglist)
			      ;; Process as non-macro:
			      nil)
			     result))
		      (t (car arglist)))))
	       (cond (ignore-this)
		     (optional-mode (push (format "[%s]" s) result))
		     (keyword-mode (push (format "%s%s" keyword-prefix s)
					 result)
				   (push s result))
		     (t (push s result))))))
      (setq arglist (cdr arglist)))
    (reverse result)))

(defun fi::next-argument-number (limit point)
  ;; If the limit and point are here:
  ;;    (foo 1 2 3 4
  ;;    |limit       |point
  ;; then the return value would `5'.
  (save-excursion
    (let ((n 0) (no-error t))
      (while (and no-error (> (point) limit))
	(condition-case nil
	    (progn
	      (forward-sexp -1)
	      (setq n (+ n 1)))
	  (error (setq no-error nil)))) 
      n)))

(defun fi::insert-list (list)
  (insert "(")
  (let (item)
    (while list
      (setq item (car list))
      (cond ((symbolp item) (insert (symbol-name item)))
	    ((or (stringp item) (numberp item)) (insert item))
	    ((consp item) (fi::insert-list item))
	    (t (error "fi::insert-list: can't handle %s" item)))
      (when (cdr list) (insert " "))
      (setq list (cdr list))))
  (insert ")"))
