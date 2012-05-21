;; Copyright (c) 1987-2002 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, and to distribute modified
;; versions, provided that this complete copyright and permission notice is
;; maintained, intact, in all copies and supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

;;; Misc utilities

(defun fi::lisp-find-char (char string &optional from-end)
  (let* ((l (length string))
	 (i (if from-end (1- l) 0))
	 (e (if from-end -1 l))
	 (d (if from-end -1 1))
	 (n nil))
    (while (and (not n) (not (= i e)))
      (if (= char (elt string i))
	  (setq n i)
	(setq i (+ i d))))
    n))

(defconst fi::space (string-to-char " "))

(defun fi::listify-string (string)
  "Take a string \"a b c\" and turn it into a list of \"a\" \"b\" and
\"c\".  nil is represented by the null string."
  (let* ((res nil)
	 (i 0)
	 (len (length string))
	 (len-1 (- len 1))
	 s
	 c
	 x)
    (while (< i len)
      (setq x i)
      (setq s "")
      (while (and (< x len) (/= fi::space (aref string x)))
	(if (and (= ?\\ (setq c (aref string x)))
		 (< x len-1))
	    (progn (setq x (+ x 1))
		   (setq c (aref string x))))
	(setq s (concat s (char-to-string c)))
	(setq x (+ x 1)))
      (setq res (cons s res))
      (setq i (+ x 1)))
    (nreverse res)))

(defun fi::symbol-value-in-buffer (symbol buffer)
  "Return the value of the local binding of SYMBOL in BUFFER, or
nil if non-exists.  Yes, a value of nil and no local value are the same."
  (save-excursion
    ;; the `set-buffer' non-sense is because there is a cache which is only
    ;; updated when a `set-buffer' is done.
    (set-buffer buffer)
    (cdr (assoc symbol (buffer-local-variables buffer)))))

(defun fi::set-in-buffer (symbol value buffer)
  "Set the value of the local binding of SYMBOL to VALUE in BUFFER, or
nil if non-exists.  Yes, a value of nil and no local value are the same."
  (save-excursion
    ;; the `set-buffer' non-sense is because there is a cache which is only
    ;; updated when a `set-buffer' is done.
    (set-buffer buffer)
    (make-local-variable symbol)
    (set symbol value)))

(defun fi::file-name-sans-type (name)
  "Return FILENAME sans file extension or type."
  (substring name 0
 	     (or (string-match "\\.cl$" name)
 		 (string-match "\\.lisp$" name)
 		 (string-match "\\.l$" name)
 		 (length name))))

(defun fi::substitute-chars-in-string (char-assoc-list string)
  "Substitute character pairs of CHAR-ASSOC-LIST in STRING."
  (let (pair)
    (mapconcat '(lambda (char)
		 (if (setq pair (assq char char-assoc-list))
		     (if (null (cdr pair))
			 nil
		       (char-to-string (cdr pair)))
		   (char-to-string char)))
	       string
	       nil)))

(defun fi::remove-chars-from-string (char-list string)
  "Remove characters in CHAR-LIST from string STRING and return the result."
  (mapconcat '(lambda (char)
	       (if (memq char char-list)
		   nil
		 (char-to-string char)))
	     string
	     nil))

(defun fi::find-other-end-of-list (&optional arg)
  (if (null arg) (setq arg 1))
  (save-excursion
    (cond ((= (preceding-char) ?\)) (scan-sexps (point) (- arg)))
	  ((= (following-char) ?\() (scan-sexps (point) arg))
	  ((= (following-char) ?\))
	   (forward-char 1) (scan-sexps (point) (- arg)))
	  (t (error "not on the beginning or end of a list")))))

(defun fi::read-password ()
  (cond
   ((fboundp 'read-passwd) (read-passwd "Password: " nil ""))
   (t (let ((echo-keystrokes 0)
	    (result "")
	    (xxx nil))
	(while (not (or (= (setq xxx (read-char)) ?\^m)
			(= xxx ?\n)))
	  (setq result (concat result (char-to-string xxx))))
	result))))

(defun fi::find-path (path file)
  "Using PATH, find FILE, return the full pathname."
  (let ((done nil) res temp)
    (while (and (not done) path)
      ;; accommodate nil's in the exec-path (bug3159)
      (setq temp (car path))
      (if (null temp)
	  (setq temp default-directory))
      (if (file-exists-p
	   (setq res (concat temp
			     (unless (string-match "/$" temp) "/")
			     file)))
	  (setq done t)
	(setq res nil))
      (setq path (cdr path)))
    res))

(defun fi::command-exists-p (command)
  "Use exec-path to determine whether or not COMMAND exists."
  (if (file-exists-p (format "%s%s" default-directory command))
      (format "%s%s" default-directory command)
    (let ((dirs exec-path) result temp)
      (while (and dirs (null result))
	(if (and (file-exists-p (setq temp (format "%s/%s" (car dirs)
						   command)))
		 (not (file-directory-p temp)))
	    (setq result temp))
	(setq dirs (cdr dirs)))
      result)))

(defvar parse-partial-sexp-result)

(defun fi::fast-parse-partial-sexp (from to
				    &optional targetdepth stopbefore state
					      result)
  "Fast version of fi::parse-partial-sexp which doesn't cons if sixth arg
is given, which should be a list of length seven.  This requires a hacked
version of parse-partial-sexp.  This function is automagically selected
based on whether calling parse-partial-sexp gives an error when called with
six arguments."
  (if result
      (let ((parse-partial-sexp-result result))
	(parse-partial-sexp from to targetdepth stopbefore state))
    (parse-partial-sexp from to targetdepth stopbefore state)))

(defun fi::slow-parse-partial-sexp (from to
				    &optional targetdepth stopbefore state
					      result)
  "Slow version of fi::parse-partial-sexp which conses like mad, no matter
what the optional sixth argument is.  This is used if parse-partial-sexp
hasn't been hacked.  This function is automagically selected based on
whether calling parse-partial-sexp gives an error when called with six
arguments."
  (if result
      (let ((res result)
	    (xx (parse-partial-sexp from to targetdepth stopbefore state)))
	(while res
	  (setcar res (car xx))
	  (setq xx (cdr xx))
	  (setq res (cdr res)))
	result)
    (parse-partial-sexp from to targetdepth stopbefore state)))

(if (boundp 'parse-partial-sexp-result)
    (fset 'fi::parse-partial-sexp
	  (symbol-function 'fi::fast-parse-partial-sexp))
  (fset 'fi::parse-partial-sexp
	(symbol-function 'fi::slow-parse-partial-sexp)))

(defun fi::all-upper-case-p (string)
  (let ((index 0)
	(max+1 (length string))
	(lower-found nil)
	char)
    (while (< index max+1)
      (setq char (aref string index))
      (if (and (>= char ?a) (<= char ?z))
	  (setq lower-found t))
      (setq index (+ index 1)))
    (not lower-found)))

(defun fi::downcase-alist-elt (item)
  ;; use: (mapcar 'fi::downcase-alist-elt xalist)
  (cons (downcase (car item))
	(downcase (cdr item))))

(defun fi::fast-search-string (char string)
  (let ((index 0)
	(max+1 (length string))
	(found nil))
    (while (and (not found) (< index max+1))
      (if (= char (aref string index))
	  (setq found t)
	(setq index (+ index 1))))
    found))

(defun fi:process-running-p (thing &optional buffer-name)
  (let ((running-states '(run stop open)) temp)
    (cond ((processp thing)
	   (memq (process-status thing) running-states))
	  ((stringp thing)
	   (and (setq temp (get-buffer (or buffer-name thing)))
		(setq temp (get-buffer-process temp))
		(memq (process-status temp) running-states)))
	  (t nil))))

(defvar fi:filename-frobber-hook
    'fi::discombobulate-automounter-lint
  "*If non-nil, then name of a function which transforms filenames received
from Lisp.  This exists solely for the purpose of removing /tmp_mnt/net
from the beginning of filenames that are on automounted filesystems.")

(defun fi::discombobulate-automounter-lint (name)
  ;; remove /tmp_mnt from the beginning of NAME
  (if (string-match "^\\(/tmp_mnt\\)?\\(.*\\)$" name)
      (substring name (match-beginning 2) (match-end 2))
    (error "discombobulate-automounter-lint: internal error on %s" name)))

(defun fi::canonicalize-filename (file)
  "If FILE starts with user's home directory, then turn it into a filename
that starts with ~."
  (if (string-match (format "^\\(%s\\)\\(.*\\)" (getenv "HOME"))
		    file)
      (concat "~" (substring file (match-beginning 2) (match-end 2)))
    file))

(defun fi::frob-case-from-lisp (arg)
  (let ((string (if (symbolp arg)
		    (symbol-name arg)
		  arg)))
    (cond ((eq ':upper fi::lisp-case-mode)
	   (downcase string))
	  (t string))))

(defun fi::frob-case-to-lisp (arg)
  (let ((string (if (symbolp arg)
		    (symbol-name arg)
		  (format "%s" arg))))
    (cond ((eq ':upper fi::lisp-case-mode)
	   (upcase string))
	  (t string))))

(defun fi::getf-property (plist property &optional default)
  (while (and plist
	      (not (eq (car plist) property)))
    (setq plist (cddr plist)))
  (if plist 
      (second plist)
    default))

(defun fi::transpose-list (list)
  (let ((l (make-list (length (car list)) nil)))
    (dolist (k list)
      (let ((n 0))
	(dolist (a k)
	  (push a (nth n l))
	  (incf n))))
    l))

(defun fi::insert-file-contents-into-kill-ring (copy-file-name)
  (let ((buffer (generate-new-buffer "*temp*")))
    (save-excursion
      (set-buffer buffer)
      (insert-file-contents copy-file-name)
      (copy-region-as-kill (point-min) (point-max)))
    (kill-buffer buffer)))

(defun fi::member-plist (prop plist)
  (and plist
       (or (eq (car plist) prop)
	   (fi::member-plist prop (cddr plist)))))

(defun fi::string-to-keyword (xpackage)
  (and xpackage (intern (concat ":" xpackage))))

(defun fi::listify (x)
  (and x
       (if (atom x)
	   (list x)
	 x)))

(defun fi::quote-every-other-one (list)
  (and list
       (list* (list 'quote (first list)) (second list)
	      (fi::quote-every-other-one (cddr list)))))

(defun fi:verify-emacs-support ()
  "A function used to test the GNU Emacs in which it is run to see if the
minimum require support for the Emacs-Lisp interface exists.
As of GNU Emacs 18.58, there is no additional support/modifications needed
for the emacs-lisp interface to function properly."
  (interactive)
  (if (interactive-p)
      (message "everything looks fine!")
    t))

(defvar fi::last-network-condition nil)

(defvar fi::muffle-open-network-stream-errors nil)

(defun fi::open-network-stream (name buffer host service)
  (condition-case condition
      (open-network-stream name buffer host service)
    (error
     (setq fi::last-network-condition condition)
     (when (not fi::muffle-open-network-stream-errors)
       (cond
	((and (not (on-ms-windows))
	      (not (file-readable-p "/etc/hosts")))
	 (fi:error "
Can't connect to host %s.  This is probably due to /etc/hosts not being
readable.  The error from open-network-stream was:
  %s"
		   host (car (cdr condition))))
	(t
	 (cond
	  ((and (string-match "xemacs" emacs-version)
		(string= "21.4.17" (symbol-value 'emacs-program-version)))
	   (fi:error "
Can't connect to host %s.  The error from open-network-stream was:
  %s

You are running XEmacs 21.4.17, which has been known to contain a bug
that prevents open-network-stream to signal an error when a numerical
port is passed.  Try this workaround to the bug, by putting the following
form into your .emacs:

   (defadvice open-network-stream (around make-ports-be-strings)
     (when (numberp service)
       (setq service (format \"%%d\" service)))
      ad-do-it)"
		     host (car (cdr condition))))
	  (t
	   (fi:error "
Can't connect to host %s.  The error from open-network-stream was:
  %s"
		     host (car (cdr condition))))))))
     nil)))

(defun fi:note (format-string &rest args)
  (let ((string (apply 'format format-string args)))
    (delete-other-windows)
    (fi::switch-to-buffer "*Help*")
    (when buffer-read-only (toggle-read-only))
    (erase-buffer)
    (insert string)
    (goto-char (point-min))))

(defun fi:error (format-string &rest args)
  (apply 'fi:note format-string args)
  (beep)
  (signal 'error (list "")))

(defun fi:map-lines (function &rest args)
  "Apply FUNCTION to ARGS once for every line in buffer, with point always
at the beginning of the line."
  (goto-char (point-min))
  (let (save)
    (while (not (eobp))
      (setq save (point))
      (apply function args)
      (goto-char save)
      (forward-line 1))))

;; This is a pretty bad hack but it appears that within completing-read
;; fi:package has the wrong value so we bind this variable to get around
;; the problem.
(defvar fi::original-package nil)

(defvar fi::use-symbol-at-point nil
  ;; fi-xemacs.el uses this to cause the fi:common-lisp-mode specific menu
  ;; to be able to grab the thing at the point.
  )

(defvar ignore-keywords nil)

(defun fi::get-default-symbol (prompt &optional up-p ignore-keywords)
  (let ((symbol-at-point (fi::get-symbol-at-point up-p)))
    (if fi::use-symbol-at-point
	(list symbol-at-point)
      (let ((read-symbol
	     (let ((fi::original-package (fi::package)))
	       (fi::ensure-minibuffer-visible)
	       (fi::completing-read
		(if symbol-at-point
		    (format "%s: (default %s) " prompt symbol-at-point)
		  (format "%s: " prompt))
		'fi::minibuffer-complete))))
	(list (if (string= read-symbol "")
		  symbol-at-point
		read-symbol))))))

(defun fi::completing-read (prompt table &optional predicate require-match
						   init hist)
  ;; Just like completing-read, except that we make sure that the binding
  ;; for space is 'minibuffer-complete instead of
  ;; 'minibuffer-complete-and-exit.  The latter binding causes problems
  ;; because it doesn't allow us to insert a space when typing forms--as
  ;; opposed to a symbol.
  (let* ((map minibuffer-local-completion-map)
	 (old-value (lookup-key map " "))
	 res)
    (define-key map " " 'minibuffer-complete)
    (condition-case c
	(setq res
	  (completing-read prompt table predicate require-match init hist))
      (error
       ;; undo temporary binding
       (define-key map " " old-value)
       (error (cdr c)))
      (quit
       ;; undo temporary binding
       (define-key map " " old-value)
       (error "Quit")))
    (define-key map " " old-value)
    res))

(defun fi::minibuffer-complete (pattern predicate what)
  (if (string-match "^[ \t]*(" pattern)
      ;; Don't do completion, since we have a form and we should just
      ;; insert a space.
      (if (= 32 last-command-char) ;; space
	  (concat pattern " ")
	pattern)
    (fi::minibuffer-complete-1 pattern predicate what)))

(defun fi::minibuffer-complete-1 (pattern predicate what)

  ;; HACK HACK HACK HACK
  ;;   ignore-keywords must be bound in the dynamic context in which this
  ;;   function is called (just above in fi::get-default-symbol).
  ;; HACK HACK HACK HACK
  
  (let ((fi:package fi::original-package))
    (let ((xpackage nil)
	  (deletion nil))
      (when (string-match ":?:" pattern)
	(setq xpackage (concat ":" (substring pattern 0 (match-beginning 0))))
	(setq deletion (substring pattern 0 (match-end 0)))
	(setq pattern (substring pattern (match-end 0))))
      (let* ((alist
	      (fi::package-frob-completion-alist
	       (fi::lisp-complete-1 pattern xpackage nil ignore-keywords)))
	     (completion
	      (when alist
		(if xpackage
		    (let ((real-package
			   (reduce 'fi::package-prefix (mapcar #'car alist))))
		      (try-completion (concat real-package pattern)
				      alist))
		  (try-completion pattern alist)))))
	(ecase what
	  ((nil) (cond ((eq completion t) t)
		       ((and (null completion) (null alist))
			;; no match
			nil)
		       ((and (null completion) alist (null (cdr alist)))
			;; one match for abbrev
			(cdr (car alist)))
		       ((and (null completion) alist)
			;; more than one match for abbrev
			(fi::abbrev-to-symbol pattern alist))
		       ((null (string= pattern completion))
			;; we can complete further than pattern
			(let ((new (cdr (assoc completion alist))))
			  (if new new completion)))
		       ((and alist (null (cdr alist)))
			;; one match
			(if (string-match "::" (cdr (car alist)))
			    (cdr (car alist))
			  (concat deletion (cdr (car alist)))))
		       (t
			;; more than one match, just return completion,
			;; with possible package prefix
			(if (and completion (string-match "::" completion))
			    completion
			  (concat deletion completion)))))
	  ((t) (mapcar (function cdr) alist))
	  (lambda (eq completion t)))))))

(defun fi::package-frob-completion-alist (alist)
  (mapcar
   (function
    (lambda (item)
      (cons (fi::make-internal (car item))
	    (cdr item))))
   alist))

(defun fi::make-internal (s)
  (if (string-match "\\([^:]+\\):?:\\([^:]+\\)" s)
      (concat (substring s (match-beginning 1)
			 (match-end 1))
	      "::"
	      (substring s (match-beginning 2)
			 (match-end 2)))
    s))

;; fi::try-completion removed

(defvar fi::*package-prefix* "\\([^:]+\\)\\([:]+\\)")

(defun fi::package-prefix (s1 s2)
  (let ((regexp fi::*package-prefix*)
	p1 p2 c1 c2)
    (and s1
	 s2
	 (progn
	   (when (string-match regexp s1)
	     (setq p1 (substring s1 (match-beginning 1) (match-end 1)))
	     (setq c1 (substring s1 (match-beginning 2) (match-end 2))))
	   (when (string-match regexp s2)
	     (setq p2 (substring s2 (match-beginning 1) (match-end 1)))
	     (setq c2 (substring s2 (match-beginning 2) (match-end 2))))
	   (string= p1 p2))
	 (if (or (= (length c1) 2)
		 (= (length c2) 2))
	     (concat p1 "::")
	   (concat p1 ":")))))

(defun fi::get-symbol-at-point (&optional up-p)
  (let ((symbol
	 (cond
	  ((looking-at "\\sw\\|\\s_")
	   (save-excursion
	     (while (looking-at "\\sw\\|\\s_")
	       (forward-char 1))
	     (fi::defontify-string
		 (buffer-substring
		  (point)
		  (progn (forward-sexp -1)
			 (while (looking-at "\\s'")
			   (forward-char 1))
			 (point))))))
	  (t
	   (condition-case ()
	       (save-excursion
		 (if up-p
		     (let ((opoint (point)))
		       (cond ((= (following-char) ?\()
			      (forward-char 1))
			     ((= (preceding-char) ?\))
			      (forward-char -1)))
		       (up-list -1)
		       (forward-char 1)
		       (if (looking-at "def")
			   (goto-char opoint)
			 (if (looking-at "funcall\\|apply")
			     (progn
			       (forward-sexp 2)
			       (backward-sexp 1)
			       (if (looking-at "#'")
				   (forward-char 2)
				 (if (looking-at "(function")
				     (progn
				       (forward-char 1)
				       (forward-sexp 2)
				       (backward-sexp 1)))))))))
		 (while (looking-at "\\sw\\|\\s_")
		   (forward-char 1))
		 (if (re-search-backward "\\sw\\|\\s_" nil t)
		     (progn (forward-char 1)
			    (fi::defontify-string
				(buffer-substring
				 (point)
				 (progn (forward-sexp -1)
					(while (looking-at "\\s'")
					  (forward-char 1))
					(point)))))
		   nil))
	     (error nil))))))
    (when symbol (setq symbol (fi::normalize-symbol-package symbol)))
    (or symbol
	(if (and up-p (null symbol))
	    (fi::get-symbol-at-point)))))

(defvar fi:hierarchical-packages t
  "*If non-nil, then packages whose name starts with a dot are assumed
hierarchical, i.e. relative to the current package, when doing, for
example, arglist lookup.")

(defun fi::normalize-symbol-package (name)
  "Normalize hierarchical package name"
  (if (string-match fi::*package-prefix* name)
      (fi::normalize-package name)
    name))

(defun fi::normalize-package (name)
  (if (and fi:hierarchical-packages
	   (> (length name) 0)
	   (= (aref name 0) ?.)
	   fi:package)
      (concat fi:package name)
    name))

(defun fi::abbrev-to-symbol (pattern alist)
  (let* ((suffix (and (string-match ".*-\\(.*\\)" pattern)
		      (substring pattern
				 (match-beginning 1)
				 (match-end 1))))
	 (nwords
	  (let ((n 0) (i 0) (max (length pattern)))
	    (while (< i max)
	      (if (= ?- (aref pattern i))
		  (setq n (+ n 1)))
	      (setq i (+ i 1)))
	    n))
	 (n 0)
	 (words nil)
	 abbrev-word
	 expanded-word
	 xx)
    (while (< n nwords)
      (setq abbrev-word (fi::word pattern n))
      (setq xx
	(mapcar (function (lambda (x) (fi::word (car x) n)))
		alist))
      (setq expanded-word (car xx))
      (if (let (done)
	    (while (and (not done) xx)
	      (if (and (cdr xx)
		       (not (string= (car xx) (car (cdr xx)))))
		  (setq done t))
	      (setq xx (cdr xx)))
	    done)
	  ;; words aren't the same
	  (setq words (cons abbrev-word words))
	(setq words (cons expanded-word words)))
      (setq n (+ n 1)))
    
    (if words
	(format "%s-%s" (mapconcat 'identity (nreverse words) "-") suffix)
      pattern)))

(defun fi::word (string word)
  ;; (fi::word "foo-bar-baz" 0) returns "foo"
  ;; (fi::word "foo-bar-baz" 1) returns "bar"
  ;; (fi::word "foo-bar-baz" 2) returns "baz"
  ;; (fi::word "foo-bar-baz" 3) returns nil
  (let* ((n 0)
	 (i 0)
	 (res nil)
	 (max (length string))
	 c
	 done)
    (while (and (not done) (< n word))
      (if (= i max)
	  (setq done t)
	(if (= ?- (aref string i))
	    (setq n (+ n 1)))
	(setq i (+ i 1))))
    
    (while (and (< i max)
		(not (= ?- (setq c (aref string i)))))
      (setq res (concat res (char-to-string c)))
      (setq i (+ i 1)))

    res))

;;;;

(defvar fi:pop-up-temp-window-behavior '(other . t)
  "*The value of this variable determines the behavior of the popup
temporary buffers used to display information which is the result of
queries of the Lisp environment.  As of version 2.4.0 of the interface,
this includes output generated by arbitrary evaluations of user-written
Lisp code, when the value of this variable is '(minibuffer).

The value is a cons of the form (style .  boolean).

The possible values for the `style' (or `car' of the cons) are the symbols
minibuffer, split, other, and replace:

- `minibuffer' causes the minibuffer to always be used, regardless of the
number of lines of output.  Recent versions of Emacs have dynamically
resizing minibuffers, and this is useful in combination with these newer
versions.

- `split' causes the largest window to be split and the new window to be
minimal in size.

- `other' causes the other window to be used, spliting the screen if there is
only one window.

- `replace' causes the current window to be replaced with the help buffer.

The possible values for the `boolean' (or `cdr' of the cons) are `t' or
`nil'.  `t' means use the minibuffer, and if the resulting text does not
fit, use a window.  `nil' means always use a window.  A value of `nil' is
handy since messages printed in the minibuffer can easily be erased.

If the `style' is `minibuffer', then the `boolean' is ignored.")

(defun fi::display-pop-up-window (buffer &optional hook args)
  (fi:lisp-push-window-configuration)
  (cond ((eq 'split (car fi:pop-up-temp-window-behavior))
	 (fi::display-pop-up-window-split buffer hook args))
	((or (eq 'other (car fi:pop-up-temp-window-behavior))
	     ;; If we get here and the car is 'minibuffer, then the output
	     ;; is too large to fit in the minibuffer and we'll assume it
	     ;; to be like `other'.
	     (eq 'minibuffer (car fi:pop-up-temp-window-behavior)))
	 (fi::display-pop-up-window-other buffer hook args))
	((eq 'replace (car fi:pop-up-temp-window-behavior))
	 (fi::display-pop-up-window-replace buffer hook args))
	(t (error "bad value for car of fi:pop-up-temp-window-behavior: %s"
		  (car fi:pop-up-temp-window-behavior))))
  (and (fboundp 'fi::ensure-buffer-visible)
       (fi::ensure-buffer-visible buffer))
  ;; If successive operations pushed identical window configurations,
  ;; remove the redundancy.
  (fi::emacs-lisp-pop-redundant-window-configuration))

(defun fi::emacs-lisp-pop-redundant-window-configuration ()
  (let ((c (caar  fi::wc-stack)))
    (when (and c
	       (fboundp 'compare-window-configurations)
	       (compare-window-configurations c
					      (current-window-configuration)))
      (pop fi::wc-stack))))

(defun fi::display-pop-up-window-replace (buffer hook args)
  (switch-to-buffer buffer)
  (when hook (apply hook args)))

(defun fi::display-pop-up-window-other (buffer hook args)
  (cond
   ((eq (current-buffer) buffer))
   ((one-window-p)
    (split-window)
    (other-window 1)
    (switch-to-buffer buffer))
   (t
    (other-window 1)
    (switch-to-buffer buffer)))

  (when hook (apply hook args))
    
  (bury-buffer buffer)
  (other-window 1))

(defun fi::display-pop-up-window-split (buffer hook args)
  (let* ((from-window (selected-window))
	 (real-from-window nil)
	 (from-window-orig-height (1- (window-height))) ; minus mode line
	 (buffer-window (get-buffer-window buffer))
	 (lines nil))
    
    (save-excursion
      (set-buffer buffer)
      (setq lines (count-lines (point-min) (point-max))))

    ;; get to the proper window
    (cond (buffer-window
	   (when (not (eq (selected-window) buffer-window))
	     (select-window buffer-window)))
	  ((eq (current-buffer) buffer))
	  ((one-window-p)
	   (setq from-window-orig-height (1- (window-height)))
	   (split-window)
	   (save-window-excursion
	     (other-window 1)
	     (setq from-window (selected-window)))
	   (switch-to-buffer buffer))
	  (t
	   (setq real-from-window (selected-window))
	   (select-window (get-largest-window))
	   (if (eq real-from-window (selected-window))
	       (setq real-from-window nil))
	   (setq from-window-orig-height (1- (window-height)))
	   (split-window)
	   (save-window-excursion
	     (other-window 1)
	     (setq from-window (selected-window)))
	   (switch-to-buffer buffer)))

    (unless (one-window-p)
      (let* ((window-min-height 2)
	     (target-size
	      (max window-min-height
		   (min lines (/ from-window-orig-height 2)))))
	(if (< target-size (window-height))
	    (shrink-window (- (window-height) target-size 1))
	  (if (> target-size (window-height))
	      (enlarge-window (- target-size (window-height) -1))))))
    
    (when hook (apply hook args))
    
    (bury-buffer buffer)
    (select-window (or real-from-window from-window))))

(defvar fi::shell-buffer-for-common-lisp-interaction-host-name nil)

(defun fi::setup-shell-buffer-for-common-lisp-interaction (process)
  "Internal function use to start an emacs-lisp interface in a buffer not
created by fi:common-lisp."
  (interactive (list (get-buffer-process (current-buffer))))
  (when (fi::lep-open-connection-p)
    (error "an emacs-lisp interface is already running in this emacs."))
  (save-excursion
    (set-buffer (process-buffer process))
    (unless process
      (error "current buffer doesn't have a process associated with it"))
    (setq fi::common-lisp-backdoor-main-process-name
      (setq fi::process-name (buffer-name (current-buffer))))
    (setq fi::lisp-host
      (or fi::shell-buffer-for-common-lisp-interaction-host-name
	  (setq fi::shell-buffer-for-common-lisp-interaction-host-name
	    (read-string "host on which lisp is running: "))))
    (set-process-filter process 'fi::common-lisp-subprocess-filter)))

(defun fi::explode (string char)
  (let ((res nil)
	(s -1)
	(i 0)
	(max (length string)))
    (while (< i max)
      (if (= char (aref string i))
	  (progn
	    (setq res (cons (substring string
				       (if (= s -1) 0 (1+ s))
				       i)
			    res))
	    (setq s i)))
      (setq i (+ i 1)))
    (unless (= s max)
      (setq res (cons (substring string
				 (if (= s -1) 0 (1+ s))
				 i)
		      res)))
    (nreverse res)))

(defun fi::shell-command-output-to-string (buffer program &rest arguments)
  ;; run COMMAND returning the output as a string
  (save-excursion
    (set-buffer buffer)
    (erase-buffer)
    (apply 'call-process program nil t nil arguments)
    (if (> (buffer-size) 0)
	(progn
	  (goto-char (point-max))
	  (if (= ?\n (preceding-char))
	      (buffer-substring (point-min) (- (point-max) 1))
	    (buffer-string)))
      nil)))

(defun fi:member-equal (item list)
  "Same as common lisp (member item list :test #'equal)."
  (let ((ptr list)
        (done nil)
        (result '()))
    (while (not (or done (atom ptr)))
      (cond ((equal item (car ptr))
             (setq done t)
             (setq result ptr)))
      (setq ptr (cdr ptr)))
    result))

(defun fi::switch-to-buffer (buffer)
  ;; if buffer is in some window, go to it, otherwise switch-to-buffer
  (let ((start (selected-window))
	(current (next-window (selected-window) 'no-minibuffer 'visible))
	(found nil))
    (while (and (not (eq current start))
		(not found))
      (if (eq buffer (window-buffer current))
	  (setq found current))
      (setq current (next-window current 'no-minibuffer 'visible)))
    (if (null found)
	(switch-to-buffer buffer)
      (select-window found))))
    
(defun fi::insert-string (string start end)
  (do ((p start (1+ p)))
      ((eq p end))
    (insert-char (aref string p) 1)))

(defun fi::mark-hack () (mark t))

(defun fi::prin1-to-string-xemacs (form)
  (let ((print-escape-newlines nil))
    (prin1-to-string form)))

(defun fi::prin1-to-string-mule (form) ;; bug8475
  (let ((print-escape-multibyte nil)
	(print-escape-nonascii nil)
	(print-escape-newlines nil))
    ;; For print-escape-nonascii to be used we must use prin1, which means
    ;; we have to use with-output-to-string.
    (with-output-to-string (prin1 form))))

(cond ((or (eq fi::emacs-type 'xemacs19)
	   (eq fi::emacs-type 'xemacs20))
       (fset 'fi::mark 'fi::mark-hack)
       (fset 'fi::prin1-to-string 'fi::prin1-to-string-xemacs))
      ((eq fi::emacs-type 'emacs19)
       (fset 'fi::mark 'fi::mark-hack)
       (fset 'fi::prin1-to-string 'prin1-to-string))
      ((eq fi::emacs-type 'emacs20)
       (fset 'fi::mark 'fi::mark-hack)
       (fset 'fi::prin1-to-string 'fi::prin1-to-string-mule))
      (t
       (fset 'fi::mark 'mark)
       (fset 'fi::prin1-to-string 'prin1-to-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The rest of the code in this file is modified from code in GNU Emacs, so
;; the following copyright applies:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright (C) 1985, 86, 87, 93, 94, 95 Free Software Foundation, Inc.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defun fi::do-auto-fill ()
  ;; This function is just like do-auto-fill, except in these ways:
  ;; 1. no justification is done
  ;; 2. it calls (indent-according-to-mode) at the end to indent the line
  ;; 3. use adaptive-fill-regexp to get the fill-prefix 
  (let ((fill-prefix fill-prefix)
	(fc (current-fill-column))
	(give-up nil))
;;;; fi changes:
    ;; Remove code to conditionally execute the rest of the code in this
    ;; function, as well as justification code.
;;;; ...end fi changes.
    
    ;; Choose a fill-prefix automatically.
    (if (and adaptive-fill-mode
	     (or (null fill-prefix) (string= fill-prefix "")))
	(let ((prefix
;;;; fi changes:
	       (fi::find-fill-prefix-from-current-line)))
;;;; ...end fi changes.
	  (and prefix
	       (not (equal prefix ""))
	       (setq fill-prefix prefix))))

    (while (and (not give-up) (> (current-column) fc))
      ;; Determine where to split the line.
      (let ((fill-point
	     (let ((opoint (point))
		   bounce
		   (first t))
	       (save-excursion
		 (move-to-column (1+ fc))
		 ;; Move back to a word boundary.
		 (while (or first
			    ;; If this is after period and a single space,
			    ;; move back once more--we don't want to break
			    ;; the line there and make it look like a
			    ;; sentence end.
			    (and (not (bobp))
				 (not bounce)
				 sentence-end-double-space
				 (save-excursion
				   (forward-char -1)
				   (and (looking-at "\\. ")
					(not (looking-at "\\.  "))))))
		   (setq first nil)
		   (skip-chars-backward "^ \t\n")
		   ;; If we find nowhere on the line to break it,
		   ;; break after one word.  Set bounce to t
		   ;; so we will not keep going in this while loop.
		   (if (bolp)
		       (progn
			 (re-search-forward "[ \t]" opoint t)
			 (setq bounce t)))
		   (skip-chars-backward " \t"))
		 ;; Let fill-point be set to the place where we end up.
		 (point)))))
	;; If that place is not the beginning of the line,
	;; break the line there.
	(if (save-excursion
	      (goto-char fill-point)
	      (not (bolp)))
	    (let ((prev-column (current-column)))
	      ;; If point is at the fill-point, do not `save-excursion'.
	      ;; Otherwise, if a comment prefix or fill-prefix is inserted,
	      ;; point will end up before it rather than after it.
	      (if (save-excursion
		    (skip-chars-backward " \t")
		    (= (point) fill-point))
		  (indent-new-comment-line t)
		(save-excursion
		  (goto-char fill-point)
		  (indent-new-comment-line t)))
		
;;;; fi changes:
	      ;; Remove justification code.
;;;; ...end fi changes.

	      ;; If making the new line didn't reduce the hpos of
	      ;; the end of the line, then give up now;
	      ;; trying again will not help.
	      (if (>= (current-column) prev-column)
		  (setq give-up t)))
	  ;; No place to break => stop trying.
	  (setq give-up t))))
;;;; fi changes:
    ;; Indent last line.
    (indent-according-to-mode)
;;;; ...end fi changes.
    t))

(defvar fi::auto-fill-hyphen-special t)

(defun fi::find-fill-prefix-from-current-line ()
  (save-excursion
    (let ((bol (progn (beginning-of-line) (point)))
;;;;eol
	  m0-start m0-end)
      (cond
       ((looking-at adaptive-fill-regexp)
	(setq m0-start (match-beginning 0))
	(setq m0-end (match-end 0))
	(if (match-beginning 1)
	    ;; there is only a comment on this line, so determining the
	    ;; fill-prefix is simple, just the entire match:
	    (let ((prefix (buffer-substring m0-start m0-end)))
	      (cond
	       ((null fi::auto-fill-hyphen-special)
		(buffer-substring m0-start m0-end))
	       ((string-match "\\(.*\\)\\( [-0-9]+\\.? \\)" prefix)
		(concat
		 (substring prefix (match-beginning 1) (match-end 1))
		 (make-string (length
			       (substring prefix
					  (match-beginning 2) (match-end 2)))
			      ? )))
	       (t prefix)))
	  ;; There is something other than a comment on this line, so we
	  ;; have to do work to find the real fill-prefix.	  
;;;;(setq eol (progn (end-of-line) (point)))
	  (goto-char bol)
	  (condition-case nil
	      (progn
		(forward-sexp 1)
		(skip-chars-forward " \t")
		(if (not (looking-at ";+ +"))
		    ;; Punt:
		    ""
		  ;; Return a string with the same number of characters at the
		  ;; current position on the line:
		  (concat
		   (make-string (- (point) (save-excursion
					     (beginning-of-line)
					     (point)))
				? )
		   (buffer-substring (match-beginning 0)
				     (match-end 0)))))
	    (error
	     ;; Most likely, the forward-sexp got an error, so punt:
	     ""))))
       (t "")))))

(defvar fi::if*-keyword-list '("then" "thenret" "else" "elseif"))

(defmacro if* (&rest args)
   (do ((xx (reverse args) (cdr xx))
	(state ':init)
	(elseseen nil)
	(totalcol nil)
	(lookat nil nil)
	(col nil))
       ((null xx)
	(cond ((eq state ':compl)
	       `(cond ,@totalcol))
	      (t (error "if*: illegal form %s" args))))
       (cond ((and (symbolp (car xx))
		   (member (symbol-name (car xx)) fi::if*-keyword-list))
	      (setq lookat (symbol-name (car xx)))))

       (cond ((eq state ':init)
	      (cond (lookat (cond ((equal lookat "thenret")
				   (setq col nil
					 state ':then))
				  (t (error
				      "if*: bad keyword %a" lookat))))
		    (t (setq state ':col
			     col nil)
		       (push (car xx) col))))
	     ((eq state ':col)
	      (cond (lookat
		     (cond ((equal lookat "else")
			    (cond (elseseen
				   (error "if*: multiples elses")))
			    (setq elseseen t)
			    (setq state ':init)
			    (push `(t ,@col) totalcol))
			   ((string-equal lookat "then")
			    (setq state ':then))
			   (t (error "if*: bad keyword %s" lookat))))
		    (t (push (car xx) col))))
	     ((eq state ':then)
	      (cond (lookat
		     (error
		      "if*: keyword %s at the wrong place" (car xx)))
		    (t (setq state ':compl)
		       (push `(,(car xx) ,@col) totalcol))))
	     ((eq state ':compl)
	      (cond ((not (string-equal lookat "elseif"))
		     (error "if*: missing elseif clause ")))
	      (setq state ':init)))))

;;;(defun fi::breakpoint (&rest foo)
;;;  foo)
;;;
;;;(cancel-debug-on-entry 'fi::breakpoint)
;;;(debug-on-entry 'fi::breakpoint)

(defun fi::file-contents (filename)
  (let* ((buffer (get-buffer-create "  *file-contents-tmp*"))
	 (s 
	  (save-excursion
	    (set-buffer buffer)
	    (erase-buffer)
	    (insert-file-contents filename)
	    (buffer-string))))
    (kill-buffer buffer)
    s))

(defun fi:start-interface-via-file (host buffer connection-file)
  (interactive "sHost: \nBBuffer name: \nfConnection data file: ")
  ;; Can't bind this, or background errors will not be handled
  ;; correctly.
  (setq fi::started-via-file t)
  (fi::start-interface-via-file-1 host buffer connection-file))

(defun fi::start-interface-via-file-1 (host buffer connection-file)
  ;; On the lisp side, you would do something like this:
  ;;  (sys::start-emacs-lisp-interface t 1 7666 "~/.eli-startup")
  ;; then, sometime later on the emacs side, do this:
  ;;  (fi:start-interface-via-file "pie" "*common-lisp*" "~/.eli-startup")
  (save-excursion
    (set-buffer (get-buffer-create buffer))
    (setq fi::lisp-host host)
    (let ((command (fi::file-contents connection-file)))
      (or (string-match "\\(.*\\)" command)
	  (error "couldn't parse connection string in %s." connection-file))
      (fi::set-connection-vars command))

    (let ((host fi::lisp-host)
	  (port fi::lisp-port)
	  (pw fi::lisp-password)
	  proc)
      (setq proc
	(fi::make-tcp-connection buffer 1
				 'fi:lisp-listener-mode
				 fi:common-lisp-prompt-pattern
				 fi::lisp-host fi::lisp-port
				 fi::lisp-password
				 'fi::setup-tcp-connection))
      (setq-default fi::lisp-host host)
      (setq-default fi::lisp-port port)
      (setq-default fi::lisp-password pw)
      
      (setq fi::common-lisp-backdoor-main-process-name (process-name proc))

      ;; Make the backdoor connection, too:
      (fi::make-connection-to-lisp fi::lisp-host fi::lisp-port
				   fi::lisp-password)
      (when (memq 'fi:show-run-status fi:start-lisp-interface-hook)
	(fi:show-run-status))
      proc)))

(defun fi::probe-file (file)
  (when (file-exists-p file)
    file))

(defun fi::package ()
  (cond
   ((not fi::multiple-in-packages) fi:package)
   (t
    ;; look back from the point for the correct package name
    (save-excursion (fi::parse-package-from-buffer t t t)))))

(defun fi::temporary-directory ()
;;;; This calculation needs to be the same as in ACL's
;;;; sys:temporary-directory, otherwise the rendevzous file won't be
;;;; found.
  (let ((temp (getenv "TEMP"))
	(tmp (getenv "TMP")))
    (or (and temp (fi::probe-file temp))
	(and tmp (fi::probe-file tmp))
	(fi::probe-file "/tmp/")
	(fi::probe-file "c:/tmp/")
	(fi::probe-file "c:/"))))

(defun fi::start-lisp-interface (use-background-streams
				 &optional lisp-image-name)
  (if (on-ms-windows)
      (fi::start-lisp-interface-windows use-background-streams
					lisp-image-name)
    (fi::start-lisp-interface-unix use-background-streams lisp-image-name)))

(defun fi::start-lisp-interface-windows (use-background-streams
					 &optional lisp-image-name)
  ;; On Windows there is a very small limit on the size of a command line.
  ;; So, we have to jump through hoops to get the below form evaluated by
  ;; lisp. 
  (let* ((xtemp-file
	  (format "%s\\elidaemon%s" (fi::temporary-directory)
		  (emacs-pid)))
	 (temp-file
	  (if (on-ms-windows)
	      (substitute ?/ ?\\ xtemp-file)
	    xtemp-file))
	 (args (list "-L"
		     (if (cygwinp)
			 ;; ACL doesn't grok cygwin syntax
			 (cygwin-to-windows-pathname temp-file)
		       temp-file)
;;;; has to be after the load, so we can delete it without a sharing
;;;; violation on Windows.
		     "-f" "excl::new-start-emacs-lisp-interface-cleanup")))

    (when lisp-image-name
      (when (string-match "^~" lisp-image-name)
	(setq lisp-image-name (expand-file-name lisp-image-name)))
      (setq args
	(append (list "-I"
		      (if (cygwinp)
			  (cygwin-to-windows-pathname lisp-image-name)
			lisp-image-name))
		args)))
    
    (with-temp-file temp-file
      (insert
       (if (on-ms-windows)
	   (format
	    "\
 (in-package :excl)\n\
 (if (fboundp (quote new-start-emacs-lisp-interface))\
     (new-start-emacs-lisp-interface :background-streams %s)\
   (progn\
     (format t\
      \"An error (~s) starting older Lisps from Emacs can be ignored.\"\
      \"eof encountered on stream\")\
     (terpri)\
     (start-emacs-lisp-interface %s)))\n\
 (defun new-start-emacs-lisp-interface-cleanup ()\
   (ignore-errors (delete-file \"%s\")))\n"
	    use-background-streams use-background-streams
	    (if (cygwinp) (cygwin-to-windows-pathname temp-file) temp-file))
	 (format
	  "\
 (in-package :excl)\n\
 (if (fboundp (quote new-start-emacs-lisp-interface))\
     (new-start-emacs-lisp-interface :background-streams %s)\
   (start-emacs-lisp-interface %s))\n\
 (defun new-start-emacs-lisp-interface-cleanup ()\
   (ignore-errors (delete-file \"%s\")))"
	  use-background-streams use-background-streams temp-file))))
    
    args))

(defun fi::start-lisp-interface-unix (use-background-streams
				      &optional lisp-image-name)
  (let ((args
	 (list
	  "-e"
	  (format
	   "\
 (if (fboundp (quote excl::new-start-emacs-lisp-interface))\
     (excl::new-start-emacs-lisp-interface :background-streams %s)\
   (excl:start-emacs-lisp-interface %s))"
	   use-background-streams use-background-streams))))

    (when lisp-image-name
      (when (string-match "^~" lisp-image-name)
	(setq lisp-image-name (expand-file-name lisp-image-name)))
      (setq args (append (list "-I" lisp-image-name) args)))
    
    args))

(defun cygwin-to-windows-pathname (string)
  ;; /c/foo => c:/foo
  ;; /cygdrive/c/foo => c:/foo
  (cond
   ((or (string-match "/cygdrive/\\([A-Za-z]\\)\\(/.*\\)" string)
	(string-match "/\\([A-Za-z]\\)\\(/.*\\)" string))
    (concat (match-string 1 string) ":" (match-string 2 string)))
   (t string)))

(defun cygwin-to-windows-process-id (proc)
  (cygwin-to-windows-process-id-via-ps (process-id proc)))

(defun cygwin-to-windows-process-id-via-ps (cygwin-pid)
  (let ((pid (int-to-string cygwin-pid))
	(output
	 (cdr (split-string (shell-command-to-string "ps -al")
			    "[\n\r]")))
	(s "[ \t]+")
	(d "[0-9]+")
	pid)
    (while output
      (when (string-match
	     (concat s pid		; CYGWIN PID
		     s d		; PPID
		     s d		; PGID
		     s "\\(" d "\\)"	; WINPIN
		     s)
	     (car output))
	(setq pid (match-string 1 (car output))))
      (setq output (cdr output)))
    (when pid (string-to-int pid 10))))

(defun fi::double-char-in-string (char string)
  (let ((oldindex 0)
	(max+1 (length string))
	(nfound 0)
	newindex
	new
	c)
    (while (< oldindex max+1)
      (when (= char (aref string oldindex))
	(incf nfound))
      (incf oldindex))
    
    (setq new (make-string (+ nfound (length string)) 0)
	  oldindex 0
	  newindex 0)
    (while (< oldindex max+1)
      (cond ((= char (setq c (aref string oldindex)))
	     (aset new newindex char)
	     (incf newindex)
	     (aset new newindex char))
	    (t (aset new newindex c)))
      (incf oldindex)
      (incf newindex))
    new))
