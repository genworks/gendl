;; The Franz Inc. Lisp/Emacs interface.

(setq fi::*debug* nil)

(require 'cl)

(defun on-ms-windows ()
  (or (cygwinp)
      (memq system-type '(windows-nt ms-windows ms-dos win386))))

(defun cygwinp ()
  (memq system-type '(cygwin cygwin32 )))

(defvar *on-windows-nt*
    (and (on-ms-windows)
	 (string-match "Windows_NT" (or (getenv "OS") ""))))

(setq fi::emacs-type
  (let ((case-fold-search t))
    (cond ((or (string-match "xemacs" emacs-version)
	       (string-match "lucid" emacs-version))
	   (cond ((string-match "^2[012]\\." emacs-version) 'xemacs20)
		 (t 'xemacs19)))
	  ((boundp 'epoch::version) 'epoch) ;; remove?
	  ((string-match "^18\\." emacs-version) 'emacs18)
	  ((string-match "^19\\." emacs-version) 'emacs19)
	  (t ;; assume emacs20 compatibility
	   'emacs20))))

;; spr30075
;; Remove if we ever stop using make-ring.
(when (eq 'xemacs20 fi::emacs-type)
  (condition-case () (make-ring 1)
    (error
     (error "make-ring, needed for ELI, not available.  Most likely, xemacs-base package is missing."))))


(defvar fi::load-subprocess-files t)
(defvar fi::install-acl-menubar t)
(defvar fi::build-time nil)
(defvar fi::emacs-type nil)

(defvar fi::*current-frame-width* nil)
(defvar fi::*new-frame-width* nil)
(when (fboundp 'frame-width)
  (ignore-errors
   (setq fi::*current-frame-width* (1- (window-width)))))

(defun fi::find-path (path file)
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

(defvar fi::library-directory nil)
(cond
 ((or (not (boundp 'load-file-name))
      ;; The following clause is due to Emacs 20.2, where load-file-name is
      ;; bound but `nil'.  I was told by Erik Naggum that this bug would be
      ;; fixed in Emacs 20.3.
      (null load-file-name))
  ;; In this case, load-path must be used to find fi/fi-site-init.el and
  ;; the directory where it is found is used as fi::library-directory.
  (let* ((file "fi-site-init.el")
	 (path
	  (or (fi::find-path load-path (format "fi/%s" file))
	      (fi::find-path load-path file))))
    (when (not path)
      (error "Can't find fi/%s or %s in your load-path." file file))
    (unless fi::library-directory
      (setq fi::library-directory (file-name-directory path))))
  ;; Toss this version of fi::find-path, since it is redefined later
  ;; (compiled, too).
  (fset 'fi::find-path nil))
 ((null fi::library-directory) 
  (setq fi::library-directory (file-name-directory load-file-name))))

(defun fi::locate-library (file)
  (let (p)
    (cond
     ((string-match "\\.elc?$" file)
      (if (file-exists-p (setq p (format "%s%s" fi::library-directory file)))
	  p
	nil))
     ((and (setq p (format "%s%s.elc" fi::library-directory file))
	   (file-exists-p p))
      p)
     ((and (setq p (format "%s%s.el" fi::library-directory file))
	   (file-exists-p p))
      p)
     ((and (setq p (format "%s.elc" file)) (file-exists-p p)) p)
     ((and (setq p (format "%s.el" file)) (file-exists-p p)) p)
     (t nil))))

(when (and (or (eq fi::emacs-type 'emacs19)
	       (eq fi::emacs-type 'xemacs19))
	   (not (on-ms-windows)))
  ;; needed for setf expansions (on some version 19.xx) when they are
  ;; compiled with non-version 19 byte-compilers.
  (let ((debug-on-error nil))
    (condition-case nil
	(require 'cl-compat)
      (error nil))))

(defvar fi::initialization-forms nil)
(defun fi::initialize-emacs-lisp-interface ()
  (when fi::initialization-forms
    (dolist (form (nreverse fi::initialization-forms))
      (eval form))
    (setq fi::initialization-forms nil)))

(unless (fboundp 'add-hook)	;not in 18.59
  (defun add-hook (hook function &optional append)
    "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
    (or (boundp hook) (set hook nil))
    ;; If the hook value is a single function, turn it into a list.
    (let ((old (symbol-value hook)))
      (if (or (not (listp old)) (eq (car old) 'lambda))
	  (set hook (list old))))
    (or (if (consp function)
	    (member function (symbol-value hook))
	  (memq function (symbol-value hook)))
	(set hook
	     (if append
		 (nconc (symbol-value hook) (list function))
	       (cons function (symbol-value hook)))))))

;; This style of loading is more appropriate on a single-user machine, like
;; a Windows machine.  It is even more appropriate on Windows where `make'
;; does not exist except for people who pay for it.  It seems to be the
;; best way to deal with the issue of how to compile the .el files.
(defvar fi:compile-at-load-time (on-ms-windows)
  "*If non-nil, then compile and load files in the fi/ directory.
Otherwise, the .elc will be loaded in preference to the .el file, if it
exists.")

(defvar fi::load-el-files-only nil)

(defun fi::load (file &rest load-before-compile)
  (cond
   (fi::load-el-files-only
    (let ((lib-file (fi::locate-library file))
	  el-lib-file)
      (unless lib-file (error "can't find file %s" file))
      (dolist (required-file load-before-compile)
	(load required-file))
      (cond ((string-match "\\.el$" lib-file)
	     (load lib-file))
	    ((string-match "\\.elc$" lib-file)
	     (load (substring lib-file 0 (- (length lib-file) 1))))
	    (t (error "not el or elc")))))
   (fi:compile-at-load-time
    (let ((lib-file (fi::locate-library file))
	  el-lib-file)
      (unless lib-file (error "can't find file %s" file))
      (dolist (required-file load-before-compile)
	(load required-file))
      (cond ((string-match "\\.el$" lib-file)
	     ;; this compiles and loads:
	     (byte-compile-file lib-file t))
	    ((and (string-match "\\.elc$" lib-file)
		  (file-newer-than-file-p
		   (setq el-lib-file
		     (substring lib-file 0 (- (length lib-file) 1)))
		   lib-file))
	     (byte-compile-file el-lib-file t))
	    (t (load lib-file)))))
   (t (load (or (fi::locate-library file)
		(error "Couldn't find \"%s\"" file))))))

(defun fi::load-compiled (file &rest load-before-compile)
  (let ((lib-file (fi::locate-library file)))
    (unless lib-file (error "can't find file %s" file))
    (dolist (required-file load-before-compile)
      (load required-file))
    (cond ((string-match "\\.el$" lib-file)
	   ;; no .elc file
	   (byte-compile-file lib-file t)
	   (setq lib-file (format "%sc" lib-file)))
	  (t
	   (let* ((xx (string-match "\\(.*\\)\\.elc$" lib-file))
		  (base (substring lib-file (match-beginning 1)
				   (match-end 1)))
		  (el (format "%s.el" base))
		  (elc (format "%s.elc" base)))
	     (when (file-newer-than-file-p el elc)
	       (byte-compile-file el t)))))
    (load lib-file)))

(fi::load "fi-vers")

(when (and (= 20 (car fi::compiled-with-version))
	   (= 19 emacs-major-version)
	   (eq 'emacs19 fi::emacs-type))
  (delete-other-windows)
  (switch-to-buffer "*Help*")
  (when buffer-read-only (toggle-read-only))
  (erase-buffer)
  (insert "
You must byte-recompile the .el files for GNU Emacs 19.x since there are
incompatibilities between FSF GNU Emacs going from version 20 to 19.

On UNIX, you can do this by running the following commands in the
<acldir>/eli/ directory:

  make clean
  make

On Windows, just remove all .elc files and restart emacs.")
  (goto-char (point-min))
  (beep)
  (error "You must byte-recompile the .el files for GNU Emacs 19.x."))

(when (and (= 19 (car fi::compiled-with-version))
	   (eq 'xemacs20 fi::emacs-type)
	   (>= emacs-major-version 20))
  (delete-other-windows)
  (switch-to-buffer "*Help*")
  (when buffer-read-only (toggle-read-only))
  (erase-buffer)
  (insert "
You must byte-recompile the .el files for XEmacs 20.x since there are
incompatibilities between FSF GNU Emacs version 19.x and XEmacs 20.x.

On UNIX, you can do this by running the following commands in the
<acldir>/eli/ directory:

  make clean
  make emacs=<xemacs20> xemacs=<xemacs20>

where <xemacs20> is the name of your XEmacs 20.x binary.

On Windows, just remove all .elc files and restart xemacs.")
  (goto-char (point-min))
  (beep)
  (error "You must byte-recompile the .el files for XEmacs 20.x."))

(fi::load "fi-utils")
(fi::load "fi-keys")			; load before fi-modes
(fi::load "fi-modes")
(when fi:lisp-do-indentation
  (fi::load "fi-indent"))
(fi::load "fi-gnu")

(when (not fi::load-subprocess-files)
  (defvar fi:package nil)
  (make-variable-buffer-local 'fi:package)
  ;; used in Common Lisp edit mode:
  (setq fi::common-lisp-backdoor-main-process-name nil))

(when fi::load-subprocess-files
  (fi::load "fi-subproc")
  (fi::load "fi-sublisp")
  (fi::load "fi-basic-lep")
  (fi::load "fi-lep")
  (fi::load "fi-dmode")
  (fi::load "fi-composer")
  (fi::load "fi-lze")
  (fi::load "fi-changes")
  (fi::load "fi-db")
  (fi::load "fi-stream")

  (when (eq fi::emacs-type 'epoch)
    (fi::load "fi-leep0")
    (fi::load "fi-leep"))

  (when (or (eq fi::emacs-type 'xemacs19)
	    (eq fi::emacs-type 'xemacs20))
    (fi::load "fi-leep0")
    (fi::load "fi-leep-xemacs"))

  (fi::load "fi-ring")
  (fi::load "fi-filec")

  ;; `shell' and `rlogin' modes:
  (fi::load "fi-shell")
  (fi::load "fi-rlogin")
  (fi::load "fi-telnet")
  (fi::load "fi-su"))

(fi::load "fi-manual")

(condition-case nil
    (fi::load "localfidev")
  (error nil))

(setq fi:package-loaded t)

;; the test for GNU Emacs 19 has to be after that for xemacs, because
;; the version of xemacs is 19.* too!

(when fi::load-subprocess-files
  (cond ((or (eq fi::emacs-type 'xemacs19)
	     (eq fi::emacs-type 'xemacs20))
	 (fi::load "fi-xemacs"))
	
	((and (boundp 'emacs-major-version)
	      (>= emacs-major-version 21))
	 (fi::load "fi-emacs21"))
	
	((or (eq fi::emacs-type 'emacs19)
	     (eq fi::emacs-type 'emacs20))
	 (fi::load "fi-emacs19"))
	
	(t (fi::load "fi-emacs18"))))

(defun fi::top-level ()
  (fi::initialize-emacs-lisp-interface)
  (eval fi::build-time))

(if fi::build-time
    (progn
      (setq fi::build-time top-level)
      (setq top-level '(fi::top-level)))
  (fi::initialize-emacs-lisp-interface))

(provide 'fi-site-init)
