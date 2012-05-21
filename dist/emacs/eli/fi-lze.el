;; Copyright (c) 1987-2002 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, and to distribute modified
;; versions, provided that this complete copyright and permission notice is
;; maintained, intact, in all copies and supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

;; Code the implements evaluation in via the backdoor

(make-variable-buffer-local 'fi::show-compilation-status)
(setq fi::show-compilation-status nil)

(defvar fi::mode-line-note-for-compile " COMPILING")
(defvar fi::mode-line-note-for-eval " EVALUATING")

(defun fi::note-background-request (compiling)
  (let ((message1 (if compiling "Compiling" "Evaluating"))
	(message (if compiling
		     fi::mode-line-note-for-compile
		   fi::mode-line-note-for-eval)))
    (message "%s..." message1)
    (let ((item (assq 'fi::show-compilation-status minor-mode-alist)))
      (or (and item (not (string= "" (car (cdr item)))))
	  (or (and item (rplacd item (list message)))
	      (setq minor-mode-alist
		(cons (list 'fi::show-compilation-status message)
		      minor-mode-alist)))))
    (setq fi::show-compilation-status
      ;; this is so we can tell when lisp has died and been restarted:
      (fi::connection-process fi::*connection*))))

(defun fi::note-background-reply (&optional compiling)
  (let ((message (when compiling
		   (if (car compiling) "Compiling" "Evaluating"))))
    (if compiling (message "%s...done." message))
    (let ((item (assq 'fi::show-compilation-status minor-mode-alist)))
      (and item (rplacd item (list ""))))
    (setq fi::show-compilation-status nil)))

;;;;;;;;;;;;;

(defvar fi:lisp-evals-always-compile t
  "*This variable controls whether or not the fi:lisp-eval-or-compile-*
functions will compile or evaluate their forms.  If non-nil, then
compilation is the default, otherwise evaluation is the default.
The non-default functionality can be selected by using a prefix argument.")

(defun fi::decode-prefix-argument-for-eval-or-compile ()
  ;; return `t' for compile, `nil' for eval
  (list (if current-prefix-arg
	    (not fi:lisp-evals-always-compile)
	  fi:lisp-evals-always-compile)))

(defun fi:lisp-eval-or-compile-defun (compilep)
  "Send the current top-level (or nearest previous) form to the Lisp
subprocess associated with this buffer.  A `top-level' form is one that
starts in column 1.  See the documentation for
fi:lisp-evals-always-compile."
  (interactive (fi::decode-prefix-argument-for-eval-or-compile))
  (if compilep (fi:lisp-compile-defun) (fi:lisp-eval-defun)))

(defun fi:lisp-eval-or-compile-region (compilep)
  "Send the text in the region to the Lisp subprocess associated with this
buffer, one expression at a time if there is more than one complete
expression.  See the documentation for fi:lisp-evals-always-compile."
  (interactive (fi::decode-prefix-argument-for-eval-or-compile))
  (if compilep (fi:lisp-compile-region) (fi:lisp-eval-region)))

(defun fi:lisp-eval-or-compile-last-sexp (compilep)
  "Send the sexp before the point to the Lisp subprocess associated with
this buffer.  See the documentation for fi:lisp-evals-always-compile."
  (interactive (fi::decode-prefix-argument-for-eval-or-compile))
  (if compilep (fi:lisp-compile-last-sexp) (fi:lisp-eval-last-sexp)))

(defun fi:lisp-eval-or-compile-current-buffer (compilep)
  "Send the entire buffer to the Lisp subprocess associated with this
buffer.  See the documentation for fi:lisp-evals-always-compile."
  (interactive (fi::decode-prefix-argument-for-eval-or-compile))
  (if compilep (fi:lisp-compile-current-buffer) (fi:lisp-eval-current-buffer)))

;;;;;;;;;;

(defun fi:lisp-eval-defun ()
  "Send for evaluation the current top-level (or nearest previous) form to
the Lisp subprocess associated with this buffer.  A `top-level' form is one
that starts in column 1."
  (interactive)
  (let* ((end (save-excursion (end-of-defun) (point)))
	 (start (save-excursion
		  (fi:beginning-of-defun)
		  (point))))
    (fi::eval-region-internal start end nil)))

(defun fi:lisp-compile-defun ()
  "Send for compilation the current top-level (or nearest previous) form to
the Lisp subprocess associated with this buffer.  A `top-level' form is one
that starts in column 1."
  (interactive)
  (let* ((end (save-excursion (end-of-defun) (point)))
	 (start (save-excursion
		  (fi:beginning-of-defun)
		  (point))))
    (fi::eval-region-internal start end t)))

(defun fi:lisp-eval-region ()
  "Send for evaluation the region to the Lisp subprocess associated with
this buffer."
  (interactive)
  (fi::eval-region-internal (min (point) (fi::mark))
			    (max (point) (fi::mark))
			    nil))

(defun fi:lisp-compile-region ()
  "Send for compilation the region to the Lisp subprocess associated with
this buffer."
  (interactive)
  (fi::eval-region-internal (min (point) (fi::mark))
			    (max (point) (fi::mark))
			    t))

(defun fi:lisp-eval-last-sexp ()
  "Send for evaluation the sexp before the point to the Lisp subprocess
associated with this buffer."
  (interactive)
  (let ((start (save-excursion
		 (forward-sexp -1)
		 (point))))
    (fi::eval-region-internal start (point) nil)))

(defun fi:lisp-compile-last-sexp ()
  "Send for compilation the sexp before the point to the Lisp subprocess
associated with this buffer."
  (interactive)
  (let ((start (save-excursion
		 (forward-sexp -1)
		 (point))))
    (fi::eval-region-internal start (point) t)))

(defun fi:lisp-eval-current-buffer ()
  "Send for evaluation the entire buffer to the Lisp subprocess associated
with this buffer."
  (interactive)
  (fi::eval-region-internal (point-min) (point-max) nil t))

(defun fi:lisp-compile-current-buffer ()
  "Send for compilation the entire buffer to the Lisp subprocess associated
with this buffer."
  (interactive)
  (fi::eval-region-internal (point-min) (point-max) t t))

;;;;;;;;;;;;;

(defun fi::eval-region-internal (start end compilep &optional ignore-package)
  (fi::note-background-request compilep)
  (let ((buffer (current-buffer)))
    (fi::make-request
	(lep::evaluation-request
	 :transaction-directory fi:emacs-to-lisp-transaction-directory
	 ;; The addition of the format wrapper in the next line works
	 ;; around the incredible bogosity of fsf emacs 19.x that prints
	 ;; strings with non-null fontification using vector syntax.
	 ;; The format call reliably if inefficiently strips the font data.
	 ;; bug3330 smh 22jun94
	 :text (fi::defontify-string (buffer-substring start end))
	 :echo fi:echo-evals-from-buffer-in-listener-p
	 :partialp (and (not (and (eq (max start end) (point-max))
			     (eq (min start end) (point-min))))
			;; This value will communicate the starting
			;; buffer position to cl.
			start)
	 :pathname (buffer-file-name)
	 :compilep (if compilep t nil)
	 :return-string (eq 'minibuffer (car fi:pop-up-temp-window-behavior)))
      ((buffer compilep) (results stuff)
       (save-excursion
	 (set-buffer buffer)
	 (cond
	  ((eq 'minibuffer (car fi:pop-up-temp-window-behavior))
	   (if (and (stringp stuff) (= 0 (length stuff)))
	       (fi::note-background-reply (list compilep))
	     (fi:show-some-text nil stuff)))
	  (t
	   (fi::note-background-reply (list compilep))))
	 (when (and (stringp results) ;; bug15748
		    (null fi:echo-evals-from-buffer-in-listener-p))
	   (fi:show-some-text nil results)))
       (when fi:pop-to-sublisp-buffer-after-lisp-eval ; bug2683
	 (pop-to-buffer fi:common-lisp-buffer-name)
	 (goto-char (point-max))))
      ((buffer compilep) (error)
	(save-excursion
	  (set-buffer buffer)
	  (fi::note-background-reply (list compilep))
	  (message "Error during %s: %s"
		   (if compilep "compile" "eval")
		   error)))
      ignore-package)))

