
(setq next-line-add-newlines nil)

(require 'cl)

;;
;; EvW's prefs
;; To do:
;;  - (EvW-save-and-kill-buffer) make argument work sensibly (?)
(defun ISO-current-time-string ()
  (format-time-string "%Y-%m-%dT%T%z"))
(defun EvW-insert-date ()
  "time and date function suggested by Tom Capey (2000-11-28T19:21:04+0100)"
  (interactive)
  (insert (ISO-current-time-string)))

;;;open a buffer for logging how my time is spent...
(find-file "~/src/CL/systems/timekeeper/data/uren.csv")
(goto-char (point-max))

(defun EvW-save-and-kill-buffer (bufname)
  "Saves current buffer, then kills it"
  (interactive "b")
  (let ((cb (current-buffer)))
    (save-buffer cb)
    (kill-buffer cb)))

(defun insert-lisp-file-header (pkg)
  "inserts a Lisp header, assuming the file is empty."
  (insert ";;; -*- Mode: Lisp -*-")
  (insert "\n\n(cl:in-package ")
  (let ((pkg-char (point-max)))	 ;to check package-name later
    (insert pkg ")\n\n")	 ;how to downcase pkg??
    (insert ";;;    Author: " user-full-name " <" user-mail-address ">\n")
    (insert ";;;   Created: " (ISO-current-time-string) " as " (buffer-name) "\n")
    (insert ";;;Time-stamp: <>\n")
    (insert ";;;   Purpose:\n")
    (insert ";;;     To do:\n")
    (insert ";;;   Remarks:\n\n")
    (insert ";;; EOF ")
    (goto-char pkg-char)))

(defun find-lisp-file (fpath)	      ;add a file header when in Lisp-mode
  "Finds (Lisp) file indicated and if empty, inserts Lisp comments."
  (interactive "FFind (lisp) file: ") ;get file name if called in minibuffer
  (let ((pkg			      ;use a sensible package
	 (cond ((boundp 'fi:package) fi:package)
	       ((boundp 'fi:default-package) fi:default-package)
	       ((fboundp 'slime-current-package)
		(or (slime-current-package) ":cl-user"))
	       (t  ":cl-user")))
	(buf (find-file fpath)))
    (set-buffer buf)
    (when (and (= 0 (buffer-size))
	       (or (eq major-mode 'lisp-mode)
		   (eq major-mode 'slime-repl-mode)))
      (insert-lisp-file-header pkg))
    buf))


(global-set-key "\M-t" 'EvW-insert-date)
(global-set-key "\C-x\C-c" 'EvW-save-and-kill-buffer)
(global-set-key "\C-x\C-f" 'find-lisp-file)

(defvar font-lock-maximum-decoration t)
(global-font-lock-mode t)

(add-hook 'write-file-hooks 'time-stamp)
(defvar time-stamp-active t)
(defvar time-stamp-format "%3a, %:y-%02m-%02dT%02H:%02M:%02S -- %f")

;;; Western European character set (klopt geen reet van)!
;;(add-hook 'text-mode-hook '(lambda () (standard-display-european 1) ))
;;(add-hook 'text-mode-hook '(lambda () (iso-accents-mode 1) ))

(custom-set-faces)
;;; Jim Traugott [traugott@iname.com] -- 2000-02-29T23:42:55
;;; Set the frame's title. %b is the name of the buffer. %+ indicates the
;;; state of the buffer: * if modified, % if read only, or - otherwise. Two
;;; of them to emulate the mode line. %f for the file name.
(setq frame-title-format "Emacs: %b %+%+ %f")
(setq icon-title-format "Emacs - %b")


;;;(require 'eol-conversion)		;remove this for Emacs 21.2?
;;;Playing around with colors
;;;(set-face-foreground 'highlight	   "black")
(set-face-background 'highlight	   "green")

;;;(set-face-foreground 'region	   "black")
(set-face-background 'region	   "green")

;;;(set-face-foreground 'secondary-selection	 "antiquewhite1")
(set-face-background 'secondary-selection    "Yellow")

(setq-default search-highlight t)
(copy-face 'highlight 'isearch)
(setq-default query-replace-highlight t)

;;;but John ffitch likes these
(custom-set-faces
 '(my-tab-face		  ((((class color)) (:background "lemon chiffon"))) t)
 '(my-trailing-space-face ((((class color)) (:background "Gold"))) t))

(defvar font-lock-keywords)

(add-hook 'font-lock-mode-hook
	  (function
	   (lambda ()
	     (setq font-lock-keywords
		   (append font-lock-keywords
			   '(("\t+" (0 'my-tab-face t))
			     ("[ \t]+$" (0 'my-trailing-space-face t))))))))

;;;(add-hook 'dired-load-hook
;;;	  (lambda () (require 'dired-sort-menu)))

;;;highlight during isearch!
;;;(require 'ishl)
;;;(ishl-mode t)
;;;(setq-default ishl-cleanup nil)

;; if you encounter a file with ^M or ... at the end of every line,
;; this means a worng copy by samba or floppy disk of the DOS file to UNIX.
;; get rid of them by pressing [F5].
(defun cut-ctrlM ()
  (interactive)
  (goto-char (point-min));(beginning-of-buffer)
  (while (search-forward "\r" nil t)
    (replace-match "" nil t))
  (not-modified)
  (goto-char (point-min)))

(defun replace-ctrlM (ch)		;2001-04-17T17:46:41+0200
  (interactive)
  "Replace all visible ^M with argument (defaulting to the empty string.)"
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (replace-match ch nil t))
  ;;(not-modified)
  (goto-char (point-min)))

(defun remove-trailing-whitespace ()
  "Removes trailing blanks and tabs from the buffer."
  (interactive)
  (save-excursion			;let deactivate-mark nil?
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match "" nil nil))))

(defun tidy-up ()
  "Removes trailing whitespace and ^M from the end of lines.
Bug: does not leave point at the position it had when the function was called"
  (interactive)
  (save-excursion
    (let ((curpos (point)))
      (cut-ctrlM)
      (remove-trailing-whitespace)
      (goto-char curpos))))

;;; EOF