;;; perldoc.el --- Show help for Perl functions, builtins, and modules.

;;
;; Copyright (C) 2000-2002 Steve Kemp <skx@tardis.ed.ac.uk>
;; Copyright (C) 2003, 2005 Peter S Galbraith <psg@debian.org>
;; Copyright (C) 2008-2009 Ben Voui <intrigeri@boum.org>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;

;;  This package allows the user to view the Perl help for the word(s) at
;;  the point.
;;
;;  When this is loaded it adds a hook both `cperl-mode', and `perl-mode',
;; allowing the perldoc help to be shown for the thing under the point, by
;; pressing F1.
;;
;;  The code handles functions, builtins, and third party modules.

;;; Version History
;;
;;  1.0 - Initial Release.
;;  1.1 - Show error message when no help is found.
;;      - Fix name.
;;      - Include GPL + URL.
;;  1.2 Alan Shutko <ats@acm.org>
;;        perldoc runs a pager, so run a benign one.  See Debian bug
;;        http://bugs.debian.org/144963
;;  1.3 Peter S Galbraith <psg@debian.org>
;;      - Checkdoc clean.
;;      - Generate list of functions on the fly instead of using a
;;        hardwired list.
;;      - ToDo? Allow completion on module names, harvested from all .pod
;;              file under directories in @INC.
;;  1.4 Peter S Galbraith <psg@debian.org>
;;      - Handle case where Debian perldoc package is not installed.
;;        Thanks to Kevin Ryde <user42@zip.com.au> for the full bug report.
;;
;;  1.5 Peter S Galbraith <psg@debian.org>
;;      - Apply patch from Kevin Ryde (Closes: #314869)
;;
;;  1.6 Ben Voui <intrigeri@boum.org>
;;      - Complete modules names as well.
;;      - Allow using several uniquely-named perldoc buffers, thanks to the
;;        perldoc-unique-buffer custom setting.
;;
;;  1.7 Ben Voui <intrigeri@boum.org>
;;      - A non-nil interactive argument forces the cache to be updated.
;;
;;  1.8 Ben Voui <intrigeri@boum.org>
;;      - Avoid saving incomplete perldoc-modules-alist
;;        perldoc-functions-alist (Closes: #575455)
;;
;;  1.9 Ben Voui <intrigeri@boum.org>
;;      - Don't depend on the existence of default-directory.
;;        Thanks to Kevin Ryde <user42@zip.com.au> for the patch.
;;        (Closes: #574650)
;;
;;  2.0 Ben Voui <intrigeri@boum.org>
;;      - Complete for Perl core documentation.
;;        Thanks to Florian Ragwitz <rafl@debian.org> for the bug report.
;;        (Closes: #589785)


;;  Comments / suggests / feedback welcomed to
;;  skx@tardis.ed.ac.uk and intrigeri@boum.org

;;  intrigeri's "upstream" lives in a Git repository:
;;  git://gaffer.ptitcanardnoir.org/perldoc-el.git

;;; Code:

(require 'thingatpt)

(autoload 'Man-fontify-manpage "man")

(defgroup perldoc nil
  "Show help for Perl functions, builtins, and modules."
  :group  'help)

(defcustom perldoc-define-F1 nil
  "If non-nil, bind [F1] to `perl-doc-at-point' in perl modes.
It installs `perldoc-perl-hook' in Perl mode hooks."
  :type 'boolean
  :group 'perldoc
  :require 'perldoc
  :set (lambda (symbol value)
         (set-default symbol value)
         (cond
          (value
           (add-hook 'cperl-mode-hook 'perldoc-perl-hook)
           (add-hook 'perl-mode-hook 'perldoc-perl-hook))
          (t
           (remove-hook 'cperl-mode-hook 'perldoc-perl-hook)
           (remove-hook 'perl-mode-hook 'perldoc-perl-hook)))))

(defcustom perldoc-unique-buffer t
  "If nil, use uniquely-named buffers, such as *Perldoc Getopt::Long*.
Else, use a single *Perldoc* buffer."
  :type 'boolean
  :group 'perldoc
  )

(defvar perldoc-functions-alist nil
  "Alist holding the list of perl functions.")

(defun perldoc-functions-alist (&optional re-cache)
  "Return the alist of perl functions constructed from perlfunc.pod.
A non-nil argument forces caches to be updated."
  (if (and perldoc-functions-alist (not re-cache))
      perldoc-functions-alist
    (setq perldoc-functions-alist
	  (let ((tmp-buffer (get-buffer-create " *perldoc*"))
		(case-fold-search nil)
		(tmp-functions-alist nil))
	    (set-buffer tmp-buffer)
	    (erase-buffer)
	    (let ((default-directory "/"))
	      (shell-command "perldoc -u perlfunc" t))
	    (goto-char (point-min))
	    (cond
	     ((search-forward "Alphabetical Listing of Perl Functions" nil t)
	      (while (re-search-forward
		      "^=item \\(\\([a-z][^ //\n]*\\)\\|\\(I<\\(.*\\)> \\)\\)" nil t)
		(let ((entry (list (or (match-string 2)(match-string 4)))))
		  (when (not (member entry tmp-functions-alist))
		    (push entry tmp-functions-alist))))
	      ;; no output means the perldoc program doesn't exist or is only the
	      ;; debian perl package dummy script
	      (unless tmp-functions-alist
		(error "`perldoc' program not available"))
	      tmp-functions-alist)
	     ((re-search-forward "You need to install.*" nil t)
	      (error (format "%s" (match-string 0))))
	     (t
	      (error "`perldoc' program not available")))))))

(defvar perldoc-modules-alist nil
  "Alist holding the list of perl modules.")

(defun perldoc-modules-alist (&optional re-cache)
  "Return the alist of perl modules found in @INC.
An non-nil argument forces caches to be updated."
  (if (and perldoc-modules-alist (not re-cache))
      perldoc-modules-alist
    (setq perldoc-modules-alist
	  (let ((tmp-buffer (get-buffer-create " *perldoc*"))
		(case-fold-search nil)
		(perldoc-inc nil)
		(tmp-modules-alist nil))
	    (set-buffer tmp-buffer)
	    (erase-buffer)
	    (let ((default-directory "/"))
	      (shell-command "perl -e 'print \"@INC\"'" t))
	    (goto-char (point-min))
	    (while (re-search-forward "\\(/[^ ]*\\)" nil t)
	      (let ((libdir (match-string 1)))
		(when (not (member libdir perldoc-inc))
		  (push libdir perldoc-inc))))
	    (dolist (dir perldoc-inc)
	      (let (modules (list))
		(when (file-readable-p dir)
		  (erase-buffer)
		  (let ((default-directory "/"))
		    (shell-command (concat "find -L " dir " -name '[A-Z]*.pm' -o -name '*.pod'") t))
		  (goto-char (point-min))
		  (while (re-search-forward (concat "^" (regexp-quote dir) "/\\(.*\\).\\(pm\\|pod\\)$") nil t)
		    (let ((entry (list (replace-regexp-in-string "/" "::"
								 (replace-regexp-in-string "^pod/" "" (match-string 1))))))
		      (when (not (member entry tmp-modules-alist))
			(push entry tmp-modules-alist)))))))
	    tmp-modules-alist))))

(defvar perldoc-all-completions-alist nil
  "Alist holding the list of perl functions and modules.")

(defun perldoc-all-completions-alist (&optional re-cache)
  "Return the alist of perl functions and modules.
A non-nil argument forces the caches to be updated."
  (if (and perldoc-all-completions-alist (not re-cache))
      perldoc-all-completions-alist
    (message "Building completion list of all perldoc topics...")
    (setq perldoc-all-completions-alist
	  (append (perldoc-functions-alist t)
		  (perldoc-modules-alist t)))))

;;;###autoload
(defun perldoc (&optional string re-cache)
  "Run perldoc on the given STRING.
If the string is a recognised function then we can call `perldoc-function',
otherwise we call `perldoc-module'.
A non-nil interactive argument forces the caches to be updated."
  (interactive (list nil current-prefix-arg))
  (if (or re-cache
	  (not perldoc-all-completions-alist))
    (perldoc-all-completions-alist t))
  (unless (stringp string)
    (setq string (completing-read "Perl function or module: "
				  (perldoc-all-completions-alist) nil nil)))
  (cond
   ((assoc string perldoc-functions-alist)
    (perldoc-function string))
   ((stringp string)
    (perldoc-module string))
   (t
    (message "Nothing to find."))))

(defun perldoc-get-buffer-name (target)
  "Return the buffer name used to display documentation about TARGET."
  (or
   (and (not perldoc-unique-buffer)
	(stringp target)
	(concat"*Perldoc " target "*"))
   "*Perldoc*"))

(defun perldoc-start-process (&rest args)
  "Call perldoc with ARGS.
Sets up process sentinals and needed environment to call perldoc."
  (let ((buffer-name (perldoc-get-buffer-name (car (reverse args)))))
    (set-buffer (get-buffer-create buffer-name))
    (kill-all-local-variables)
    (erase-buffer)
    (text-mode)
    (message "Loading documentation ..")
    (set-process-sentinel
     (let ((default-directory "/"))
       (apply 'start-process args))
     'perldoc-sentinel)))

(defun perldoc-function (function)
 "Show the help text for the given Perl FUNCTION / builtin."
 (interactive (list (completing-read "Perl function: "
                                     (perldoc-functions-alist) nil t)))
 (perldoc-start-process "perldol" (perldoc-get-buffer-name function) "perldoc" "-T" "-f" function))

(defun perldoc-module (module)
 "Show the help text for the given Perl MODULE."
 (interactive (list (completing-read "Perl module: "
                                     (perldoc-modules-alist) nil t)))
   (perldoc-start-process "perldol" (perldoc-get-buffer-name module) "perldoc" "-T" module))

(defun perldoc-process-filter (proc string)
  "Process the results from the catdoc process PROC, inserting STRING."
  (message "buffer: %s" (process-buffer proc))
  (set-buffer (process-buffer proc))
  (insert string))

(defun perldoc-sentinel (proc msg)
  "Perldoc sentinel for process PROC and MSG describing the change.
When the catdoc process has finished, switch to its output buffer."
  (let ((buffer (process-buffer proc)))
    (when (eq (process-status proc) 'exit)
      (set-buffer buffer)
      (goto-char (point-min))
      (cond
       ((and (< (count-lines (point-min) (point-max)) 2)
	     (re-search-forward "No documentation found for .*" nil t))
	(message (match-string 0))
	(kill-buffer (get-buffer buffer)))
       (t
	(pop-to-buffer buffer)
	(goto-char (point-min))
	(let ((Man-args "perldoc"))
	  (Man-fontify-manpage)))))))

;;;###autoload
(defun perldoc-at-point ()
  "Call `perldoc' for string at point."
  (interactive)
  (perldoc (or (thing-at-point 'word)
               (thing-at-point 'filename))))

;;;###autoload
(defun perldoc-perl-hook ()
  "A hook which binds F1 to `perldoc-at-point'."
  (local-set-key [f1] 'perldoc-at-point))

(provide 'perldoc)
;;; perldoc.el ends here
