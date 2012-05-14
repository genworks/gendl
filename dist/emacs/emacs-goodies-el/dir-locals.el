;;; dir-locals.el --- Local variables for a directory tree

;; Copyright (C) 2005, 2006  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: files
;; $Revision: 1.1 $
;; URL: http://www.loveshack.ukfsn.org/emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; It can be useful to specify local variables directory-wide, e.g. to
;; define CC mode styles consistently.  This library implements such a
;; scheme, controlled by the global minor mode `dir-locals-mode'.

;; Place a file named `.emacs-locals' (or the value of
;; `dir-locals-file-name') in the directory root.  This should specify
;; local variables in the usual way.  The values it sets are inherited
;; when a file in the directory tree is found.  Local variables
;; specified in the found file override the directory-wide ones.

;; However, `eval' pseudo-variables specified in the file are
;; evaluated (assuming `enable-local-eval' is true) _before_ any
;; directory-wide processing, and they are evaluated in a scratch
;; buffer, so that they are only useful for side effects on local
;; variables.  `mode' pseudo-variables which specify minor modes
;; toggle those modes for files within the directory.  If
;; .emacs-locals specifies a major mode, it doesn't propagate, but any
;; local variables and minor modes its hook sets will; thus it should
;; normally not specify a major mode.  The `coding' pseudo-variable
;; will not propagate from .emacs-locals.

;; For example, with dir-locals mode on, placing this in .emacs-locals
;; at the top-level of the Linux source tree would set the C
;; indentation style appropriately for files within the tree:
;;
;;   Local variables:
;;   c-file-style: "linux"
;;   End:
;;
;; (and ignore the stupid remarks in Documentation/CodingStyle).

;; Another possible use is, say, setting change-log parameters in
;; different trees for which the Emacs 22 development source broke use
;; of change-log-mode-hook.

;; NB:  This doesn't work with some versions of the Emacs 22 codebase
;; which changed the way hack-local-variables-hook is run, but the
;; change has been reverted.

;; Another, less clean, implementation of this sort of thing was
;; posted to gnu-emacs-sources as dirvals.el by Benjamin Rutt
;; <rutt.4@osu.edu>, June 2006, based on work by Matt Armstrong
;; <matt@lickey.com>.  It uses a different format for the equivalent
;; of .emacs-locals.

;;; Code:

(defgroup dir-locals ()
  "Directory-wide file-local variables"
  :link '(emacs-commentary-link "dir-locals")
  :group 'files)

(defcustom dir-locals-file-name ".emacs-locals"
  "File name used by Dir-Locals mode to specify local variables.
This should specify local variables in the normal way.  When Dir-Locals
minor mode is active, these will be inherited by files found in a
directory tree containing such a file at its root.

This may also be a function of no arguments which returns the name to
use, allowing arbitrary per-directory customization of the
per-directory customization file on the basis of `default-directory'."
  :group 'dir-locals
  :type '(choice file function))

;; Adapted from dirvals.el.
(defcustom dir-locals-chase-remote nil
  "Non-nil means search upwards for `dir-locals-file-name' in remote filesystem."
  :group 'dir-locals
  :type 'boolean)

;;;###autoload
(define-minor-mode dir-locals-mode
  "Toggle use of directory-wide file-local variables.
See `dir-locals-file-name'."
  :global t
  :require 'dir-locals
  (if dir-locals-mode
      (add-hook 'hack-local-variables-hook 'dir-locals-hack-local-variables)
    (remove-hook 'hack-local-variables-hook
		 'dir-locals-hack-local-variables)))

;; Following find-change-log.  Fixme:  Should be abstracted from there.
(defun dir-locals-tree-find (file)
  "Find FILE in the current directory or one of its parents.
If one is found, return its fully-qualified name, otherwise return
nil.

FILE may be a string or a nullary function returning one on the basis
of `default-directory'."
  (unless (and (not dir-locals-chase-remote)
	       (fboundp 'file-remote-p)	; not in Emacs 21
	       (file-remote-p default-directory))
    (let* ((dir-name
	    ;; Chase links in the source file and start searching in
	    ;; the dir where it resides.
	    (or (if buffer-file-name
		    (file-name-directory (file-chase-links buffer-file-name)))
		default-directory))
	   (file (if (functionp file)
		     (funcall file)
		   file))
	   (file1 (if (file-directory-p dir-name)
		      (expand-file-name file dir-name))))
      ;; Chase links before visiting the file.  This makes it easier
      ;; to use a file for several related directories.
      (setq file1 (expand-file-name (file-chase-links file1)))
      ;; Move up in the dir hierarchy till we find a suitable file.
      (while (and (not (file-exists-p file1))
		  (setq dir-name (file-name-directory
				  (directory-file-name
				   (file-name-directory file1))))
		  ;; Give up if we are already at the root dir.
		  (not (string= (file-name-directory file1) dir-name)))
	;; Move up to the parent dir and try again.
	(setq file1 (expand-file-name (file-name-nondirectory file) dir-name)))
      (if (file-exists-p file1)
	  file1))))

(defun dir-locals-hack-local-variables ()
  "Set local variables from directory-wide values.
Inherit the local variables set in `dir-locals-file-name' if that is
found by `dir-locals-tree-find'.  Ignore everything ignored by
`hack-local-variables'."
  (let* ((file (dir-locals-tree-find dir-locals-file-name))
	 (hack-local-variables-hook nil)
	 (buffer-file
	  (if buffer-file-name
	      (expand-file-name (file-chase-links buffer-file-name))))
	 ;; Fixme:  Probably condition-case this and ensure any error
	 ;; messages indicate the directory file.
	 (vars (when (and file
			  ;; Don't do it twice, so as to avoid
			  ;; repeating possible interactive queries.
			  (not (equal file buffer-file)))
		 (with-temp-buffer
		   ;; Make queries from `hack-local-variables' clearer.
		   (rename-buffer (file-name-nondirectory file) t)
		   (insert-file-contents file)
		   (let* ((locals (buffer-local-variables))
			  (_ (hack-local-variables))
			 (new-locals (buffer-local-variables)))
		     ;; Derive the list of new pairs.
		     (dolist (l locals)
		       (setq new-locals (delete l new-locals)))
		     ;; And some internals which get updated.
		     (dolist (l '(buffer-display-time buffer-display-count))
		       (setq new-locals (assq-delete-all l new-locals)))
		     new-locals)))))
    (dolist (v vars)
      (let ((sym (car v)))
	(unless (local-variable-p sym)	; file-locals take precedence
	  (if (and (string-match "-mode\\'" (symbol-name sym))
		   (fboundp sym))
	      (funcall sym)
	    (set (make-local-variable sym) (cdr v))))))))

(provide 'dir-locals)

;;; dir-locals.el ends here
