;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.
;; Copyright (C) 1993-2001 Franz Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Find the file version.el in the path for lisp files, 
;; and set version-file.

(setq version-file (format "%sfi-site-init.el" default-directory))

(insert-file-contents version-file)

(re-search-forward "emacs-lisp-interface-version \"[^\"]*[0-9]+\"")
(forward-char -1)
(save-excursion
  (save-restriction
    (narrow-to-region (point)
		      (progn (skip-chars-backward "0-9") (point)))
    (goto-char (point-min))
    (setq version (1+ (read (current-buffer))))
    (delete-region (point-min) (point-max))
    (prin1 version (current-buffer))))
(skip-chars-backward "^\"")
(message "%s"
	 (setq whole-version
	   (buffer-substring (point)
			     (progn (skip-chars-forward "^\"") (point)))))


(write-region (point-min) (point-max) version-file nil 'nomsg)
(erase-buffer)
(set-buffer-modified-p nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(setq changelog-file (format "%sChangeLog" default-directory))

(insert-file-contents changelog-file)

(goto-char (point-min))
(insert
 "*******************************************************************************\n")
(insert
 (format "%s public release\n" whole-version))
(insert
 "*******************************************************************************\n\n")

(write-region (point-min) (point-max) changelog-file nil 'nomsg)
(erase-buffer)
(set-buffer-modified-p nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(kill-emacs)
