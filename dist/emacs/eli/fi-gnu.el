;; Stuff grabbed and modified from the GNU Emacs sources.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright (C) 1985, 1986, 1987, 1992, 1993, 1994, 1995, 1996,
;;   1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007 Free Software Foundation, Inc.

;; Maintainer: FSF

;; This file is part of GNU Emacs.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Trimmed down version of find-backup-file-name from lisp/files.el.
(defun fi::find-most-recent-backup-file-name (fn)
  (if (eq version-control 'never)
      (make-backup-file-name fn)
    (let* ((basic-name (make-backup-file-name-1 fn))
	   (base-versions (concat (file-name-nondirectory basic-name) ".~"))
	   (backup-extract-version-start (length base-versions))
	   (high-water-mark 0)
	   (number-to-delete 0)
	   possibilities deserve-versions-p versions)
      (condition-case ()
	  (setq possibilities (file-name-all-completions
			       base-versions
			       (file-name-directory basic-name))
		versions (sort (mapcar #'backup-extract-version
				       possibilities)
			       #'<)
		high-water-mark (apply 'max 0 versions)
		deserve-versions-p (or version-control
				       (> high-water-mark 0))
		number-to-delete (- (length versions)
				    kept-old-versions
				    kept-new-versions
				    -1))
	(file-error (setq possibilities nil)))
      (list (if (not deserve-versions-p)
		(make-backup-file-name fn)
	      (format "%s.~%d~" basic-name high-water-mark))))))
