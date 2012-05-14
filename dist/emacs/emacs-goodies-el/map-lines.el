;;; map-lines.el --- Map a command over many lines

;; Copyright (C) 2002  Andreas Fuchs <asf@void.at>
;; Copyright (C) 2010  Paul Hobbs <Paul_Hobbs@hmc.edu>

;; Author: Andreas Fuchs <asf@void.at>
;; Maintainer: Paul Hobbs <Paul_Hobbs@hmc.edu>
;; Keywords: matching, files

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

;; ------------------------------ TYPICAL USE ------------------------------
;; This module allows you to map a command over a set of lines
;; matching a regex.  The trick: You can then go ahead and insert these
;; lines in one clean yank.
;;
;; Example text:
;; 
;;     Hello,
;;     Here are the requested documents:
;;     a.txt
;;     b.txt
;;     c.txt
;;     Also, I have included the following:
;;     license.txt
;;
;; Running M-x map-lines-copy-regex ".txt" will give you
;;     a.txt
;;     b.txt
;;     c.txt
;;     license.txt
;;
;; This is also useful for using Emacs with UNIX: just run M-! ls, and filter
;; out the files you want to operate on using map-lines-kill, or grab those you
;; want using map-lines-copy.  Then, paste into a scratch buffer and use
;; keyboard macros and/or rectangles to form the commands you want to run on
;; each file, and to execute each command.  Nifty!

;; ------------------------------ INSTALLATION ------------------------------
;; To use this module, put this file somewhere in your load-path and this into
;; your .emacs:
;; (load-library "map-lines")
;; 
;; Alternatively, you can autoload the functions one at a time, which will
;; reduce your Emacs start-up time and typical RAM usage (slightly):
;;     (autoload 'map-lines "map-lines"
;;       "For each matching line, kill, copy or run a custom command" t)
;;     (autoload 'map-lines-kill "map-lines" "Kill each line matching regex" t)
;;     (autoload 'map-lines-copy "map-lines" "Copy each line matching regex" t)
;;     (autoload 'copy-line      "map-lines" "Copy the current line" t)
;;
;; You can set (recommended) keyboard shortcuts using
;;     (global-set-key (kbd "C-c m l") 'map-lines)
;;     (global-set-key (kbd "C-c m k") 'map-lines-kill)
;;     (global-set-key (kbd "C-c m c") 'map-lines-copy)
;;     (global-set-key (kbd "C-x c")
;;
;; ... or your own key combinations as you see fit.

;; ------------------------------ VERSIONS ------------------------------
;; This is version 0.2 of map-lines.el.
;;
;; You can find the latest version of this module in the debian package
;; emacs-goodies-el.  If you want to see new features, feel free to add them and
;; email the maintainer of this package.
;; 
;;; History:
;;
;; Version 0.2
;;  - Changed map-lines to always put a newline between each line, and added
;;    kill-lines and copy-lines.  (Paul Hobbs)
;;
;; Version 0.1
;;  - First version (Andreas Fuchs)

;;; Code:

(defvar mapl-command-alist
  '((?k . mapl-kill-line)
    (?c . mapl-copy-line)
    (?o . mapl-other-command))
  "An alist of command-char->command-name mappings.")


(defun mapl-lookup-command (command-char)
  "Return the matching command for COMMAND-CHAR."
  (let ((command (cdr (assq command-char mapl-command-alist))))
    (if (eq command 'mapl-other-command)
	(read-command "Other command (takes no args and returns a string): ")
      command)))

;;;###autoload
(defun map-lines (command-c regex)
  "Map a COMMAND-C (kill, copying, or a custom command) over lines matching REGEX."
  (interactive "cCommand (Kill, Copy, Other) [kco]:
sRegular Expression: ")
  (save-excursion
    (let ((command (mapl-lookup-command command-c))
	  (live-buffer (current-buffer)))
      (with-temp-buffer
	(let ((temp-buffer (current-buffer)))
	  (with-current-buffer live-buffer
	    (goto-char (point-min))
	    (while (re-search-forward regex nil t)
	      (let ((the-line (funcall command)))
	        (with-current-buffer temp-buffer
		  (insert the-line)
		  (newline)))
	      (end-of-line)))
	  (kill-region (point-min) (point-max)))))))

(defun mapl-kill-line ()
  "Kill a line entirely and return it."
  (mapl-kill-universal (lambda () (kill-line))))


;;;###autoload
(defun copy-line ()
  "Copy a whole line to the kill ring."
  (interactive)
  (let ((original-point (point)))
    (copy-region-as-kill (progn (beginning-of-line)
				(point))
			 (progn (end-of-line)
				(point)))
    (goto-char original-point)))

(defun mapl-copy-line ()
  "Copy a line entirely and return it."
  (mapl-kill-universal (lambda () (copy-line))))

(defun mapl-kill-universal (kill-fun)
  "Execute KILL-FUN on an entire line."
  (beginning-of-line)
  (funcall kill-fun)
  (prog1 (car kill-ring)
    (setq kill-ring (cdr kill-ring))))

;;;###autoload
(defun map-lines-kill (regex)
  "Kill all lines matching REGEX.  Yanking will insert all killed lines."
  (interactive "sRegular Expression: ")
  (map-lines ?\k regex))

;;;###autoload
(defun map-lines-copy (regex)
  "Copy all lines matching REGEX to the kill ring.  Yanking will insert all such lines."
  (interactive "sRegular Expression: ")
  (map-lines ?\c regex))

(provide 'map-lines)
;;; map-lines.el ends here
