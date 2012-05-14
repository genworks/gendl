;;; align-string.el --- align string components over several lines

;; Copyright (c) 2001 Markus Bjartveit Krüger

;; Author:   Markus Bjartveit Krüger <markusk@pvv.org>
;; Created:  20-Sep-2001
;; Version:  0.1
;; Keywords: convenience
;; X-URL:    http://www.pvv.org/~markusk/align-string.el

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.  This is distributed in the hope that it will be
;; useful, but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose.  See the
;; GNU General Public License for more details.  You should have
;; received a copy of the GNU General Public License along with GNU
;; Emacs; see the file `COPYING'.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Code:

;;;###autoload
(defun align-string (begin end regexp count)
  "Align first occurrence of REGEXP in each line of region.
If given a prefix argument, align occurrence number COUNT on each line."
  (interactive "r
sAlign by: 
p")
  (undo-boundary)
  (save-excursion
    ;; Move begin point to start of line.
    (goto-char begin)
    (setq begin (line-beginning-position))
    ;; Make end a marker, to track updates made in buffer.  Point the
    ;; marker at the end of the last line, unless at start of line.
    (goto-char end)
    (setq end (set-marker (make-marker) (if (bolp) 
					    (point)
					  (line-end-position))))
    (let ((max-col 0))
      ;; Find max column of first occurrence of string in the lines
      ;; bounded by begin-marker and end-marker
      (goto-char begin)
      (while (< (point) end)
	(when (re-search-forward regexp (line-end-position) t count)
	  (goto-char (match-beginning 0))
	  (setq max-col (max max-col (current-column))))
	(beginning-of-line 2))
      ;; For each line in region, indent first occurrence of string
      ;; to max column.
      (goto-char begin)
      (while (< (point) end)
	(when (re-search-forward regexp (line-end-position) t count)
	  (goto-char (match-beginning 0))
	  (indent-to max-col))
	(beginning-of-line 2)))
    ;; Clear end marker.
    (set-marker end nil)))

;;;###autoload
(defun align-all-strings (begin end regexp)
  "Align all occurrences of REGEXP in each line of region.
That is to say, align the first occurrence of each line with each other,
align the second occurence of each line with each other, and so on."
  (interactive "r
sAlign by: ")
  (save-excursion
    ;; Move begin point to start of line.
    (goto-char begin)
    (setq begin (line-beginning-position))
    ;; Make end a marker, to track updates made in buffer.  Point the
    ;; marker at the end of the last line.
    (goto-char end)
    (setq end (set-marker (make-marker) (if (bolp)
					    (point)
					  (line-end-position))))
    ;; Count max number of occurrences in any line in region, then
    ;; run align-string for each enumerated occurrence.
    (let ((max-occs 0))
      (goto-char begin)
      (while (< (point) end)
	(let ((occs-this-line 0)
	      (line-end (line-end-position)))
	  (while (and (< (point) line-end)
		      (re-search-forward regexp line-end t))
	    (setq occs-this-line (1+ occs-this-line))
	    ; Ensure that search moves forward even if match is empty
	    (when (= (match-beginning 0) (point))
	      (forward-char)))
	  (setq max-occs (max max-occs occs-this-line)))
	(beginning-of-line 2))
      (let ((i 1))
	(while (<= i max-occs)
	  (align-string begin end regexp i)
	  (setq i (1+ i)))))
    ;; Clear end marker.
    (set-marker end nil)))

(provide 'align-string)
