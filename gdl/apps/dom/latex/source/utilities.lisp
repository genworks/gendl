;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: com.genworks.dom-latex; Base: 10 -*-

(in-package :com.genworks.dom-latex)

(defun remove-keys (keys plist)
  (let ((list (copy-list plist)))
    (dolist (key keys list) (remf list key))))



(defun replace-substring (string old new)
  (let ((position (search old string)))
    (if position
	(string-append 
	 (subseq string 0 position) new
	 (replace-substring (subseq string (+ position (length old))) old new))
      string)))

(defun escape-string (string)
  (replace-substring 
   (replace-substring
    (replace-substring string "#" "\\#") "$" "\\$") "_" "\\_"))
