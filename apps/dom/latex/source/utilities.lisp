;;
;; Copyright 2002, 2009, 2012 Genworks International
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;
;; This source file contains free software: you can redistribute it
;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This source file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with this source file.  If not, see
;; <http://www.gnu.org/licenses/>.
;;


(in-package :com.genworks.dom-latex)

(defun remove-keys (keys plist)
  (let ((list (copy-list plist)))
    (dolist (key keys list) (remf list key))))



#+nil
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
    (replace-substring
     (replace-substring 
      (replace-substring
       (replace-substring string "#" "\\#") "$" "\\$") "_" "\\_") "^" "hat-") "&" "\\&") "%" "\\%"))

