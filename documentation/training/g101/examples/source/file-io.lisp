;;
;; Copyright 2002-2011, 2012 Genworks International
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

(in-package :gdl-user)

(defun file-size (pathname)
  (with-open-file (in pathname)
    (do ((line (read-line in nil nil) (read-line in nil nil)))
	((null line)(file-position in)))))

(defun read-large (pathname starting-at &key from-end?)
  "List of strings. Returns a list with one string for each line in ascii file,
starting from position given by starting-at."
  (with-open-file (in pathname)
    (file-position in (if from-end? (- (file-length in) starting-at)
			starting-at))
    (let (result)
      (do ((line (read-line in nil nil)(read-line in nil nil)))
	  ((null line) (nreverse result))
	(push line result)))))
