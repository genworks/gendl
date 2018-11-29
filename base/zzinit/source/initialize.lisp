;;
;; Copyright 2002-2011 Genworks International
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

(in-package :gdl)


(defun initialize ()
  
  (setq glisp:*gdl-program-home* (probe-file (glisp:executable-homedir-pathname)))
  (setq glisp:*gdl-home* (make-pathname :name nil
					:type nil
					:directory (butlast (pathname-directory glisp:*gdl-program-home*))
					:defaults glisp:*gdl-program-home*))
  (setq glisp:*gendl-home* glisp:*gdl-home*)

  (when (find-package :asdf) (funcall (read-from-string "asdf:initialize-output-translations")))
  
  (setq *quicklisp-home* (or (when (and (find-package :ql) (boundp (read-from-string "ql:*quicklisp-home*"))
					(probe-file (symbol-value (read-from-string "ql:*quicklisp-home*"))))
			       (symbol-value (read-from-string "ql:*quicklisp-home*")))
			     (probe-file (merge-pathnames "quicklisp/" glisp:*gendl-home*))
			     (probe-file (merge-pathnames "genworks/quicklisp/" glisp:*gendl-home*))
			     (probe-file (merge-pathnames "quicklisp/" glisp:*genworks-source-home*))
			     (probe-file (merge-pathnames "genworks/quicklisp/dists/quicklisp/distinfo.txt"
							  glisp:*gdl-home*))
			     ))

  (when (and (find-package :ql) (boundp (read-from-string "ql:*quicklisp-home*"))
	     (not (probe-file (symbol-value (read-from-string "ql:*quicklisp-home*")))))
    (setf (symbol-value (read-from-string "ql:*quicklisp-home*")) *quicklisp-home*))
  
  (pushnew (make-keyword (format nil "gendl-~a" *gendl-version*)) *features*)
  (glisp:set-genworks-source-home-if-known)
  (glisp:set-default-float-format)
  (glisp:set-defpackage-behavior)
  (glisp:set-default-package)
  (glisp:set-window-titles))

