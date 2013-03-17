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

;;
;; FLAG GLOBALS catalog these settings which affect compiling.
;;
(glisp:set-default-float-format)
(glisp:set-defpackage-behavior)

(defparameter *packages-to-initialize* (list :gdl :geom-base :gwl))


w(defun start-gendl! (&key (packages *packages-to-initialize*))
  (dolist (package packages)
    (when (find-package package) 
      (let ((function-sym (read-from-string (format nil "~a::initialize" package))))
	(when (fboundp function-sym) 
	  (let ((description (glisp:package-documentation package)))
	    (format t "~&Initializing ~a (~a) subsystem...~%~%" description package)
	    (let ((anything-changed? (funcall function-sym)))
	      (format t "~&...Generative ~a (~a) subsystem initializaton is complete.~a~%~%"
		      description package (if anything-changed? "" 
					      (format nil "~% (no new global settings).")))))))))
  (startup-banner) (load-gdl-init-files) (values))


(defun load-gdl-init-files (&key edition)
  (declare (ignore edition))
  (let* ((user-homedir (user-homedir-pathname))
         (current-directory (glisp:current-directory))
         (homedir-init-file (or (probe-file (merge-pathnames ".gdlinit.cl" user-homedir))
                                (probe-file (merge-pathnames "gdlinit.cl" user-homedir))))
         (current-init-file (when (not (equalp user-homedir current-directory))
                              (or (probe-file (merge-pathnames ".gdlinit.cl" current-directory))
                                  (probe-file (merge-pathnames "gdlinit.cl" current-directory))))))
    (let* ((command-args (glisp:basic-command-line-arguments))
           (homedir-init? (and (not (member "-q" command-args :test #'string-equal))
                               (not (member "-qq" command-args :test #'string-equal))))
           (current-dir-init? (not (member "-qq" command-args :test #'string-equal))))
      (when (and current-dir-init? current-init-file) (load current-init-file))
      (when (and homedir-init? homedir-init-file) (load homedir-init-file)))))



(defun startup-banner (&key edition)       
  (glisp:display-startup-banner edition
"
 Copyright 2013 Genworks International

 Welcome to Gendl(TM).

 This program contains free software: you can redistribute it
 and/or modify it under the terms of the GNU Affero General Public
 License as published by the Free Software Foundation, either
 version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public
 License along with the source code for this program. If not, see:

 http://www.gnu.org/licenses/
 
"))


;;
;; FLAG -- do some of these at beginning of bootstrapping, others
;; defer until end of loading everything.
;;
(defun initialize ()
  
  (declare (ignore edition))
  (setq glisp:*gdl-program-home* (glisp:executable-homedir-pathname))
  (setq glisp:*gdl-home* (make-pathname :name nil
					:type nil
					:directory (butlast (pathname-directory glisp:*gdl-program-home*))
					:defaults glisp:*gdl-program-home*))
  (glisp:set-genworks-source-home-if-known)
  (glisp:set-default-float-format)
  (glisp:set-defpackage-behavior)
  (glisp:set-default-package)
  (glisp:xref-off)
  (glisp:set-window-titles))

