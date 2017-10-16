;;;; -*- coding: utf-8 -*- ;
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


(defparameter *features-to-initialize* (list :base :glisp :geom-base :gwl
					     :gwl-graphics :tasty :yadd :robot :cl-lite))

(defun start-gendl! (&key (features *features-to-initialize*)
		     (banner? t) (init-files? t))
  (dolist (feature features)
    ;;
    ;; This assumes feature name = package name of its initialize!
    ;; function.  This is the case now. Let's hope we can keep it that
    ;; way.
    ;;
    (let ((package feature))
      (if (and (find-package package) (glisp:featurep feature))
	  (let ((function-sym (read-from-string (format nil "~a::initialize" package))))
	    (when (fboundp function-sym)
	      (let ((description (glisp:package-documentation package)))
		(format t "~&Initializing ~a subsystem...~%" description)
		(let ((anything-changed? (funcall function-sym)))
		  (format t "~&...Done~a~%"
			  (if anything-changed? "."
			      (format nil " (no new global settings).")))))))
	  (format t "~&Note: Feature ~s does not appear to be loaded at this time.~%" feature))))
  (when banner? (startup-banner))
  (when init-files? (load-gdl-init-files) )
  (values))


(defun load-gdl-init-files ()
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


(defun quicklisp-copyright-string ()
  (when *already-loaded-systems*
    (let ((ql-libs 
	   (safe-sort 
	    (set-difference *already-loaded-systems*
			    (append (list "asdf" "crypt" "quicklisp" "base" "ent" "validate" "glisp" 
					  "monofasl" "pro" "enterprise")
				    *packages-to-lock*)
			    :key #'string :test #'string-equal) #'string-lessp))
	  (ql-version (with-open-file (in (or (probe-file (merge-pathnames "dists/quicklisp/distinfo.txt" *quicklisp-home*))
					      (error "Quicklisp directory not found in gendl:*quicklisp-home*, 
which is set to: ~a.~%" *quicklisp-home*)))
			(read-line in) (string-trim (list #\space) (second (glisp:split-regexp ":" (read-line in)))))))
      (format nil "Also contains the following Common Lisp libraries
from Quicklisp version ~a, whose source files are available in the
Quicklisp repository at http://quicklisp.org, Copyright© their
respective authors:

~{~a~^, ~}.~%" ql-version ql-libs))))


(defun startup-banner ()

  (format t
	  "

Gendl® Free Edition, version ~a
Within ~a ~a 
Copyright© 2017, Genworks International, Birmingham MI, USA. 
All Rights Reserved.

~a


Welcome to Gendl®
Copyright© 2017, Genworks® International, Birmingham MI, USA.
All Rights Reserved.

This program contains free software: you can redistribute it and/or
modify it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public
License along with the source code for this program. If not, see:

http://www.gnu.org/licenses/
"
	  gendl:*gendl-version*
	  (lisp-implementation-type)
	  (lisp-implementation-version)
	  (quicklisp-copyright-string)
	  ))


