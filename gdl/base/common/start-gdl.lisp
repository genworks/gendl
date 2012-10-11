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


(defparameter *gdl-init-functions* nil)

(defun start-gdl (&key edition)
  (mapc #'(lambda(function)
            (funcall function :edition edition))
        (reverse *gdl-init-functions*))
  
  (startup-banner)
  (load-gdl-init-files)

  (values))

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


 Copyright 2002-2011 Genworks International

 This is the General-purpose Declarative Language (GenDL).

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

 -----

 Note that you may have received Gendl under supplemental licensing
 terms, e.g. Proprietary or Trial/Evaluation. The terms of such a
 dual-license, and information about whether they do or do not
 override the AGPL, were printed at the initial startup of your
 session. Please contact Genworks if you have any uncertainty about
 licensing terms beyond those in the AGPL.

"))


;;
;; FLAG -- do some of these at beginning of bootstrapping, others
;; defer until end of loading everything.
;;

(defun initialize-gdl (&key edition)
  (declare (ignore edition))
  
  ;;
  ;; FLAG -- pull asdf-specific code into glisp layer
  ;;
  (multiple-value-bind (value error)
      (ignore-errors (asdf:system-definition-pathname :gdl-base))
    (declare (ignore value))
    (when error
      (warn "

asdf has system definitions for non-existent source directories. 

You should probably do: 

  (asdf:map-systems #'asdf:clear-system)

And really this should be done in the post-load-form of your build."))
    
    (if error
	(setq glisp:*genworks-source-home* nil)
	(setq glisp:*genworks-source-home* 
	      (when (asdf:find-system :gdl-base nil)
		(let ((gdl-base-home (glisp:system-home :gdl-base)))
		  (make-pathname :name nil
				 :type nil
				 :directory (butlast 
					     (butlast (pathname-directory gdl-base-home)))
				 :defaults gdl-base-home))))))
  
  (setq glisp:*gdl-program-home* (glisp:executable-homedir-pathname))

  (setq glisp:*gdl-home* (make-pathname :name nil
					:type nil
					:directory (butlast (pathname-directory glisp:*gdl-program-home*))
					:defaults glisp:*gdl-program-home*))

  (glisp:set-default-float-format)
  (glisp:set-defpackage-behavior)

  ;;
  ;; FLAG -- don't do this here. Don't want to mess up people's
  ;; Quicklisp environment if they are just loading Gendl from within
  ;; an existing Quicklisp environment. 
  ;;
  ;; Furthermore it is not allowed by Quicklisp to have libraries
  ;; which depend on Quicklisp itself.
  ;;
  ;; Only worry about this in proprietary builds, handled from the
  ;; internal Genworks codebase.
  ;;
  ;;(setq ql:*quicklisp-home* (merge-pathnames "quicklisp/" glisp:*gdl-home*))
  ;;
  
  (asdf:initialize-output-translations)

  (glisp:set-default-package)
  (glisp:xref-off)
  (glisp:set-window-titles))


(pushnew #'initialize-gdl *gdl-init-functions*)

(glisp:set-default-float-format)
(glisp:set-defpackage-behavior)
