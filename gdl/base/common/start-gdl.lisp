;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
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


;;(pushnew #'load-gdl-init-files *gdl-init-functions*)


(defun startup-banner (&key edition)       
  (glisp:display-startup-banner edition
"


 Copyright 2002-2011 Genworks International and Genworks BV 

 This is the General-purpose Declarative Language (GDL).

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



(pushnew #'startup-banner *gdl-init-functions*)

;;
;; FLAG -- do some of these at beginning of bootstrapping, others
;; defer until end of loading everything.
;;

(defun initialize-gdl (&key edition)
  (declare (ignore edition))
  (glisp:set-defpackage-behavior)
  (glisp:set-default-float-format)
  (glisp:set-default-package)
  (glisp:xref-off)
  (glisp:set-window-titles))


(pushnew #'initialize-gdl *gdl-init-functions*)

(glisp:set-default-float-format)
(glisp:set-defpackage-behavior)
