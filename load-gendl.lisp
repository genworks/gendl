(cl:in-package :cl-user)

;;                          LOAD-GENDL.LISP
;;           Nick Levine, Ravenbrook Limited, 2013-05-17
;;
;; 1.  INTRODUCTION
;;
;; The purpose of this document is to oversee the loading of GENDL.
;;
;; This will typically be in a SLIME environment. The function
;; gdl-welcome in gdl.el inserts a cl:load of this file into the new
;; *slime-repl* buffer. Quicklisp won't be present yet but this file
;; takes care of that.
;;
;; If you're outside the SLIME environment, you can still load this
;; file. It's up to you to load quicklisp beforehand. First time you
;; load the GENDL system, it will write a configuration file
;; ("configure.el" in this directory) which will tell future
;; invocations where your lisp image and quicklisp installation are.
;; That sets you up for future loads via SLIME.


;; 2.  LOAD GENDL AND DEPENDENCIES; START GENDL RUNNING.

(let* ((this-file *load-pathname*)
       (gendl (make-pathname :name nil :type nil :defaults this-file)))

  ;; 1. Locate and load quicklisp - without this we can't load the
  ;; system or its dependencies. But don't try to reload quicklisp
  ;; if it's already loaded.
  ;;
  ;; Note that if this is the first time load-gendl.lisp has been
  ;; loaded then "configure.el" won't be found.
  #-quicklisp
  (let ((configure-file (merge-pathnames "configure.el" gendl)))
    (when (probe-file configure-file)
      (let (lisp-invocation
            (path-to-quicklisp-helper nil))
        (declare (special lisp-invocation path-to-quicklisp-helper))
        (load configure-file)
        (if path-to-quicklisp-helper
            (let ((path-to-slime-setup (merge-pathnames "setup.lisp" path-to-quicklisp-helper)))
              (if (probe-file path-to-slime-setup)
                  ;; Load quicklisp.
                  (load path-to-slime-setup)
                (error "\"~a\" not found. Visit \"~a\" and reset path-to-quicklisp-helper."
                       path-to-slime-setup configure-file)))
          (error "~s not set in \"~a\"."
                 'path-to-quicklisp-helper configure-file)))))


  ;; 2. If we were redistributing all our dependencies, this form
  ;; would tell asdf where to find them.

  #+#:nil (asdf:initialize-source-registry `(:source-registry (:tree ,gendl :inherit-configuration)))

  ;; 3. Load gendl plus dependencies.

  (funcall (find-symbol "QUICKLOAD" "QL") :gendl)

  ;; 4. Load the slime integration. Ideally this should be done
  ;; (conditionally) by the quickload above.

  #+:swank
  (load (compile-file (merge-pathnames "emacs/gendl.lisp" gendl)))

  ;; 5. Start gendl.

  (let ((starter (find-symbol "START-GENDL!" "GENDL")))
    (funcall starter)))




;; A.  REFERENCES
;;
;;
;; B.  HISTORY
;;
;; 2013-05-17 NDL Created.
;;
;;
;; C.  COPYRIGHT
;;
;; Copyright 2013 Genworks International
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


