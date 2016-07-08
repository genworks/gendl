;;
;; Copyright 2002-2012 Genworks International
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

(#+allegro
 excl:without-package-locks #-allegro progn
 (#+allegro
  excl:without-redefinition-warnings #-allegro progn
  (defun load-hotpatches (&key force? (directory (merge-pathnames "hotpatches/" (glisp:executable-homedir-pathname))))

    (if (not (and (probe-file directory)
		  (glisp:file-directory-p directory)))
	(warn "~s does not exist and/or is not a directory~%" directory)
	(let* ((files (glisp:directory-list directory))
	       (source-files (sort (remove-if-not #'(lambda(file) 
						      (let ((type (pathname-type file)))
							(or (string-equal type "lisp")
							    (string-equal type "gdl")
							    (string-equal type "gendl")))) files)
				   #'string-lessp :key #'namestring)))
	  (when (or (not (glisp:featurep :allegro))
		    (glisp:featurep :compiler))
	    (dolist (file source-files)
	      (with-error-handling ()
		(format t "~&Compiling patch file ~a...~%" file)
		(#+allegro excl:compile-file-if-needed #-allegro compile-file file))))

	  (setq files (remove-if #'(lambda(file)(string-equal (pathname-name file) "load")) (glisp:directory-list directory)))

	  (let ((compiled-files (sort (remove-if-not #'(lambda(file)
							 (string-equal (pathname-type file) glisp:*fasl-extension*)) files)
				      #'string-lessp :key #'namestring)))

	    (dolist (file compiled-files)
	      (unless (and (not force?) (find file *loaded-hotpatches* :test #'equalp))
		(with-error-handling ()
		  (format t "~&Loading patch file ~a...~%" file)
		  (load file)
		  (pushnew file *loaded-hotpatches* :test #'equalp))))))))))


(load-hotpatches :directory (make-pathname :directory (pathname-directory *load-truename*) :defaults nil) :force? t)
