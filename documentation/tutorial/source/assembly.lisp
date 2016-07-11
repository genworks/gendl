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

(in-package :gendl-doc)

(defparameter *data* nil)


(defun initialize-data ()
  (setq *data*
	`((:document :class ("11pt" "book")
		     :textwidth 6.5
		     :topmargin 0
		     :textheight 8.5
		     :oddsidemargin 0
		     :evensidemargin 0
		     :pdfimageresolution "135"
		     :usepackage ("datetime")
		     :title "Genworks GDL: A User's Manual"
		     :date ("\\monthname\\ \\the\\year")
		     :author "Dave Cooper"
		     :usepackage ("dvips" "graphicx")
		     :usepackage ("usenames, dvipsnames" "color")
		     :usepackage ("makeidx")
		     :usepackage ("textcomp")
		     :usepackage ("colorlinks=true, urlcolor=cyan" "hyperref")
		     :usepackage ("nottoc, numbib" "tocbibind")
		     :newsavebox ("\\boxedverb")
		     :makeindex nil)
	  :frontmatter
	  :maketitle
	  (:footnotetext "Copyright "
			 (:copyright)
			 " 2012, Genworks International. Duplication, by any means, in whole or in part, requires 
written consent from Genworks International.")
	  :tableofcontents
	  :mainmatter
	  ,*introduction*
	  ,*installation*
	  ,*basic-operation*
	  ,*understanding-common-lisp*
	  ,*understanding-gendl*
	  ,*tasty-environment*
	  ,*gendl-geometry*
	  ,*custom-user-interfaces*
	  ,*advanced-common-lisp*
	  ,*advanced-gendl*
	  ,*upgrade-notes*
	  ,(the-object (make-self 'yadd::assy) dom-chapter)
	  :backmatter
	  ,*bibliography*      
	  :printindex
	  )))



(defmacro run-prog (command)
  `(multiple-value-bind (output error return-code)
       (uiop:run-program ,command :output :string :error :string :ignore-error-status t)
     (when (or error (not (zerop return-code)))
       (format t "Command was: ~a~%" ,command)
       (warn "Error was ~a, ~a and return-code was ~a" output error return-code))))

(defun make (&optional (level 1))

  (let ((pdf-path (merge-pathnames "pdf/" *system-home*))

	(pdftex-path (cond ((probe-file "/usr/texbin/pdflatex")
			    "/usr/texbin/pdflatex")
			   ((probe-file "/opt/local/bin/pdflatex")
			    "/opt/local/bin/pdflatex")
			   ((probe-file "/Library/TeX/texbin/pdflatex")
			    "/Library/TeX/texbin/pdflatex")))
	(makeindex-path (or (probe-file "/usr/texbin/makeindex")
			    (probe-file "/opt/local/bin/makeindex")
			    (probe-file "/Library/TeX/texbin/makeindex"))))
			    


    (load (merge-pathnames "../source/assembly.lisp" pdf-path))

    (initialize-data)

    (let ((object (make-part 'com.genworks.dom:assembly :data *data*)))
      (ensure-directories-exist (translate-logical-pathname pdf-path))
      (with-format (com.genworks.dom-writers:latex (merge-pathnames "pdf/tutorial.tex" *system-home*))
	(write-the-object object (:base))))
    
    (run-prog (format nil "cd ~a; ~a -interaction=nonstopmode tutorial.tex"  pdf-path pdftex-path))
    (run-prog (format nil "cd ~a; ~a tutorial" pdf-path makeindex-path))

    (dotimes (n level)
      (run-prog (format nil "cd ~a; ~a -interaction=nonstopmode tutorial.tex" 
			pdf-path pdftex-path)))))

(define-object assembly (com.genworks.dom:assembly)
  :input-slots ((data *data*)
		(style-url "tutorial.css")))


(publish :path "/dom-doc"
	 :function #'(lambda(req ent)
		       (gwl-make-object req ent "gendl-doc:assembly")))


(defun make-html (&key (output-directory (merge-pathnames "tmp/" (user-homedir-pathname)))
		  (output-file-namestring "tutorial.html"))
  (ensure-directories-exist output-directory) 
  (initialize-data)
  (let ((self (make-object 'assembly)))
    (let ((output-path (merge-pathnames output-file-namestring output-directory)))
      (with-format (html-format output-path) (write-the cl-who-out))
      output-path)))


(defun make-css (&key (output-directory (merge-pathnames "tmp/" (user-homedir-pathname)))
		 (output-file-namestring "tutorial.css"))
  (let ((output-path (merge-pathnames output-file-namestring output-directory)) 
	(css-string 
	 (reduce 
	  (lambda (x y) 
	    (concatenate 'string x y))
	  (mapcar 
	   (lambda (style)
	     (format nil "~a{~a}" 
		     (car style)
		     (format nil "~{~a~}" 
			     (mapcar (lambda (pair)
				       (format nil "~a:~a;" 
					       (car pair)
					       (cadr pair))) 
				     (cdr style)))))
	   *styles*))))

    (with-open-file (stream output-path 
			    :direction :output
			    :if-exists :overwrite
			    :if-does-not-exist :create)
      (format stream css-string)
      output-path)))

      
(defun make-html-and-css (&key (output-directory (merge-pathnames "tmp/" (user-homedir-pathname)))
		 (html-output-file-namestring "tutorial.html")
		 (css-output-file-namestring "tutorial.css")) 
  (make-html :output-directory output-directory 
	     :output-file-namestring html-output-file-namestring)
  (make-css :output-directory output-directory 
	    :output-file-namestring css-output-file-namestring))
