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


(defun make (&optional (level 1))
  (initialize-data)
  (let ((pdf-path (merge-pathnames "pdf/" *system-home*)))

    (load (merge-pathnames "../source/assembly.lisp" pdf-path))

    (let ((object (make-part 'com.genworks.dom:assembly :data *data*)))
      (ensure-directories-exist (translate-logical-pathname pdf-path))
      (with-format (com.genworks.dom-writers:latex (merge-pathnames "pdf/tutorial.tex" *system-home*))
	(write-the-object object (:base))))
  
    ;;
    ;; FLAG - replace this asdf:run-shell-command with glisp:run-shell-command or uiop:run-program. 
    ;;

    (asdf:run-shell-command 
     ;;(format nil "cd ~a; /opt/local/bin/pdflatex -interaction=nonstopmode tutorial.tex" pdf-path)
     (format nil "cd ~a; /usr/texbin/pdflatex -interaction=nonstopmode tutorial.tex" pdf-path)
     )
  
  (asdf:run-shell-command 
   (format nil "cd ~a; /usr/texbin/makeindex tutorial" pdf-path))
  
  (dotimes (n level)
    (asdf:run-shell-command 
     (format nil "cd ~a; /usr/texbin/pdflatex -interaction=nonstopmode tutorial.tex" pdf-path)))))


(define-object assembly (com.genworks.dom:assembly)
  :input-slots ((data *data*)))


(publish :path "/dom-doc"
	 :function #'(lambda(req ent)
		       (gwl-make-object req ent "gendl-doc:assembly")))
