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

(defparameter *data*
    `((:document :class ("11pt" "book")
		 :textwidth 6.5
		 :topmargin 0
		 :textheight 8.5
		 :oddsidemargin 0
		 :evensidemargin 0
		 :pdfimageresolution "135"
		 :title "GenDL Unified Documentation"
		 :author "Dave Cooper"
		 :usepackage ("dvips" "graphicx")
		 :usepackage ("usenames, dvipsnames" "color")
		 :usepackage ("makeidx")
		 ;;:usepackage ("pdfborder={0 0 0}" "hyperref")
		 :usepackage ("colorlinks=true, urlcolor=cyan" "hyperref")
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
      :backmatter
      :printindex
      ))


(defun make (&optional (level 1))
  (let ((pdf-path (merge-pathnames "pdf/" *system-home*)))

    (load (merge-pathnames "../source/assembly.lisp" pdf-path))

    (let ((object (make-part 'com.genworks.dom:assembly :data *data*)))
      (ensure-directories-exist (translate-logical-pathname pdf-path))
      (with-open-file (out (merge-pathnames "pdf/tutorial.tex" *system-home*)
			   :direction :output :if-exists :supersede :if-does-not-exist :create)
	(with-format (com.genworks.dom-writers:latex out)
	  (write-the-object object (:base)))))
  
    (glisp:run-shell-command 
     (format nil "cd ~a; pdflatex tutorial.tex" pdf-path))
  
  (glisp:run-shell-command 
   (format nil "cd ~a; makeindex tutorial" pdf-path))
  
  (dotimes (n level)
    (glisp:run-shell-command 
     (format nil "cd ~a; pdflatex tutorial.tex" pdf-path)))))


(define-object assembly (com.genworks.dom:assembly)
  :input-slots ((data *data*)))


(publish :path "/dom-doc"
	 :function #'(lambda(req ent)
		       (gwl-make-object req ent "gendl-doc:assembly")))
