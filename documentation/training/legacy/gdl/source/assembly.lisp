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

(in-package :gdl-tutorial)

(defparameter *data*
    `((:document :class ("11pt" "book")
		 :textwidth 6.5
		 :topmargin 0
		 :textheight 8.5
		 :oddsidemargin 0
		 :evensidemargin 0
		 :pdfimageresolution "135"
		 :title "Genworks GDL Tutorial"
		 :author "David J. Cooper, Jr."
		 :usepackage ("dvips" "graphicx")
		 :usepackage ("usenames, dvipsnames" "color")
		 :usepackage ("makeidx")
		 :newsavebox ("\\boxedverb")
		 :makeindex nil)
      :frontmatter
      :maketitle
      (:footnotetext "Copyright "
		     (:copyright)
		     " 2002, Genworks International. Duplication, by any means, in whole or in part, requires 
written consent from Genworks International.")
      :tableofcontents
      :mainmatter
      ,*introduction*
      ,*gdl-syntax*
      ,*gwl-syntax*
      ,*example-1*  ;; nongeometric
      ,*example-2*  ;; robot
      ,*example-3*  ;; school bus
      :backmatter
      :printindex
      ))



(defun make (&optional (level 1))
  (load "~/gendl/documentation/training/legacy/gdl/source/assembly.lisp")
  
  (let ((object (make-part 'com.genworks.dom:assembly :data *data*)))
    (ensure-directories-exist (translate-logical-pathname "~/gendl/documentation/training/legacy/gdl/pdf/"))
    (with-open-file (out "~/gendl/documentation/training/legacy/gdl/pdf/tutorial.tex"
		     :direction :output :if-exists :supersede :if-does-not-exist :create)
      (with-format (com.genworks.dom-writers:latex out)
	(write-the-object object (:base)))))
  
  (excl:run-shell-command 
   (format nil "cd ~a; pdflatex tutorial.tex"
	   (translate-logical-pathname "~/gendl/documentation/training/legacy/gdl/pdf/")))
  
  (excl:run-shell-command 
   (format nil "cd ~a; makeindex tutorial"
	   (translate-logical-pathname "~/gendl/documentation/training/legacy/gdl/pdf/")))
  
  (dotimes (n level)
    (excl:run-shell-command 
     (format nil "cd ~a; pdflatex tutorial.tex"
	     (translate-logical-pathname "~/gendl/documentation/training/legacy/gdl/pdf/")))))

(define-object assembly (com.genworks.dom:assembly)
  :input-slots ((data *data*)))



(publish :path "/dom-doc"
	 :function #'(lambda(req ent)
		       (gwl-make-object req ent "gdl-tutorial:assembly")))
