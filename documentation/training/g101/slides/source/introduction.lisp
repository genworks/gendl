;;
;; Copyright 2002, 2009 Genworks International and Genworks BV 
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

(in-package :training-g101)

(define-object introduction (slide-show-leaf)
  :computed-slots
  ((strings-for-display "Introduction")
   (slide-data 
    `(      
      (:title 
       "My Background"
       :bullet-points
       ((:description
	 "1997-present: Approaching 13 years running Genworks, a KBE tools vendor")
	(:description
	 "1993-1997: at Ford Motor Company, building and administering KBE applications with ICAD Design Language")
	(:description 
	 ,(string-append 
	  "1983-1993:"
	  (with-output-to-string (ss)
	     (html-stream 
	      ss
	      (:ul
	       (:li "Master in Computer Science, U of Michigan")
	       (:li "Work Experience in Database Industry (Quantum/Progress)")
	       (:li "Work Experience in CAD Industry (Applicon)")
	       (:li "Bachelors in Computer Science and German, U of Michigan"))))))))
      
      (:title 
       "CL Timeline - Past"
       :bullet-points
       ((:description 
	 "Conceived in 1958")
	(:description 
	 "Professor John McCarthy")
	(:description 
	 "2nd oldest high-level computer language still in use")
	(:description 
	 "Still on the leading edge")
	(:description 
	 "<b>Designed to evolve</b>")))
      
      
      (:title 
       "One of the most popular extension languages:"
       :bullet-points
       ((:description "The Genworks GDL System" :image-url "smiley.gif")
	(:description "Gnu Emacs")
	(:description "The ICAD System")
	(:description "AutoCAD (AutoLisp, VisualLisp)")
	(:description "DesignPower Design++")
	(:description "Game development (Nichimen Graphics, Mirai, Naughty Dog Software)")
	(:description "Sawfish window manager")))

      (:title 
       "The Original RAD Environment"
       :bullet-points
       ((:description 
	 "<b><u>R</u></b>apid <b><u>A</u></b>pplication <b><u>D</u></b>evelopment")
	(:description "Write a prototype faster <b>than the spec</b>")
	(:description "Prototype is a better spec <b>than a paper spec</b>")
	(:description "Fine-tune the resulting prototype into a production application")))

      
      (:title 
       "Lisp Thrives on Complex Problems"
       :bullet-points
       ((:description
	 "You can't completely specify something if you have never solved something
like it before - you have to experiment and prototype")
	(:description
	 "The Lisp environment is well-suited to supporting exploratory programming")
	(:description
	 "The Lisp environment is also well-suited to supporting VERY large
programs in a workable manner (e.g. the GDL System)")))))))
