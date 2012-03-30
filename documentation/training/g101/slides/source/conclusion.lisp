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

(define-object conclusion (slide-show-leaf)
  
  :computed-slots
  ((strings-for-display "Conclusion")
   (:slide-data 
    `((:title "Congratulations"
	      :bullet-points
	      ((:description
		"If you have understood most of these slides and completed most of the exercises, Common Lisp
should now be more familiar and less of a mystery.")
	       (:description
		,(string-append
		  "For further resources, see the "
		  (with-output-to-string(ss)(html-stream ss ((:a :href "http://www.alu.org/alu/res-lisp") "Association of Lisp Users")))
		  " and the "
		  (with-output-to-string(ss)(html-stream ss ((:a :href "http://www.lispworks.com/reference/HyperSpec/Front/index.htm") "Common Lisp Hyperspec")))
		  " for the complete official specification of the language." ))
	       (:description
		,(string-append
		  "For creating complete and well-rounded CL-based applications, especially web-based and 
technical/engineering applications, please consider using Genworks "
		(with-output-to-string(ss)(html-stream ss ((:a :href "http://www.genworks.com/products/index.html")
							   "General-purpose Declarative Language (GDL)")))
		"."))
	       (:description "Genworks offers G102, as a followup to this course, for quickly coming up to speed in GDL.")
	       (:description "Thank you for your attention, and please do not hesitate to voice questions and comments on this training course. Ask your instructor
or send them to info at genworks dot com.")))
      
      
      
      (:title "Licensing"
	      :bullet-points
	      ((:description
		"Moving toward AGPL for GDL source code.")
	       
	       (:description
		"Certain files already available, working from the outside inward.")
	       
	       (:description
		"AGPL requires all derived works (for GDL, this includes applications delivered with GDL runtime) to be released 
open-source under compatible license.")
	       
	       (:description
		"Dual licensing approach - Genworks will continue to make Commercial GDL licenses available
 for supported or proprietary work, and containing commercial Common Lisp and geometry kernels.")
	       
	       
	       (:description
		"Currently released package is _not_ open-source (depends on commercial Lisp compiler and runtime 
and optional geometry kernel) and issued to individuals based on email address. Please keep your individual 
license file protected.")
	       
	       (:description
		"Limited-time Trial Editions made available for following exercises in this course.")
	       
	       (:description
		"TU Delft currently has 25 Student Licenses (350 Euros per year) -- please apply for one of 
these if you use GDL past the Trial period.")
	       
	       
	       (:description
		"Student and Trial licenses may _not_ be used for development of funded research 
projects (Academic licenses are available for that purpose).")
	       
	       
	       ))
      
      
      
      ))))


