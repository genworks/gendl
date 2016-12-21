;;
;; Copyright 2002, 2009 Genworks International
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
		  (with-output-to-string(ss)(html-stream ss ((:a :href "http://www.common-lisp.net") "Common Lisp.net")))
		  ",  "
		  
		  ", and the "
		  (with-output-to-string(ss)(html-stream ss ((:a :href "http://www.lispworks.com/reference/HyperSpec/Front/index.htm") "Common Lisp Hyperspec")))
		  " for the complete official specification of the language." ))
	       
	       (:description
		,(string-append
		  "For creating complete and well-rounded CL-based applications, especially web-based and 
technical/engineering applications, please consider using the open-source "
		  (with-output-to-string(ss)(html-stream ss ((:a :href "http://gendl.org") "Gendl Project")))
		  " or the commercial Genworks "
		(with-output-to-string(ss)(html-stream ss ((:a :href "http://www.genworks.com/products/index.html")
							   "General-purpose Declarative Language (GDL)")))
		"."))
	       
	       (:description "Genworks offers G102, as a followup to this course, for quickly coming up to speed in GDL.")
	       (:description "Thank you for your attention, and please do not hesitate to voice questions and 
comments on this training course. Ask your instructor or send them to info at genworks dot com.")))
      
      
      
      ))))


