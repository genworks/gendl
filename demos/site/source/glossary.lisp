;;
;; Copyright 2002-2011, 2012 Genworks International
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

(in-package :genworks.com)


(define-object glossary (base-site-sheet)

  :input-slots ((dt (the glossary-gdl dt))
		(dd (the glossary-gdl dd))
		(column-left (the column-left%)))
  
  :computed-slots ((body-class "products glossary"))
		   
  :objects
  ((column-left% 
    :type 'sheet-section
    :inner-html (with-cl-who-string (:indent t)
		 (:h3 "Glossary")
		 (:ul (:li ((:a :href (the glossary-gdl url) :title "GDL") "GDL"))
		      (:li ((:a :href (the glossary-declarative url) :title "Declarative") "Declarative"))
		      (:li ((:a :href (the glossary-knowledge url) :title "Knowledge") "Knowledge"))
		      (:li ((:a :href (the glossary-kbe url) :title "KBE") "KBE"))
		      (:li ((:a :href (the glossary-object url) :title "Object-Oriented Language") "Object-Oriented Language"))
		      (:li ((:a :href (the glossary-web url) :title "Web-Based") "Web-Based"))
		      (:li ((:a :href (the glossary-caching url) :title "Caching") "Caching"))
		      (:li ((:a :href (the glossary-tracking url) :title "Dependency Tracking") "Dependency Tracking")))))
   
   (column-center :type 'sheet-section
		  :inner-html (with-cl-who-string (:indent t)
			       ((:div :class "column_center") 
				(:h2 "Technical Terms") 
				(:h3 "(In Plain English)")
				(:dl (:dt (str (the dt)))
				     (:dd (str (the dd)))))))
   
   (glossary-gdl :type 'glossary
		 :dt "GDL"
		 :dd "is an acronym for the Genworks core language. 
The initials stand for:  General-Purpose, Declarative, Language. By General-Purpose it 
is meant that the language may be used to create a wide spectrum of end results, 
business and engineering."
		 :pass-down (footer column-left))
   
   (glossary-declarative :type 'glossary
			 :dt "Declarative"
			 :dd (with-cl-who-string ()
			       "means that the user first \"declares,\" or describes, what he wishes to create, 
using the GDL core language, and the system then \"generates\" the object that has been described. 
For example, if a programmer wants to design a steering column, using GDL code he/she states (declares) 
in a concise manner that what he/she wants to generate has a cylindrical column, 4 wheels, 
the column attaching to the left or right front wheel, and a steering wheel. 
The GDL system, with its embedded geometry and algorithms \"generates\" a steering column."
			       :br :br 
			       "Thereafter, many refinements and changes may be made incrementally 
and \"on the fly,\" meaning that a change may be inserted into the object's description or its 
specifications, and the system will update the existing model throughout so as to be consistent 
with the change, without re-starting everything from scratch.")
			 :pass-down (footer column-left))
   
   (glossary-knowledge :type 'glossary
		       :dt "Knowledge"
		       :dd "is a reference to the information that is coded into the program. 
In the above example, the knowledge would be the four wheels, the column, 
the steering wheel, and the relationships among these parts."
		       :pass-down (footer column-left))
   
   (glossary-kbe :type 'glossary
		 :dt "KBE"
		 :dd "is an acronym for Knowledge-Based Engineering, that is, (a) starting with 
the object you wish to generate, (b) coding in the knowledge about the object (i.e., the 
components of a steering column), and (c) incrementally permitting the embedded geometry 
and algorithms to generate the exact specifications of the object you set out to design. 
All is done by the use of code."
		 :pass-down (footer column-left))
   
   (glossary-object :type 'glossary
		    :dt "Object-Oriented Language"
		    :dd "in a KBE context, begins with the object (the steering column), 
by coding in the knowledge the user possesses, and then through compact use of code 
refining it into the finished design. In KBE, the entire process is \"oriented\" 
to the object that the user has declared at the outset of the application."
		    :pass-down (footer column-left))
   
   (glossary-web :type 'glossary
		 :dt "Web-Based"
		 :dd "means that multiple users can access a single software installation, 
using standard computers, together with standard web browser software. 
Consequently, the power of a KBE language can be accessed without 
extensive hardware investment."
		 :pass-down (footer column-left))
   
   (glossary-caching :type 'glossary
		     :dt "Caching"
		     :dd "refers to the feature that once the application has 
computed a value, that value becomes \"memorized\" and the value 
consequently does not have to be re-computed when it subsequently is \"asked for.\""
		     :pass-down (footer column-left))
   
   (glossary-tracking :type 'glossary
		      :dt "Dependency Tracking"
		      :dd "keeps track of which memorized values (caching) are
			still available (\"valid\"), and which have to be
			re-computed (\"stale\"). The program automatically
			determines the status of a given value and then
			automatically re-computes those that are stale."
		      :pass-down (footer column-left))))



