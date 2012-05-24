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

(in-package :www.genworks.com)

(define-object documentation-and-educational (base-site-sheet)

  :computed-slots
  ((title "Genworks International - Documentation")
   (link-title  "Documentation")
   
   (right-section-inner-html
    (with-cl-who-string ()
      (:h2 "Documenation and Educational Resources") 
      
      (:h3 "Lecture and Tutorial Videos")
      ((:div :class "profile")
       ((:div :class "people")

	(:h4 "G102: GenDL Quickstart (customized for TU Delft ADM Course)")
	(:p "Slides are available " ((:a :href "http://www.genworks.com/downloads/training-g102-tud/" :target "g102-slides") "here") ", "
	    "Sample code " ((:a :href "https://github.com/genworks/Genworks-GDL/blob/master/documentation/training/g102-tud/examples/source/ch2-examples.lisp" 
				:target "gendl-code") "here") 
	    ". The exercise solutions will be provided later, after you have had a chance to try them yourself.")

	(:h4 "G102: GenDL Quickstart (old slides)")
	(:p "Slides are available " ((:a :href "http://www.genworks.com/downloads/training-g102/" :target "g102-slides") "here") ", "
	    "Exercise skeletons " ((:a :href "https://github.com/genworks/Genworks-GDL/blob/master/documentation/training/g102/examples/source/g102-exercises.lisp?raw=true" :target "gendl-code") "here") ", and "
	    "Sample code from the slides " ((:a :href "https://github.com/genworks/Genworks-GDL/blob/master/documentation/training/g102/examples/source/section-2-examples.lisp?raw=true" :target "gendl-code") "here") ".")


	(:h4 "Lecture Vidoes for Some of the Slides")

	(:p "You can play all the G102 videos, one after another, using this " 
	    ((:a :href "http://www.youtube.com/playlist?list=PLCF84A1DE8098DCE2" :target "gdl-videos") "playlist") ".")

	(:ol
	 (:li ((:a :href "http://www.youtube.com/watch?v=0fj9Cv_QWpw&hd=1" :target "gdl-videos")
	       "Installation of GenDL (Windows version) from simple Zipfile distribution (4:21)"))
	 (:li ((:a :href "http://www.youtube.com/watch?v=9QSzuBeYHPE&hd=1" :target "gdl-videos") 
	       "Section 1: Introduction (3:34)"))
	 (:li ((:a :href "http://www.youtube.com/watch?v=z0jSK3RyVIM&hd=1" :target "gdl-videos") 
	       "Section 2.1 - 2.3: Functions (4:20)"))
	 (:li ((:a :href "http://www.youtube.com/watch?v=AeYUr1AQH1Y&hd=1" :target "gdl-videos") 
	       "Section 2.3 - 2.4: Defining Functions (4:14)"))
	 (:li ((:a :href "http://www.youtube.com/watch?v=-KpiRYODYJ0&hd=1" :target "gdl-videos") 
	       "Section 2.4 - 2.5: Functions as Objects (3:57)"))
	 (:li ((:a :href "http://www.youtube.com/watch?v=qKbgzdvfjfE&hd=1" :target "gdl-videos") 
	       "Section 2.6 - 2.8: Basic Syntax of define-object (4:36)"))
	 (:li ((:a :href "http://www.youtube.com/watch?v=syMKEtC_rVI&hd=1" :target "gdl-videos") 
	       "Section 2.8 - 2.9: Making Objects and Sending Messages to them (3:35)"))
	 (:li ((:a :href "http://www.youtube.com/watch?v=DSQTeUMQ8Og&hd=1" :target "gdl-videos") 
	       "Section 2.10: Exercise 1 Intro (3:29)"))
	 (:li ((:a :href "http://www.youtube.com/watch?v=ZuYOZ33PiDI&hd=1" :target "gdl-videos") 
	       "Section 2.11: Child Objects (5:02)"))
	 (:li ((:a :href "http://www.youtube.com/watch?v=bIu_AywMMQU&hd=1" :target "gdl-videos") 
	       "Section 2.12: GenDL Object Sequences (4:44)")))))
      
      (:h3 "Manuals and Tutorials")
      ((:div :class "profile")
       ((:div :class "people")
	(:h4 "GenDL full Documentaion")
	(:p (:ul (:li ((:a :href "https://github.com/genworks/Genworks-GDL/blob/master/documentation/gdl-documentation.pdf?raw=true") "GDL Unified Documentation"))))))
      

      ))))
	 
	 
	
	    
