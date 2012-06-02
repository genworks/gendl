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
      (:h2 "Documentation and Educational Resources") 
      
      (:h3 "i. Standard Emacs Tutorial:")
      ((:div :class "profile")
       ((:div :class "people")

	(:ul 
	 (:li
	  ((:a :href "http://www.youtube.com/playlist?list=PLCD0CCC8235C9E988&feature=view_all" :target "gdl-videos") 
	   "Video Playlist (23:36)")
	  (:ol
	   (:li ((:a :href "http://www.youtube.com/watch?v=1kcspOlfrc4&hd=1" :target "gdl-videos")
		 "Emacs Tutorial Part 1") ": Moving around Emacs (5:09)")
	   (:li ((:a :href "http://www.youtube.com/watch?v=7093SgxBQ9M&hd=1" :target "gdl-videos")
		 "Emacs Tutorial Part 2") ": Files and Buffers (4:28)")
	   (:li ((:a :href "http://www.youtube.com/watch?v=-Wr7X6RV_qw&hd=1" :target "gdl-videos")
		 "Emacs Tutorial Part 3") ": Cutting/Copying & Pasting, Searching, Tutorial Conclusion (4:58)"))))))

      (:h3 "ii. Gendl Authoring with Emacs:")
      ((:div :class "profile")
       ((:div :class "people")
	(:ul
	 (:li 
	  ((:a :href "http://www.youtube.com/watch?v=GYJwwYJQfJg&list=PLC7A2A32482620129&feature=view_all" :target "gdl-videos") 
	   "Video Playlist (4:10)")
	  (:ol 
	   (:li ((:a :href "http://www.youtube.com/watch?v=GYJwwYJQfJg&hd=1" :target "gdl-videos")
		 "Emacs for Gendl Editing Part 1 (4:10)"))
	 (:li "...Stay tuned for more parts in this series..."))))))
      

      (:h3 "1. Gendl Introduction")
      ((:div :class "profile")
       ((:div :class "people")
	(:ul
	 (:li 
	  ((:a :href "http://www.genworks.com/downloads/training-g102-tud/introduction/index.html" :target "g102-slides") "Tutorial Slides"))
	 (:li "(no example code for this section)")
	 (:li ((:a :href "http://www.youtube.com/watch?v=2ybhiiw7Z0M&hd=1" :target "gdl-videos")
	       "Video")))))

      (:h3 "2. Functions, and Objects")
      ((:div :class "profile")
       ((:div :class "people")
	(:ul
	 (:li ((:a :href "http://www.genworks.com/downloads/training-g102-tud/objects/index.html" :target "g102-slides") "Tutorial Slides"))
	 (:li ((:a :href "https://github.com/genworks/Genworks-GDL/blob/master/documentation/training/g102-tud/examples/source/ch2-examples.lisp" 
		   :target "gendl-code") "Example Code")
	      " "
	      (:i "The exercise solutions will be provided after you have had a chance to try them yourself."))
	 (:li ((:a :href "http://www.youtube.com/playlist?list=PL3DEECB461E154659" :target "g102-slides") 
	       "Video Playlist")
	      (:ol
	       (:li ((:a :href "http://www.youtube.com/watch?v=f8fra0SVFyU&hd=1" :target "gdl-videos")
		     "G102-TUD Seminar Part 2 (4:59)")
		    " Up through the concept of (setq self (make-object ...))")
	       (:li ((:a :href "http://www.youtube.com/watch?v=dlIWDhDLSPU&hd=1" :target "gdl-videos")
		     "G102-TUD Seminar Part 3 (4:23)")
		    " Shorthand for setting self to a new instance; Defining an object which takes
inputs, and providing input values when creating an instance.")
	       (:li ((:a :href "http://www.youtube.com/watch?v=AK9pLmU_L18&hd=1" :target "gdl-videos")
		     "G102-TUD Seminar Part 4 (4:54)")
		    " Different types of input-slots; Exercise 1 involving input-slots; 
Inroduction to Child objects.")
	       (:li ((:a :href "http://www.youtube.com/watch?v=ZHxYhbst3Zc&hd=1" :target "gdl-videos")
		     "G102-TUD Seminar Part 5 (4:53)")
		    " Working with standard object sequences (via aggregate objects) ")
	       (:li ((:a :href "http://www.youtube.com/watch?v=RUYP8_e4QN0&hd=1" :target "gdl-videos")
		     "G102-TUD Seminar Part 6 (4:29)")
		    " Lisp background on plists. Exercise 2 using plist to feed a sequenced child object.")
	       (:li ((:a :href "http://www.youtube.com/watch?v=MucUXirjVnA&hd=1" :target "gdl-videos")
		     "G102-TUD Seminar Part 7 (4:20)")
		    " Gendl Functions, and Exercise 3 involving Gendl Functions."))))))
      
      (:h3 "3. Geometry")
      ((:div :class "profile")
       ((:div :class "people")
	(:ul 
	 (:li ((:a :href "http://www.genworks.com/downloads/training-g102-tud/geometry/index.html" :target "g102-slides") "Tutorial Slides"))
	 (:li ((:a :href "https://github.com/genworks/Genworks-GDL/blob/master/documentation/training/g102-tud/examples/source/ch3-examples.lisp" 
		   :target "gendl-code") "Example Code")
	      " "
	      (:i "The exercise solutions will be provided after you have had a chance to try them yourself."))
	 (:li ((:a :href "http://www.youtube.com/playlist?list=PL89112790947EE798" :target "g102-slides")
	       "Video Playlist")
	      (:ol
	       (:li ((:a :href "http://www.youtube.com/watch?v=lzTuk6xAgYA&hd=1" :target "gdl-videos")
		     "G102-TUD Seminar Part 8 (5:00)")
		    " Overview of base-object for coordinate system, points and vectors, 
translating points along a vector and within a local coordinate system.")
	       (:li ((:a :href "http://www.youtube.com/watch?v=UfvyJj8M--k&hd=1" :target "gdl-videos")
		     "G102-TUD Seminar Part 9 (4:52)")
		    " Vectors, 
orientation, and the first Exercise.")
	       (:li ((:a :href "http://www.youtube.com/watch?v=oHj6yw0Q_Rk&hd=1" :target "gdl-videos")
		     "G102-TUD Seminar Part 10 (1:59)")
		    " Overview of Wireframe Geometric Primitives ")
	       (:li ((:a :href "http://www.youtube.com/watch?v=LpR_fBt9uOs&hd=1" :target "gdl-videos")
		     "G102-TUD Seminar Part 11 (4:59)")
		    " NURBS Curves")

	       (:li ((:a :href "http://www.youtube.com/watch?v=yK4ewwm_Ctk&hd=1" :target "gdl-videos")
		     "G102-TUD Seminar Part 12 (4:49)")
		    " NURBS Surfaces"))))))


      (:h3 "Manuals and Tutorials")
      ((:div :class "profile")
       ((:div :class "people")
	(:ul 
	 (:li ((:a :href "https://github.com/genworks/Genworks-GDL/blob/master/documentation/gdl-documentation.pdf?raw=true") "GDL Unified Documentation"))
	 (:li ((:a :href "http://localhost:9000/yadd") "Built-in Reference Documentation")))))))))


	#|
	(:h4 "G102: GenDL Quickstart")
	(:p "Slides are available " ((:a :href "http://www.genworks.com/downloads/training-g102/" :target "g102-slides") "here") ", "
	    "Exercise skeletons " ((:a :href "https://github.com/genworks/Genworks-GDL/blob/master/documentation/training/g102/examples/source/g102-exercises.lisp?raw=true" :target "gendl-code") "here") ", and "
	    "Sample code from the slides " ((:a :href "https://github.com/genworks/Genworks-GDL/blob/master/documentation/training/g102/examples/source/section-2-examples.lisp?raw=true" :target "gendl-code") "here") ".")

	(:h4 "Lecture Videos for Some of the Slides")

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
	       "Section 2.12: GenDL Object Sequences (4:44)")))
	|#
