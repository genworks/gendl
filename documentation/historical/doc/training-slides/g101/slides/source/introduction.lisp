;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: training-g101; Base: 10 -*-

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
	 "1997-present: Approaching 12 years running Genworks, a KBE tools vendor")
	(:description
	 "1993-1997: at Ford Motor, building and administering KBE applications with ICAD Design Language")
	(:description 
	 ,(string-append 
	  "1983-1993:"
	  (with-output-to-string (ss)
	     (html-stream 
	      ss
	      (:ul
	       (:li "Master in Computer Science, U of Michigan")
	       (:li "Experience in Database Industry (Quantum/Progress)")
	       (:li "Experience in CAD Industry (Applicon)")
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
