;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: training-g101; Base: 10 -*-

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
or send them to info at genworks dot com.")))))))


