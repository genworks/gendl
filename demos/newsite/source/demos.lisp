(in-package :www.genworks.com)


(define-object demos (base-site-sheet)

  :computed-slots
  ((title "Genworks International - Demos")
   (link-title  "Demos")
   
   #+nil
   (right-section-inner-html 
    (with-cl-who-string ()
      (:h2 "Our Products") ((:div :id "welcome"))
      (:ul
       (:li ((:a :href (the descriptions url)) "Descriptions"))
       (:li ((:a :href (the licensing url)) "Licensing"))
       (:li ((:a :href (the configurator url)) "Configurator"))))))

  
  :objects
  ((twenty-four :type 'assembly-24
		:pass-down (respondent))))


#+nil
(define-object demos (base-site-sheet)

  :computed-slots
  ((title "Genworks International - Demos")
   (link-title  "Demos")

   (right-section-inner-html
    (with-cl-who-string ()
      (:H2 "Demos") (:H3 "Aerospace")
      ((:DIV :ID "profile")
       ((:DIV :ID "d-corp")
	((:DIV :ID "d-corp-img") "
            Aero Demo 1")
	(:P "Under construction "))
       ((:DIV :ID "d-indu")
	((:DIV :ID "d-indu-img") "
            Aero Demo 2")
	(:P "Under construction "))
       ((:P :CLASS "more") ((:A :HREF "") "View Details")))
      (:H3 "Wind Energy")
      ((:DIV :ID "profile")
       ((:DIV :ID "d-corp")
	((:DIV :ID "d-corp-img") "
	            Wind Demo 1")
	(:P "Under construction"))
       ((:DIV :ID "d-indu")
	((:DIV :ID "d-indu-img") "
	            Wind Demo 2")
	(:P "Under construction"))
       ((:DIV :CLASS "clear"))
       ((:P :CLASS "more") ((:A :HREF "") "View Details")))))))

