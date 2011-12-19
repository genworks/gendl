(in-package :genworks.com)

(define-object landing (base-site-sheet)

  :computed-slots 
  ((title "Genworks International: Knowledge-based Engineering and Beyond")
		   
   (header? nil)
   (normal-layout? nil)
		   
   (custom-content 
    (with-cl-who-string (:indent t)
      ((:div :class "content")

       ((:a :href (the home url) :title "Enter the Genworks Site")
	((:img :src "/site-static/images/logos/genworks-logo.gif" :alt "Genworks International")))

       ((:div :class "about_top")
	((:div :class "about_bottom")

	 ;;((:a :href "http://planet.lisp.org") ((:img :src "http://xach.com/img/jmc.jpg" :alt "John C McCarthy")))

	 
	 ((:a :href (the home url)) ((:img :src "/site-static/images/icons/robot.gif" :alt "")))
	 
	 ))

       ((:div :class "about_top_left")
	((:div :class "about_bottom_left")

	 ((:p :class "about") ((:img :src "/site-static/images/format/Genworks.gif" :alt "Genworks")) "
 provides high-end Knowledge-Based Engineering (KBE) software tools in
 practical and affordable Web-based packages.")))


       ((:div :class "about_top_left")
	((:div :class "about_bottom_left")
	 ((:p :class "about_left") "

GDL's unique combination of powerful programming language, tight integration
with a web server and a class leading geometry modeling kernel, plus access to
multiple open source libraries coupled with a licensing model designed to
encourage application deployment put GDL in a class of its own. No job is
too small yet no job is too large...

Throughout the development of our use of GDL, technical support and innovative
product enhancements has been at a level that other software companies can
only aspire to." :br " --Fortune 500 Customer, KBE Dep't Manager

 ")))
      
       ((:a :href (the home url) :class "link_enter" :title "Enter the Genworks Site")
	((:img :src "/site-static/images/icons/Enter.gif" :alt "Enter"))))))))
