(in-package :www.genworks.com)

(define-object base-site-sheet (base-ajax-sheet)

  :computed-slots
  ((additional-header-content 
    (with-cl-who-string ()
      ((:link :type "text/css" :rel "stylesheet" :href "/newsite-static/css/style.css"))
      ((:meta :name "description" 
	      :content "Genworks is a developer and vendor for General-purpose Declarative Language (GDL), a high-productivity application development 
system used for creating web-centric Knowledge-based Engineering and Business applications which can involve complex 3D geometry."))
      ((:meta :name "keywords" :content "Genworks, Knowledge-Based Engineering, KBE, GDL, Common Lisp, Generative Programming, Dynamic Languages, 3D Geometry, NURBS, Solid Modeling, Disruptive Technologies"))))
   

   (html-sections (list (the nav-section)
			(the news-section)
			(the support-section)
			(the right-section)
			(the footer-section)))

   (main-sheet-body
    (with-cl-who-string ()
      ((:div :id "wrapper") 
       ((:div :id "header") (when *developing?* (str (the development-links))))
       ((:div :id "left")
	((:div :id "logo") (:H1 "Genworks International") 
	 (:p "Practical Tools for Generative Application Development"))
	(str (the nav-section main-div))
	(str (the news-section main-div))
	(str (the support-section main-div)))
       (str (the right-section main-div))
       ((:div :class "clear")) ((:div :id "spacer"))
       (str (the footer-section main-div)))))
   
   (right-section-inner-html (with-cl-who-string ()
			       (:h2 "Empty Template")))


   (link-title (the strings-for-display)))



  :hidden-objects
  ((nav-section :type 'sheet-section
		:dom-id "nav"
		:inner-html (with-cl-who-string ()
			      (:ul (dolist (page (the pages))
				     (htm (:li ((:a :href (the (relative-url-to (the-object page url))))
						(str (the-object page link-title)))))))))

   
   (news-section :type 'sheet-section
		 :dom-id "news"
		 :inner-html (with-cl-who-string ()
			       ((:div :class "news")
				(:h2 "Latest News") 
				(:h3 "2012-03-28")
				(:p
				 "New Genworks Website prototype is launched.")
				((:div :class "hr-dots")) 
				(:h3 "2012-03-15")
				(:p
				 "Integration and Testing of Genworks GDL 1581 with "
				 ((:a :href "http://www.smlib.com") " SMLib 8.51 ")
				 " is completed.")
				(if (eql self (the news))
				    (htm ((:p :class "more") ((:a :href (the (relative-url-to (the index-html url)))) "less...")))
				    (htm ((:p :class "more") ((:a :href (the (relative-url-to (the news url)))) "more...")))))))

   (support-section :type 'sheet-section
		    :dom-id "support"
		    :inner-html (with-cl-who-string ()
				  (:P "Technical Support")))

   
   (right-section :type 'sheet-section
		  :dom-id "right"
		  :inner-html (the right-section-inner-html))
   
   (footer-section :type 'sheet-section
		    :dom-id "footer"
		    :inner-html (with-cl-who-string ()
				  ((:div :id "copyright") "Copyright &copy; 2011 Copyright KE-Works S.R.L Romania All right reserved.")
				  ((:div :id "footerline"))))))



