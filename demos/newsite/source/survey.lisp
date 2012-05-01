(in-package :www.genworks.com)

(define-object survey (base-site-sheet)
  

  :computed-slots
  ((title "Genworks-NOESIS Interoperability - Welcome")
   
   (additional-header-content 
    (with-cl-who-string ()
      
      ((:link :type "text/css" :rel "stylesheet" :href "/newsite-static/css/style.css"))
      ((:meta :name "description" 
	      :content "Spurred by a group of researchers at the TU
Delft, Genworks and NOESIS are considering a joint offering..."))
      ((:meta :name "keywords" :content "Optimization, NOESIS, Optimus, Genworks, 
Knowledge-Based Engineering, KBE, GDL, Gendl, GenDL, Common Lisp, Generative Programming, 
Dynamic Languages, 3D Geometry, NURBS, Solid Modeling, Disruptive Technologies"))

      ((:script :src "/newsite-static/detect.js"))
      ((:script :src "/newsite-static/swfobject.js"))))



   (main-sheet-body
    (with-cl-who-string ()
      ((:canvas :id "x3dom-logo" :style "display:none"))
      ((:div :id "wrapper") 
       ((:div :id "header-nobg")
	
	((:div :id "no-webgl-no-flash" :class "header-nobg" :style "display:none;")
	 (str (the vector-graphics)))

	((:div :id "all-go" :class "header-nobg" :style "display:none;")
	 (the write-embedded-x3dom-world))
	
	((:div :id "product-image" :class "product-image" :style "display:none;")
	 (:img :src "/newsite-static/images/Gendl-dave-low-521.png" )))

       ((:div :id "left")
	((:div :id "genworks-noesis-logo") (if nil (str (the development-links))))

	(str (the nav-section main-div))
	
	(str (the news-section main-div))
	(str (the support-section main-div)))
       
       
       (str (the right-section main-div))
       ((:div :class "clear")) ((:div :id "spacer"))
       (str (the footer-section main-div)))))


   (lang (the language choice value))
   
   (right-section-inner-html (the current-right-section right-section-inner-html))
   ;;(right-section-js-to-eval (the current-right-section right-section-js-to-eval))
   ;;(right-section-js-always-to-eval (the current-right-section right-section-js-to-eval))

   (right-section-js-to-eval nil)
   (right-section-js-always-to-eval nil)


   (link-title "Home")
   
   (pages (the children))

   (display-list-object-roots (list (the b-spline)))

   (survey-root self)
   )
  
  :trickle-down-slots (pages lang survey-root)

  :hidden-objects
  (   
   (b-spline :type 'surf:test-b-spline-surface
	     :z-stretch (the demos z-stretch value))
   

   (products :type 'null-part)
   

   (support-section 
    :type 'sheet-section
    :dom-id "support"
    :inner-html (with-cl-who-string ()
		  ((:div :id "address" :class "support" :style "display:block")
		   "&nbsp;&nbsp;&nbsp;&nbsp;Kluyverweg 1" :br
		   "&nbsp;&nbsp;&nbsp;&nbsp;2629 HS Delft" :br
		   "&nbsp;&nbsp;&nbsp;&nbsp;The Netherlands" :br
		   "&nbsp;&nbsp;&nbsp;&nbsp;+31 152-785-332")))



   (news-section :type 'sheet-section
		 :dom-id "news"
		 :inner-html (with-cl-who-string ()
			       ((:div :class "news")
				(:h2 "Latest News") 
				(:h3 "2012-03-28")
				(:p
				 "Genworks-NOESIS Interop Survey Site launched.")
				((:div :class "hr-dots")) 
				(:h3 "2012-04-27")
				(:p
				 "Prototype Testing of HTTP-based Gendl-Optimus Link is completed at TU Delft.")
				(if (eql self (the news))
				    (htm ((:p :class "more") 
					  ((:a :href (the (relative-url-to (the survey-root url)))) "less...")))
				    (htm ((:p :class "more") "more..."
					  ;;((:a :href (the (relative-url-to (the news url)))) "more...")
					  ))))))


   (footer-section :type 'sheet-section
		   :dom-id "footer"
		   :inner-html (with-cl-who-string ()
				 ((:div :id "copyright") 
				  (:h1 "Practical Tools for Multi-Disciplinary Optimization")
				  (:p "")
				  "Copyright &copy; 2012 Genworks-NOESIS Interop Project. All right reserved.")
				 ((:div :id "footerline"))))
   
   (news :type 'survey-news)

   )

  :objects
  ((index-html :type 'survey-index-html
	       :respondent self)

   (demos :type 'survey-demos
	  :respondent self)

   (people :type 'survey-people
	   :respondent self)

   (contact-us :type 'survey-survey
	       :respondent self)
   
   (language :type 'language
	     :respondent self)

   ))



(define-object survey-survey (base-site-sheet)

  :computed-slots
  ((title "Genworks NOESIS Interop - Survey")
   (link-title  "Survey")
   
   (right-section-inner-html
    (with-cl-who-string ()
      (unless (the message-sent?)
	(htm (:h2 "Customer Interest Survey") ((:div :id "contact"))
	     
	     (:p "Optionally provide your email address so that we may follow
up with a customized response.  Your email will not be shared beyond
this project and will be removed from our site records upon request.")))

      (when (the message-sent?)
	(htm (:h2 "Thank You")))

      (with-html-form (:cl-who? t)
	
	(if (the message-sent?)
	    (htm (:p "Your survey has been sent. Your answers will help us tailor our offering to best suit your needs.")

		 (:p "Please " ((:span :class "clickme" 
				       :onclick (the (gdl-ajax-call :function-key :reset-message-sent)))
				"click here") " if you would like to fill out another survey."))

	    (htm 
	     (:fieldset
	      (:p (str (the email-address html-string)))
	      (:p (str (the how-did-you-hear html-string)))
	      (when (eql (the how-did-you-hear value) :other)
		(htm (:p (str (the how-did-you-hear-other html-string)))))
	      (:p (str (the main-industry html-string)))
	      (when (eql (the main-industry value) :other)
		(htm (:p (str (the main-industry-other html-string)))))
	      (:p (str (the biggest-bottleneck html-string)))
	      (when (eql (the biggest-bottleneck value) :other)
		(htm (:p (str (the biggest-bottleneck-other html-string)))))
	      :tr (str (the send-button form-control-string))
	      ))))))
   
   
   (how-did-you-hear-options (list :paper-mailing "Brochure received in Paper Mail"
				   :email "Receved an Announcement via Email"
				   :phone-call "Receive a phone call from a Project Representative"
				   :word-of-mouth "Heard through unspecified Word of Mouth"
				   :other "Other means (optionally please specify more info...)"))


   (main-industry-options (list :aerospace-oem "Aerospace OEM"
				:aerospace-tier-1 "Aerospace Tier-1 Supplier"
				:aerospace-specialized "Aerospace Specialized Supplier"
				:automotive-oem "Automotive OEM"
				:automotive-tier-1 "Automotive Tier-1 Supplier"
				:automotive-specialized "Automotive Specialized Supplier"
				:transport "non-Aerospace, non-Automotive Transportation Industry"
				:marine "Marine Construction"
				:civil "Civil Engineering"
				:other "Other, Please Specify..."))


   (biggest-bottleneck-options (list :initial-modeling "Building 3D models"
				     :adjust-modeling "Changes to 3D models"
				     :cad-management "Management of CAD files"
				     :fea-mesh "Building FEA meshes"
				     :data-reentry "Redundant Data Re-entry"
				     :other "Other, please specify..."))
				


   (message-sent? nil :settable)

   )


  :objects
  (


   (how-did-you-hear :type 'menu-form-control
		     :size 1
		     :choice-plist (the how-did-you-hear-options)
		     :default (first (the-child choice-plist))
		     :ajax-submit-on-change? t
		     :prompt "How did you Hear of the Project?")


   (how-did-you-hear-other :type 'text-form-control
			   :default ""
			   :prompt (the how-did-you-hear prompt)
			   :size 40
			   :rows 5)

   (main-industry :type 'menu-form-control
		  :size 1
		  :choice-plist (the main-industry-options)
		  :default (first (the-child choice-plist))
		  :ajax-submit-on-change? t
		  :prompt "What is your Main Industry?")


   (main-industry-other :type 'text-form-control
			:default ""
			:prompt (the main-industry prompt)
			:size 40
			:rows 5)

   (biggest-bottleneck :type 'menu-form-control
		       :size 1
		       :choice-plist (the biggest-bottleneck-options)
		       :default (first (the-child choice-plist))
		       :ajax-submit-on-change? t
		       :prompt "What is your Biggest Bottleneck?")

   (biggest-bottleneck-other :type 'text-form-control
			     :default ""
			     :prompt (the biggest-bottleneck prompt)
			     :size 40
			     :rows 5)


   ;; FLAG -- change this to email-form-control when available 
   (email-address :type 'text-form-control 
		  :size 25
		  :label-position :prepend
		  :prompt "Your Email Address (optional): "
		  :default ""
		  :ajax-submit-on-change? t)




   (send-button :type 'button-form-control
		:default "Send"
		:onclick (the (gdl-ajax-call :function-key :send-email)))

   )


  :functions
  ((reset-message-sent 
    ()
    (the (set-slot! :message-sent? nil)))

   (send-email
    ()
    (format t "Should be sending Email...~%")
    
    (the restore-form-controls!)
    (the (set-slot! :message-sent? t)))

   (restore-form-controls!
    ()
    (dolist (item (list (the how-did-you-hear) (the main-industry) (the biggest-bottleneck)))
      (the-object item restore-defaults!)))))


(define-object survey-people (base-site-sheet)

  :computed-slots
  ((title "Genworks-NOESIS Interop - People")
   (link-title  "People")
   
   (right-section-inner-html
    (with-cl-who-string ()
      (:h2 "The People who will make Genworks-NOESIS Interop possible") 
      
      (:h3 "Reinier van Dijk")
      ((:image :src "/newsite-static/images/reinier.jpg"))
      ((:div :class "profile")
       ((:div :class "people")
	(:h4 "Head of Product Development and Marketing")
	(:p
	 "Reinier is currently completing the final requirements for PhD from Technical University of Delft. 
Prior to his career as a PhD researcher, Reinier was involved with ...")
	(:p "Reinier holds Bacheler and Master degrees in Aerospace Engineering 
	   both from the TU Delft.")
	(:p "Reinier has spoken and delivered papers at numerous Conferences...")))

      (:h3 "Michel van Tooren")
      ((:image :src "/newsite-static/images/michel.jpg"))
      ((:div :class "profile")
       ((:div :class "people")
	(:h4 "Head of Strategic Relationships")
	(:p
	 "Michel van Tooren took up the post of New Concept
	 Development Manager with Fokker Aerostructures on 1 July, 2010. Van
	 Tooren continues to supervise his PhD students at the TU Delft Faculty.")
	(:p "Dr van Tooren holds PhD degree from the TU Delft and was
	the recipient of Most Entrepreneurial Professor award in
	2009. Michel also holds the following distinguished titles: ...")))))))
      




(define-object survey-index-html (base-site-sheet)

  :computed-slots
  ((title "Genworks-NOESIS Interop - Welcome")
   (link-title "Home")
   (right-section-inner-html  
    (with-cl-who-string ()
      (:h2 "Welcome to the Genworks and NOESIS Interop Project")
      ((:div :id "welcome")
       (#+nil
	(:img :src "/newsite-static/images/logo-transparent.png" :ALT "star-logo" :CLASS "left" :WIDTH "155"
	      :height "149"))
       (:p (:strong "Genworks International")
	   " and "
	   (:strong "NOESIS Solutions")
	   " are considering a joint offering, facilitated by a spin-off from the TU Delft, ..."))

      (:h3 "About Genworks")
      ((:div :class "profile")
       ((:div :id "corp")
	;;((:div :id "corp-img") "Aerospace")
	(:p (:strong "Genworks International")
	    " was founded in November, 1997 as Knowledge Based
              Solutions, a Michigan Corporation. ")
	(:p "Genworks International operates as an independent entity
             free of underlying debt, hidden partners, or
             big-corporate influences.")
	(:p "Our focus is primarily as a first-level vendor for the
             GenDL suite of tools, rather than domain-specific
             vertical applications or specialized application
             consulting. This allows us to keep objectives aligned
             with our customers, and avoid potential
             conflict-of-interest experienced by other KBE vendors who
             also presume to dominate the market for vertical
             applications and consulting using their own tool.")))


      (:h3 "About NOESIS")
      ((:div :class "profile")
       ((:div :id "corp")
	;;((:div :id "corp-img") "Aerospace")
	(:p (:strong "NOESIS Solutions")
	    (:p "Noesis Solutions is an engineering innovation partner
	    to manufacturers in automotive, aerospace and other
	    advanced engineering industries. Specialized in simulation
	    process integration and numerical optimization, its
	    flagship product Optimus focuses on resolving customers’
	    toughest multi-disciplinary engineering challenges.")

	    (:p "The Optimus software platform identifies the best
	    design candidates by managing a parametric simulation
	    campaign and using the software tools of customers. After
	    evaluating the proposed design candidates, customers pick
	    the most optimal and robust design option to verify in
	    detail and take into production. This winning strategy
	    delivers the best product in the shortest time possible
	    while saving tremendously on resources.")

	    (:p "In July 2010, Noesis Solutions joined the CYBERNET
	    SYSTEMS Group, a group of companies focused on Computer
	    Aided Engineering. Noesis Solutions operates through a
	    network of subsidiaries and representatives in key
	    locations around the world. Noesis Solutions takes part in
	    key research projects sponsored by various official
	    organizations, including the European Commission."))))))))


(define-object survey-demos (base-site-sheet)

  :computed-slots
  ((title "Genworks International - Demos")
   (link-title  "Demos")

   (right-section-inner-html 
    (with-cl-who-string ()
      (:h2 "Simple Demo") ((:div :id "welcome"))
      (:fieldset (str (the z-stretch html-string)))
      (str (the ok-button form-control-string))
      (:p (:small "OK button refreshes entire screen only for testing -- 
in finished app, graphics will be updated with Ajax")))))

  :hidden-objects
  ((ok-button :type 'button-form-control
	      :default " OK "
	      :onclick (string-append ;;(the (gdl-ajax-call :form-controls (list (the z-stretch))))
			"location.reload(true);"))

   (z-stretch :type 'number-form-control
	      :ajax-submit-on-change? t
	      :default 1
	      :size 5
	      :prompt "Z Stretch")))


(define-object survey-news (base-site-sheet)

  :computed-slots
  ((title "Genworks-NOEIS Interop - News")
   (link-title  "News")
   
   (display-list-object-roots (list (the b-spline)))
   
   (right-section-inner-html 
    (with-cl-who-string ()
      (:h2 "Prior News") 
      (:h3 "2012-02-15")
      (:p "Prototype work for Gendl-Optimus HTTP-based connection initiated at TU Delft by Reinier van Dijk.")
      (:h3 "2011-12-21")
      (:p "Concept for neutral third-party entity to manage Interop project is initiated.")
      (:h3 "2011-10-22")
      (:p "Initial discussions regarding Genworks-NOESIS Interop project initiated by Michel van Tooren.")))))


(publish-gwl-app "/survey" "www.genworks.com::survey")
(publish-gwl-app "/survey/" "www.genworks.com::survey")
(publish-gwl-app "/survey.html" "www.genworks.com::survey")


