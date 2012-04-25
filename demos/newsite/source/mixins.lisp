(in-package :www.genworks.com)

(define-object base-site-sheet (base-ajax-graphics-sheet)
  
  :input-slots ((lang *lang* :defaulting))

  :computed-slots
  ((additional-header-content 
    (with-cl-who-string ()
      
      ((:link :type "text/css" :rel "stylesheet" :href "/newsite-static/css/style.css"))
      ((:meta :name "description" 
	      :content "Genworks is a developer and vendor for General-purpose Declarative Language (Gendl), 
a high-productivity application development system used for creating web-centric Knowledge-based 
Engineering and Business applications which can involve complex 3D geometry."))
      ((:meta :name "keywords" :content "Genworks, Knowledge-Based Engineering, KBE, GDL, Gendl, GenDL, Common Lisp, 
Generative Programming, Dynamic Languages, 3D Geometry, NURBS, Solid Modeling, Disruptive Technologies"))

      ((:script :src "/newsite-static/detect.js"))
      ((:script :src "/newsite-static/swfobject.js"))))


   (additional-header-js-content

    (with-cl-who-string ()
      (:script "

        $j = jQuery.noConflict();
        $j(document).ready(function() {
            var canvas;
            var gl, experimental;

            var has_web_gl = false;
            var has_flash  = false;
            var x3d_element = null;
            
            b = BrowserDetect;
            b.init();

            canvas = document.getElementById(\"x3dom-logo\");

            try {
                gl = canvas.getContext(\"webgl\");
            } catch (x) {
                gl = null;
            }

            if (gl == null) {
                try {
                    gl = canvas.getContext(\"experimental-webgl\");
                    experimental = true;
                } catch (x) {
                    gl = null;
                }
            }

            if (gl) {
                // webgl works
                has_web_gl = true;
            } else if (\"WebGLRenderingContext\" in window) {

            } else {
                // no webgl
                has_web_gl = false;
            }

            // detect flash
            if (swfobject.hasFlashPlayerVersion('11.0.0')) {
                has_flash = true;
            }

            if (!has_web_gl && !has_flash) {
                $j(\"#no-webgl-no-flash\").show();
                
            } else {
                $j(\"#all-go\").show();
            }

        });

")))


   (use-jquery? t)
   (use-raphael? t)

   (html-sections (list (the nav-section)
			(the news-section)
			(the support-section)
			(the right-section)
			(the footer-section)))
   
   (length 420)
   (width 526)
   (background-color :silver )
   
   (main-sheet-body
    (with-cl-who-string ()
      ((:canvas :id "x3dom-logo" :style "display:none"))
      ((:div :id "wrapper") 
   ((:div :id "header-nobg")
	((:div :id "no-webgl-no-flash" :class "header-nobg" :style "display:none;")
	 (str (the vector-graphics)))
  
  ((:div :id "all-go" :class "header-nobg" :style "display:none;")
     (the write-embedded-x3dom-world))
  
  #+nil	

    ((:div :id "all-go" :class "product-image" :style "display:none;")
     (:img :src "/newsite-static/images/Gendl-dave-low-521.png" )))
       ((:div :id "left")
	((:div :id "logo") (if *developing?* (str (the development-links))
			       (htm (:H1 "Genworks International"))))
	(str (the nav-section main-div))
	(str (the news-section main-div))
	(str (the support-section main-div)))
       
       
       (str (the right-section main-div))
       ((:div :class "clear")) ((:div :id "spacer"))
       (str (the footer-section main-div)))))
   
   (right-section-inner-html (with-cl-who-string ()
			       (:h2 "Empty Template")))


   (link-title (the strings-for-display))

   
   (display-list-object-roots (list (the robot)))
   (x3dom-view-controls? nil)
   
   (current-right-section-rp (the index-html root-path) :settable)
   (current-right-section (the root (follow-root-path (the current-right-section-rp))))

   (show-child-links? nil :settable)

   (child-pages (remove-if-not #'(lambda(item) (typep item 'base-site-sheet)) (the children)))

   )
  
  :functions
  ((set-right-section-rp! 
    (page)
    (the (set-slot! :current-right-section-rp (the-object page root-path))))

   (toggle-children!
    (page)
    (the-object page (set-slot! :show-child-links? (not (the-object page show-child-links?))))))


  :hidden-objects
  ((nav-section :type 'sheet-section
		:dom-id "nav"
		:inner-html (with-cl-who-string ()
			      (:ul (dolist (page (the pages))
				     (htm (:li ((:span :class (format nil "clickable~a" (if (eql (the current-right-section) page) " selected" ""))
						       :onclick (the (gdl-ajax-call :function-key (if (the-object page child-pages) 
												      :toggle-children! 
												      :set-right-section-rp!)
											:arguments (list page))))
						(str (the-object page link-title)))
					       ;;
					       ;; FLAG - make this recursive
					       ;;
					       (when (the-object page show-child-links?)
						 (htm (dolist (child (the-object page children))
							(htm (:li ((:span :class (format nil "clickable submenu~a" (if (eql (the current-right-section) child) " selected" ""))
									  :onclick (the (gdl-ajax-call :function-key (if (the-object child child-pages) 
															 :toggle-children! 
															 :set-right-section-rp!)
												       :arguments (list child))))
								   "&nbsp;&nbsp;" (str (the-object child link-title))))))))))))))
   
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
				 "Integration and Testing of Genworks GDL 1581 (GenDL) with "
				 ((:a :href "http://www.smlib.com") " SMLib 8.51 ")
				 " is completed.")
				(if (eql self (the news))
				    (htm ((:p :class "more") 
					  ((:a :href (the (relative-url-to (the index-html url)))) "less...")))
				    (htm ((:p :class "more") 
					  ((:a :href (the (relative-url-to (the news url)))) "more...")))))))

   (support-section :type 'sheet-section
		    :dom-id "support"
		    
		    :inner-html (with-cl-who-string ()
				  ((:div :class "support")
			(:img :src "/newsite-static/images/support.jpg"))	  
			
				  #+nil
				  (:ul
				   (:li (:h3 ((:a :href "http://dl.dropbox.com/u/19667598/static/documents/gdl-documentation.pdf" :target "_fresh")
					      "Prototype GenDL Manual")))
				   (:li (:h3 ((:a :href "/yadd" :target "_fresh")
					      "Live Reference Docs (YADD)"))))))

   
   (right-section :type 'sheet-section
		  :dom-id "right"
		  :inner-html (the right-section-inner-html))
   
   (footer-section :type 'sheet-section
		    :dom-id "footer"
		    :inner-html (with-cl-who-string ()
				  ((:div :id "copyright") 
				   (:h1 "Practical Tools for Generative Application Development")
				   (:p "")
				   "Copyright &copy; 2012 Genworks International. All right reserved.")
				   ((:div :id "footerline"))))))



