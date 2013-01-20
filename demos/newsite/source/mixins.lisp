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

(define-object base-site-sheet (base-ajax-graphics-sheet)
  
  :input-slots ((lang *lang* :defaulting))

  :computed-slots
  ((additional-header-content 
    (with-cl-who-string ()
      
      ((:link :type "text/css" :rel "stylesheet" :href "/newsite-static/css/style.css"))
      ((:meta :name "description" 
	      :content "Genworks is a developer and vendor for General-purpose Declarative Language (GenDL), 
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
            b = BrowserDetect;
            b.init(); 
            display_x3dom_or_raphael ();
          })
   
        function hide_all ()
        {
         $j(\"#no-webgl-no-flash\").hide();
         $j(\"#all-go\").hide();
         $j(\"#product-image\").hide();
        }
       
        function resize_x3dom(width, height)
        {$j(\"#the_element\").attr(\"width\",width);
         $j(\"#the_element\").attr(\"height\",height);
        }


        function display_x3dom_or_raphael ()
         {
            hide_all();

            var canvas;
            var gl, experimental;

            var has_web_gl = false;
            var has_flash  = false;
            var x3d_element = null;
            
/*            b = BrowserDetect;
            b.init(); */

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

        } 
")))


   (use-jquery? t)
   (use-raphael? t)

   (html-sections (list (the nav-section)
			(the news-section)
			(the support-section)
			(the right-section)
			(the footer-section)))
   
   (length 444)
   (width 526)
   (background-color :silver)
   
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
   
   ;;(right-section-js-to-eval "$j('#product-image').hide(200); $j('#all-go').show(200); $j('#address').hide(200); $j('#tickete').show(200);")
   (right-section-js-to-eval (string-append "display_x3dom_or_raphael();"
					    (format nil "resize_x3dom('~apx', '~apx');"
						    (the width) (the length))))


   (link-title (the strings-for-display))

   
   ;;(display-list-object-roots (list (the robot)))

   (display-list-object-roots (list (the primi-plane)))

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
  ((nav-section 
    :type 'sheet-section
    :dom-id "nav"
    :inner-html 
    (with-cl-who-string ()
      (:ul (dolist (page (the pages))
	     (htm (:li ((:span :class (format nil "clickable~a" 
					      (if (eql (the current-right-section) page) " selected" ""))
			       :onclick (the (gdl-ajax-call :function-key (if (the-object page child-pages) 
									      :toggle-children! 
									      :set-right-section-rp!)
							    :arguments (list page))))
			(str (the-object page link-title)))
		       ;;
		       ;; FLAG - make this recursive
		       ;;
		       (when (the-object page show-child-links?)
			 (htm (dolist (child (remove-if-not #'(lambda(page)
								(typep page 'base-site-sheet))
							    (the-object page children)))
				(htm (:li "&nbsp;&nbsp;&nbsp;&nbsp;"
					  ((:span :class 
						  (format nil "clickable ~a" 
							  (if (eql (the current-right-section) child) " selected" ""))
						  :onclick (the (gdl-ajax-call 
								 :function-key (if (the-object child child-pages) 
										   :toggle-children! 
										   :set-right-section-rp!)
									       :arguments (list child))))
					   (str (the-object child link-title))))))))))))))
   
   (news-section :type 'sheet-section
		 :dom-id "news"
		 :inner-html (with-cl-who-string ()
			       ((:div :class "news")
				(:h2 "Latest News") 
				(:h3 "2012-12-31")
				(:p
				 "New Genworks Website made live.")
				((:div :class "hr-dots")) 
				(:h3 "2012-12-15")
				(:p "Port of core Genworks GenDL
to " ((:a :href "http://www.clozure.com/clozurecl") "Clozure Common Lisp") " (CCL) is completed.")
				((:div :class "hr-dots"))

				(:h3 "2012-11-30")
				(:p "GenDL 1582 Professional and Student version is released.")
				((:div :class "hr-dots"))

				

				(if (eql self (the news))
				    (htm ((:p :class "more") 
					  ((:a :href (the (relative-url-to (the index-html url)))) "less...")))
				    (htm ((:p :class "more") 
					  ((:a :href (the (relative-url-to (the news url)))) "more...")))))))

   (support-section 
    :type 'sheet-section
    :dom-id "support"
    :inner-html (with-cl-who-string ()
		  ((:div :id "tickete" :class "support" :style "display:block")
		   (:img :src "/newsite-static/images/support.jpg"))
		  ((:div :id "address" :class "support-contact" :style "display:none")
		   ((:div :id "address-text" :class "support-contact-text") ;;:style "display:none")
		   "" :br
		   "255 E Brown Street, Suite 310" :br
		   "Birmingham, MI 48009 USA" :br
		   "+1 248-327-3253" :br
		   "info@genworks.com" :br))))
    
   (right-section :type 'sheet-section
		  :dom-id "right"
		  :js-to-eval (the right-section-js-to-eval)
		  :js-always-to-eval (the right-section-js-to-eval)
		  :inner-html (the right-section-inner-html))
   
   (footer-section :type 'sheet-section
		    :dom-id "footer"
		    :inner-html (with-cl-who-string ()
				  ((:div :id "copyright") 
				   (:h1 "Practical Tools for Generative Application Development")
				   (:p "")
				   (str (locale-string :copyright-genworks))
				   ((:div :id "footerline")))))))



