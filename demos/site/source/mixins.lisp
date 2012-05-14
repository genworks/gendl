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

(in-package :genworks.com)


(define-object base-site-sheet (base-ajax-sheet)

  :input-slots 
  ((footer (the empty-footer)))
  
  :computed-slots
  ( 
   (doctype-string nil)
   
   (header? t)
   (normal-layout? t)
   
   (custom-content nil)
   
   (additional-header-content 
    (with-cl-who-string ()
      ((:meta :name "description" 
	      :content "Genworks is a premier developer and vendor for a Generative Application Development 
system used for creating web-centric Knowledge-based Engineering and Business applications."))
      ((:meta :name "keywords" :content "Genworks, Knowledge-Based Engineering, KBE, GDL"))
      ((:meta :http-equiv "Content-Type" :content "text/html; charset=iso-8859-1"))
      ((:link :rel "stylesheet" :href "/site-static/css/main.css" :type "text/css" :media "screen"))
      (:comment "[if IE]><link rel=\"stylesheet\" href=\"/site-static/css/ie.css\" type=\"text/css\" media=\"screen\" /><![endif]")
      ((:script :type "text/javascript" :src "/site-static/js/niftycube.js"))))
   
   (main-sheet-body 
    (with-cl-who-string ()
      
      (when (the header?)
	(htm ((:div :id "header")
	      ((:a :href (the idx url) :title "home") 
	       ((:img :src "/site-static/images/logos/genworks-logohome.gif" 
		      :alt "Genworks International" :id "logo")))
	      ((:ul :id "nav") 
	       (:li ((:a :href (the home url) :id "home") "Home")) 
	       (:li ((:a :href (the products url) :id "products") "Products"))
	       (:li ((:a :href (the demos url) :id "demo") "Demos")) 
	       (:li ((:a :href (the pricing url) :id "pricing") "Pricing"))
	       (:li ((:a :href (the opportunities url) :id "opportunities") "Opportunities"))
	       (:li ((:a :href (the new url) :id "new") "What's New"))
	       (:li ((:a :href (the people url) :id "people") "People"))
	       (:li ((:a :href (the contact url) :id "contact") "Contact Us"))))))
      
      (when (the custom-content) (str (the custom-content)))
      
      (when (the normal-layout?)
	(htm
	 ((:div :class "content")
	  (when (the column-left inner-html)
	    (htm ((:div :class "column_left_top")
		  ((:div :class "column_left_bottom")
		   ((:div :class "column_left") 
		    (str (the column-left main-div)))))))
       
	  ((:div :class "column_center") 
	   (str (the column-center main-div)))
       
	  (when (the column-right inner-html)
	    (htm ((:div :class "column_right_top")
		  ((:div :class "column_right_bottom")
		   ((:div :class "column_right") 
		    (str (the column-right main-div)))))))
       
	  (when (the footer inner-html)
	    (htm ((:p :class "clear") "&nbsp;")
		 ((:div :class :footer)
		  (str (the footer main-div)))))))))))
  
  :objects
  ((column-left 
    :type 'sheet-section
    :inner-html
    (with-cl-who-string ()
      (:h3 "Connections")
      (:ul
       (:li "Suppliers"
	    (:ul (:li ((:a :href "http://www.franz.com" :title "Franz Inc.") "Franz Inc"))
		 (:li ((:a :href "http://www.lispworks.com" :title "Lispworks Ltd") "Lispworks Ltd"))
		 (:li ((:a :href "http://www.smlib.com" :title "Solid Modeling Solutions") "Solid Modeling Solutions"))))
       (:li "Resellers"
	    (:ul (:li ((:a :href "http://www.liberatinginsight.com" :title "Liberating Insight") "Liberating Insight"))
		 (:li ((:a :href "http://www.transtecsolutions.de" :title "Trans Tech Solutions") "Trans Tech Solutions"))
		 (:li ((:a :href "http://www.whiteboxlearning.com" :title "Whitebox Learning") "Whitebox Learning"))))
       (:li "Other Resources
						"
	    (:ul (:li ((:a :href "/downloads/gdl-pamphlet.pdf" :title "GDL Pamphlet") "GDL Pamphlet"))
		 (:li ((:a :href "/downloads/customer-documentation/index.xml" :title "Genworks Documentation") 
		       "GDL Documentation"))
		 (:li ((:a :href "/dl" :title "Downloads") "Downloads and Patch Logs"))
		 (:li
		  ((:a :href "/downloads/100806 - Thesis Presentation Maarten van Hoek.swf" 
		       :title "Maarten van Hoek Presentation")
		   "Maarten van Hoek Presentation"))
		 (:li ((:a :href "/downloads/kbe2008.pdf" :title "Wichita SAE Paper") "Wichita SAE Paper"))
		 (:li ((:a :href "/downloads/kbe2007.pdf" :title "Belfast AIAA Paper") "Belfast AIAA Paper"))
		 (:li
		  ((:a :href "http://www.franz.com/success/customer_apps/knowledge_mgmt/genworks.lhtml" 
		       :title "Industry Success Stories")
		   "Industry Success Stories"))
		 (:li ((:a :href "http://www.smlib.com/nl0804.html#link02" :title "SMS Newsletter") 
		       "SMS Newsletter"))
		 (:li ((:a :href (the glossary url) :title "Glossary") "Glossary"))))
       
       
       (:li "Technical Standards
						"
	    (:ul
	     (:li
	      ((:a :href "http://www.franz.com/support/documentation/8.0/ansicl/ansicl.htm" :title "ANSI CL Standard Compliance")
	       "ANSI CL Standard Compliance"))
	     (:li ((:a :href "http://en.wikipedia.org/wiki/NURBS" :title "NURBS Standard Compliance") 
		   "NURBS Standard Compliance")))))
      ((:p :class "clear") "&nbsp;")))
   
   (column-center :type 'sheet-section
		  :inner-html (with-cl-who-string ()
			       (:p "This section intentionally left blank.")))

   
   (empty-footer :type 'sheet-section
		 :inner-html nil)
   
   (column-right 
    :type 'sheet-section
    :inner-html
    (with-cl-who-string ()
      (:h3 "Interactive Demos") 
      (:p "Click Links Below To See GDL in Action")
      (:ul (:li ((:a :href (the demos url) :title "Robot") 
		 ((:img :src "/site-static/images/icons/Robothome.gif" :alt "Robot")) :br "Robot"))  
	   (:li ((:a :href (the demos url) :title "Cockpit") 
		 ((:img :src "/site-static/images/icons/cockpit.gif" :alt "Cockpit")) :br "Cockpit"))  
	   (:li ((:a :href (the demos url) :title "Nurbs") 
		 ((:img :src "/site-static/images/icons/Nurbs.gif" :alt "Nurbs")) :br "Nurbs")) 
	   (:li ((:a :href (the demos url) :title "Rocket Tank") 
		 ((:img :src "/site-static/images/icons/Rocket-Tank.gif" :alt "Rocket Tank")) :br "Rocket Tank"))
	   (:li ((:a :href (the demos url) :title "School Bus") 
		 ((:img :src "/site-static/images/icons/School-Bus.gif" :alt "School Bus")) :br "School Bus"))  
	   (:li ((:a :href (the demos url)  :title "Bracket") 
		 ((:img :src "/site-static/images/icons/bracket.gif" :alt "Bracket")) :br "Bracket")))
      ((:p :class "clear") "&nbsp;")))))
