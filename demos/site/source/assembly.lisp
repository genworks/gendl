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

(define-object assembly (base-site-sheet)
  
  :computed-slots
  ((main-sheet-body (the idx main-sheet-body))
   (title (the idx title))
   (body-class (the idx body-class))
   )
  
  
  :objects
  ((idx :type 'landing)
   
   (home :type 'index)
   
   (products :type 'products
	     :footer (the pricing-footer))
   
   (demos :type 'demos)
   
   (pricing :type 'pricing
	    :footer (the pricing-footer))

   (options :type 'options
	    :footer (the pricing-footer))
   
   (maintenance :type 'maintenance
		:footer (the pricing-footer))

   (glossary :type 'glossary
	     :footer (the pricing-footer))
   
   (licensing :type 'licensing
	      :footer (the pricing-footer))
   
   (security :type 'security
	     :footer (the pricing-footer))

   
   (opportunities :type 'opportunities)
   
   (new :type 'new)
   
   (people :type 'people)
   
   (contact :type 'contact)
   
   
   (pricing-footer :type 'sheet-section
		   :inner-html
		   (with-cl-who-string ()
		     (:h5 "Additional Genworks Product, Pricing and Support Information")
		     (:ul (:li ((:a :href (the options url) :title "Options" :id "options") "Options"))
			  (:li ((:a :href (the maintenance url) :title "Maintenance, Support, &amp; Onsite Training" :id "maintenance")
				"Maintenance, Support, &amp; Onsite Training"))
			  (:li ((:a :href (the licensing url) :title "Licenses" :id "licensing") "Licenses"))
			  (:li ((:a :href (the security url) :title "Security" :id "security") "Security"))
			  (:li ((:a :href (the glossary url) :title "Glossary" :id "glossary") "Glossary")))))
   
   ))



