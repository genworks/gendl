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

(define-object assembly (base-site-sheet)
  

  :computed-slots
  ((title "Genworks International - Welcome")
   
   (lang (the language choice value))
   
   (right-section-inner-html (the current-right-section right-section-inner-html))
   (right-section-js-to-eval (the current-right-section right-section-js-to-eval))
   (right-section-js-always-to-eval (the current-right-section right-section-js-to-eval))

   (link-title "Home")
   
   (pages (the children)))
  
  :trickle-down-slots (pages lang)

  :hidden-objects
  ((news :type 'news)
   
   (robot :type 'robot:assembly))

  :objects
  ((index-html :type 'index-html
	       :respondent self)
   (products :type 'products
	     :respondent self)
   ;;(services :type 'services)
   
   (documentation :type 'documentation-and-educational
		  :respondent self)
   
   (demos :type 'demos
	  :respondent self)

   (downloads :type 'downloads
	      :respondent self)

   (people :type 'people
	   :respondent self)
   (contact-us :type 'contact-us
	       :respondent self)
   
   (language :type 'language
	     :respondent self)
   
   ))