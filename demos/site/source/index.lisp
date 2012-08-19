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


(define-object index (base-site-sheet)
  
  :computed-slots
  
  ((title "Genworks International")
   (body-class "home"))

  
  :objects
  ((column-center :type 'sheet-section
		  :inner-html
		  (with-cl-who-string ()
		    (:p "Preview our upcoming " ((:a :href "http://www.genworks.com/newsite") (:i "New Site here")) ".")

		    ((:p :id "genworks_title") 
		     ((:img :src "/site-static/images/icons/Genworks_title.gif" :alt "Genworks")))
		    (:p
		     "is a premier developer and vendor for a Generative Application Development system used for 
creating web-centric Knowledge-based Engineering and Business applications.")
		    (:p "Our groundbreaking software sets new standards for ease and speed of development 
and seamless web-based deployment for geometry-intensive, engineering, and business solutions.")
		    (:p "Genworks GDL software comes in a variety of configurations based on your needs. Check out our "
			((:a :href (the products url) :title "Products") "Products") " page for available packages and licenses.")
		    (:p "Our Open Architecture allows great flexibility for connecting / integrating with other software applications.")
		    ((:p :class "footnote") "Check out a demo or click on a link to learn more.")))))

