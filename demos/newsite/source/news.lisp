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

(define-object news (base-site-sheet)

  :computed-slots
  ((title "Genworks International - News")
   (link-title  "News")

   (right-section-inner-html 
    (with-cl-who-string ()
      (:h2 "Prior News") 

      (:h3 "2012-03-28")
      (:p
       "New Genworks Website prototype is launched.")
      ((:div :class "hr-dots")) 
      (:h3 "2012-03-15")
      (:p
       "Integration and Testing of Genworks GDL 1581 (GenDL) with "
       ((:a :href "http://www.smlib.com") " SMLib 8.51 ")
       " is completed.")
      ((:div :class "hr-dots")) 
      (:H3 "2012-03-01")
      (:p "Genworks GDL (GenDL)included as a standard dist with " ((:a :href "http://www.quicklisp.org") "Quicklisp") ".")
      ((:div :class "hr-dots")) 
      (:h3 "2012-02-01")
      (:p "Port of core Genworks GDL (GenDL) to " ((:a :href "http://www.sbcl.org") "Steel Bank Common Lisp") " is completed.")
      (:h3 "2012-01-01")
      (:p "Genworks GDL 1581 (GenDL) Stable Beta release available, available on "
	  ((:a :href "http://www.franz.com/products/allegro") "Allegro CL 8.2") " and "
	  ((:a :href "http://www.lispworks.com/news/news31.html") "LispWorks 6.1"))
      (:h3 "2011-10-24")
      (:p ((:a :href "http://en.wikipedia.org/wiki/John_McCarthy_(computer_scientist)") "John McCarthy")
	  ", father of Lisp programming language family, exits the planet.")
      (:h3 "2011-10-23")
      (:p "Open-sourcing of GDL (GenDL) under "
	  ((:a :href "http://www.gnu.org/licenses/agpl-3.0.html") "Gnu Affero General Public License")
	  " is announced at "
	  ((:a :href "http://blip.tv/eclm") "ECLM 2011") ".")))))
