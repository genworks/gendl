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


(define-object demos (base-site-sheet)

  :computed-slots
  ((title "Genworks International - Demos")
   (link-title  "Demos")
   
   #+nil
   (right-section-inner-html 
    (with-cl-who-string ()
      (:h2 "Our Products") ((:div :id "welcome"))
      (:ul
       (:li ((:a :href (the descriptions url)) "Descriptions"))
       (:li ((:a :href (the licensing url)) "Licensing"))
       (:li ((:a :href (the configurator url)) "Configurator"))))))

  
  :objects
  ((twenty-four :type 'assembly-24
		:pass-down (respondent))))


#+nil
(define-object demos (base-site-sheet)

  :computed-slots
  ((title "Genworks International - Demos")
   (link-title  "Demos")

   (right-section-inner-html
    (with-cl-who-string ()
      (:H2 "Demos") (:H3 "Aerospace")
      ((:DIV :ID "profile")
       ((:DIV :ID "d-corp")
	((:DIV :ID "d-corp-img") "
            Aero Demo 1")
	(:P "Under construction "))
       ((:DIV :ID "d-indu")
	((:DIV :ID "d-indu-img") "
            Aero Demo 2")
	(:P "Under construction "))
       ((:P :CLASS "more") ((:A :HREF "") "View Details")))
      (:H3 "Wind Energy")
      ((:DIV :ID "profile")
       ((:DIV :ID "d-corp")
	((:DIV :ID "d-corp-img") "
	            Wind Demo 1")
	(:P "Under construction"))
       ((:DIV :ID "d-indu")
	((:DIV :ID "d-indu-img") "
	            Wind Demo 2")
	(:P "Under construction"))
       ((:DIV :CLASS "clear"))
       ((:P :CLASS "more") ((:A :HREF "") "View Details")))))))

