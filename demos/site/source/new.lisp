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


(define-object new (base-site-sheet)

  :computed-slots ((body-class "new"))
  
  :objects
  ((column-center 
    :type 'sheet-section
    :inner-html
    (with-cl-who-string ()
      (:h2 "What's New for May, 2011")
      (:p (:i "Stay tuned for additional Demos")) 
      
      (:p "Genworks constantly enhances the GDL product, as well as incorporating the latest stable versions of GDL's foundational supplied components. Below is a sampling of some the more recent updates. For a detailed and comprehensive specification of GDL's capabilities, please request an evaluation copy of GDL or of the GDL Documentation Set.")
      
      (:ul
      (:li "Core Language Features"
	   (:p "Please visit The Franz " 
	       ((:a :href "http://www.franz.com/support/tech_corner/") "Tech Corner") 
	       ", Lispworks "
	       ((:a :href "http://www.lispworks.com\"") "Latest News") 
	       ", and the SMS " 
	       ((:a :href "http://www.smlib.com/") "site") 
	       " for the latest information from these vendors."))
      
      (:li "Enhanced TaSty web-based development interface")
      
      (:li "Improved geometry primitives")
      
      (:li "New odd-numbered Development Branch (currently 1581) for trying cutting-edge GDL features")
      
      (:li "Increasing Customer Base") 
      (:ul 
       (:li "Welcome to Fokker Aerostructures in the Netherlands! ")

       (:li "Whitebox Learning again scaling up their GDL-powered Dragster and Structures applications and adds Green Car application")
      
      (:li "Technical University of Delft in the Netherlands expands their use of GDL for 
Advanced Design Methods coursework, with 100 new Students learning GDL this month! ")
      
      (:li "Allegro Common Lisp 8.2")
      (:li "LispWorks 6 with Symmetric Multiprocessing")))))))
