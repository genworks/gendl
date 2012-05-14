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


(define-object security (base-site-sheet)

  :input-slots (pricing-footer)

  :computed-slots ((body-class "products security"))
  
  :objects
  (
   
   (column-left 
    :type 'sheet-section
    :inner-html nil)
   
   (column-right 
    :type 'sheet-section
    :inner-html nil)


   
   (column-center :type 'sheet-section
		  :inner-html
		  (with-cl-who-string ()
		    ((:div :class "content") 
		     (:h2 "Security") 
		     (:h5 "Source Code Escrow")
		     (:p "Genworks offers Enterprise customers the option of entering
into a Source Code Escrow agreement administered by "
			 ((:a :href "http://www.softescrow.com/") "Lincoln-Parry")
			 ", to protect your investment in GDL application 
development against all eventualities. Please contact us for details. ")
		     
		     )))))


