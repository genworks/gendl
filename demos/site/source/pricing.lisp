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


(define-object pricing (base-site-sheet)

  :computed-slots ((body-class "pricing"))
  
  :objects
  (

   (column-left 
    :type 'sheet-section
    :inner-html nil)
   

   (column-right 
    :type 'sheet-section
    :inner-html nil)
   
   
   (column-center
    :type 'sheet-section
    :inner-html
    (with-cl-who-string ()
      ((:div :class "content pricing") 
       (:h2 "Pricing")
       (:h5 "GDL products offer a savings of " (:strong "up to 75% or more") " as compared with legacy KBE tools.") 
       (:h5 "We also offer an alternative 12- or 24-month Lease with Option to Purchase.")
       (:h5 "Please email us at " ((:a :href "mailto:info@genworks.com") "info@genworks.com") " for a specific quote."))))))
