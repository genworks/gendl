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

(define-object demos (base-site-sheet)

  :computed-slots
  ((body-class "demo demos"))
  
  :objects
  ((column-left 
    :type 'sheet-section
    :inner-html nil)
   
   (column-right 
    :type 'sheet-section
    :inner-html nil)
   
   (column-center
    :type 'sheet-section
    :inner-html
    (with-cl-who-string ()
      ((:div :class "content")
       ((:p :class "demo_intro")
	"Please check back for additional demos, we plan to have more online in the near future. 
The following applications demonstrate a small portion of the capabilities of GDL/GWL. Although 
the current sample applications display geometric output using a standard web page template, 
GDL/GWL is also appropriate for any type of customized web application, whether involving geometry or not.")
       
       ((:div :class "demo_description")
	(:p ((:a :href "/demos/robot" :title "Simplified Android Robot") 
	     ((:img :src "/site-static/images/icons/Robothome.gif" :alt "Robot"))))
	(:p ((:a :href "/demos/robot" :title "Simplified Android Robot") "Simplified Android Robot"))
	(:p
	 "This very basic demo allows the user to manipulate and view a GDL model of the classic geometric 
example found in computer graphics literature such as Computer Graphics: Principles and Practice by Foley, 
Van Dam, Feiner, and Hughes."))
       
       ((:div :class "demo_description")
	(:p ((:a :href "/demos/bus" :title "Bus Configurator") 
	     ((:img :src "/site-static/images/icons/School-Bus.gif" :alt "School Bus"))))
	(:p ((:a :href "/demos/bus" :title "Bus Configurator") "School Bus Configurator "))
	(:p
	 "This rudimentary configurator allows you to modify and explore \"what-if\" scenarios with the steering 
system and interior seating configuration of a traditional North American school bus. Note that you can visit 
each sub-node (Chassis, Body, and Interior) to modify its inputs and see graphics, rules, reports, etc. specific 
to that node.")))))))
