;;
;; Copyright 2002, 2009 Genworks International and Genworks BV 
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

(in-package :genworks.demos.bus)

(define-object interior (application-mixin)

  :input-slots
  (firewall-base
   length
   width
   height
   
   (number-of-rows 10 :settable))

  
  :computed-slots
  ((ui-display-list-objects (list (the :sections)))
   
   (reclined-angle 20 :settable)
   (max-reclined-angle 30 :settable)
   (minimum-inter-seat-clearance 7 :settable)
   (use-local-box? nil))

  :objects
  ((sections :type 'seating-section
	     :body-reference-points 
	     (list :left
		   (translate (the :firewall-base) :front
			      (half (the :length)) :right
			      (the :width))
		   :right
		   (translate (the :firewall-base) :rear
			      (half (the :length)) :right
			      (the :width)))
	     :usable-cabin-width (the :width)
	     :pass-down (number-of-rows reclined-angle max-reclined-angle
			 minimum-inter-seat-clearance))))
