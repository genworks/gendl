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

(define-object inter-seat-clearance-check (gwl-rule-object)

  :input-slots
  (inter-seat-spacing
   clearance-extent-typical
   value)

  :computed-slots
  ((rule-title "Legroom")
   (rule-description "Distance from front of one seat to back of the seat fore of it.")
   (rule-result (number-format (the result) 2))
   (violated? (< (the result) (the value)))
   (result (- (the inter-seat-spacing) 
	      (the clearance-extent-typical)))))
