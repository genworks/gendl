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

(define-object inter-seat-spacing (gwl-rule-object)

  :input-slots
  (max-reclined-angle
   usable-cabin-width
   number-of-rows)

  :computed-slots
  ((rule-title "Seat Spacing")
   (rule-description "Distance from front of one seat to front of the next.")
   (rule-result (number-format (the :result) 2))
   (x-extent-typical (the :seat-at-max :x-extent))
   (clearance-extent-typical (the :seat-at-max :clearance-extent))
   (x-max-typical (the :seat-at-max :x-max))
   (result (let ((extra-space
		  (- (the :usable-cabin-width)
		     (* (the :x-extent-typical) (the :number-of-rows))
		     (the :x-max-typical))))
	     (+ (the :x-extent-typical) (/ extra-space (the :number-of-rows))))))

  :hidden-objects
  ((seat-at-max :type 'seat
		:reclined-angle (the :max-reclined-angle))))
