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

(in-package :bus)


#+nil
(define-object fleet (base-object)
  
  
  :input-slots 
  ((csv-file (merge-pathnames "fleet.csv" *data-directory*)))
  
  
  :computed-slots
  ((csv-data (fare-csv:read-csv-file (the csv-file)))
   
   (active-data-rows (remove-if #'(lambda(row) (the-object row in-maintenance?))
			       (list-elements (the data-rows)))))

  
  :hidden-objects
  ((data-rows :type 'bus-data-row
	      :sequence (:size (length (the csv-data)))
	      :data-list (nth (the-child index) (the csv-data))))
  
  :objects
  ((busses :type 'assembly
	   :sequence (:size (length (the active-data-rows)))
	   :center (translate (the center) :rear (* (the-child index) 150))
	   ;;
	   ;; Data-row is a "psuedo-input" -- it is not actually an input-slot for assembly, but used only
	   ;; here for convenience so that the wheelbase and number-of-rows can refer to (the-child data-row ...)
	   ;;
	   :data-row (nth (the-child index) (the active-data-rows))
	   :wheelbase (the-child data-row wheelbase)
	   :number-of-rows (the-child data-row number-of-seat-rows))))


#+nil
(define-object bus-data-row ()
  
  :input-slots (data-list)
  
  :computed-slots ((wheelbase (first (the data-list)))
		   (number-of-seat-rows (second (the data-list)))
		   (in-maintenance? (string-equal (third (the data-list)) "yes"))))
  
