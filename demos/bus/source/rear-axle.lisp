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

(define-object rear-axle (base-object)

  :computed-slots
  ((wheel-centers (list :left
			(translate (the :center) :front (half (the :length)))
			:right
			(translate (the :center) :rear (half (the :length))))))

  :objects
  ((center-sphere :type 'sphere
		  :radius 9)
   (cylinder :type 'c-cylinder
	     :start (getf (the :wheel-centers) :left)
	     :end (getf (the :wheel-centers) :right)
	     :radius 2.5)))
