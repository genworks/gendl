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



(in-package :gdl-lift-tests)


(define-object intersected-solid-test (intersected-solid)
    :computed-slots
    ((brep (the cylinder))
     (other-brep (the box))
     
     (regression-test-data (append (multiple-value-list (the precise-properties))
				   (the %curves-to-draw%)
				   (the %lines-to-draw%)
				   (multiple-value-list (the intersect precise-properties))
				   (the intersect %curves-to-draw%)
				   (the intersect %lines-to-draw%))))
    
    :objects
    ((cylinder :type 'cylinder-solid
	       :radius 10 
	       :length 20)
     
     (box :type 'box-solid
	  :length 10
	  :height 10
	  :width 30)
     
     (plane :type 'planar-surface
	    :p00 (make-point -15 0 -15)
	    :p01 (make-point -15 0 15)
	    :p10 (make-point 15 0 -15)
	    :p11 (make-point 15 0 15))
     
     (intersect :type 'intersected-solid
		:brep (the cylinder)
		:other-brep (the plane brep))))


(register-test-definition 'intersected-solid-test)
