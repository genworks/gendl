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


(define-object separated-solid-test (base-object)
    :computed-slots
    ((regression-test-data (mapcar #'(lambda(brep)
				       (append (multiple-value-list (the-object brep precise-properties))
					       (the-object brep %curves-to-draw%)
					       (the-object brep %lines-to-draw%)))
				   (list-elements (the separated breps)))))
    
    :objects
    ((cylinder :type 'cylinder-solid
	       :radius 10 
	       :length 20)
     
     (plane :type 'planar-surface
	    :p00 (make-point -15 0 -15)
	    :p01 (make-point -15 0 15)
	    :p10 (make-point 15 0 -15)
	    :p11 (make-point 15 0 15))
     
     
     (separated-2 :type 'surf::separated-solid-2
		  :brep (the cylinder)
		  :other-brep (the plane)
		  :approximation-tolerance (* 0.001 (the-child brep adaptive-tolerance)))
     

     (separated :type 'separated-solid
		:brep (the cylinder)
		:other-brep (the plane)
		:approximation-tolerance (* 0.001 (the-child brep adaptive-tolerance))
		;;:cap-results? t
		)))


(register-test-definition 'separated-solid-test)
