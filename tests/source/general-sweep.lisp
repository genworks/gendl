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

(define-object general-sweep-test (surf::general-sweep)
  
  :computed-slots
  ((profile-curve (the test-data-set (input-entities 1)))
   (guide-curve (the test-data-set (input-entities 0)))
   
   (v1-function #'(lambda(param)
		     (the guide-curve (tangent param))))
   
   (v2-function #'(lambda(param)
		    (cross-vectors
		     (the (face-normal-vector :right))
		     (the guide-curve (tangent param)))))
   
   (regression-test-data (multiple-value-list (the b-spline-data))))
  
  :objects
  ((test-data-set :type 'surf::nurbs-reader
		  :file-name (merge-pathnames "fuselage_fuselage-part_0_section-structure_frames_element_0.dat"
					      gdl-lift-utils::*lift-data-directory*))))




(register-test-definition 'general-sweep-test)
