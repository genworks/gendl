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


(define-object joined-surfaces-test (joined-surfaces) 

  :computed-slots
  ((surface (the surf-A))
   (other-surface (the surf-B))
   
   (regression-test-data (multiple-value-list (the b-spline-data))))
  
  :objects
  ((surf-A :type 'rectangular-surface
	   :length 10
	   :width 10)
   
   (surf-B :type 'rectangular-surface
	   :center (make-point 10 0 0 )
	   :length 10
	   :width 10)))


(register-test-definition 'joined-surfaces-test)
