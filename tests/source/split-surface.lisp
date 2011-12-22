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

(define-object split-surface-test (base-object)
  
  :computed-slots ((projection-point (make-point 10 0 3))
		   (projection-vector (make-vector 1 0 0))
		   (u-or-v :u :settable)
		   (keep-side :left :settable)
		   
		   (regression-test-data (multiple-value-list (the split b-spline-data))))
  
  :objects
  ((test-surface :type 'test-b-spline-surface
		 :display-controls nil)

   
   (split :type 'split-surface
	  :display-controls (list :color :red :line-thickness 3)
	  :surface-in (the test-surface)
	  :pass-down (keep-side u-or-v projection-point projection-vector))))


(register-test-definition 'split-surface-test)
