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


(define-object dual-blend-surface-test (surf::dual-blend-surface)
  
  :computed-slots
  ((regression-test-data (multiple-value-list (the b-spline-data))))
  
  :objects
  ((surf-1-top :type 'linear-curve
	       :hidden? t
	       :start (make-point -5 -5 0)
	       :end (make-point 5 -5 0))
    
   (surf-1-bottom :type 'linear-curve
		  :hidden? t
		  :start (make-point -7 -10 -2)
		  :end (make-point 7 -10 -2))
    
   (surface-1 :type 'ruled-surface
	      :curve-1 (the surf-1-top)
	      :curve-2 (the surf-1-bottom))
    
   (curve-1 :type 'iso-curve
	    :display-controls (list :color :red :line-thickness 4)
	    :surface (the surface-1)
	    :parameter 0
	    :u-or-v :v)
    
    
   (surf-2-bottom :type 'linear-curve
		  :hidden? t
		  :start (make-point -5 5 0)
		  :end (make-point 5 5 0))
    
   (surf-2-top :type 'linear-curve
	       :hidden? t
	       :start (make-point -7 10 2)
	       :end (make-point 7 10 2))
    
   (surface-2 :type 'ruled-surface
	      :curve-1 (the surf-2-bottom)
	      :curve-2 (the surf-2-top))
    
   (curve-2 :type 'iso-curve
	    :display-controls (list :color :blue :line-thickness 4)
	    :surface (the surface-2)
	    :parameter 0
	    :u-or-v :v)))

(register-test-definition 'dual-blend-surface-test)
