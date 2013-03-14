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


(in-package :gwl-user)

(define-object tasty-test-part (base-ajax-sheet)

  :input-slots
  ((control-points (list (make-point 0 0 0)
			 (make-point 2 3.0 0.0) 
			 (make-point 4 2.0 0.0) 
			 (make-point 5 0.0 0.0) 
			 (make-point 4 -2.0 0.0) 
			 (make-point 2 -3.0 0.0) 
			 (make-point 0 0 0))))


  :objects
  ((model :type 'curves-model
	  :pass-down (control-points))))


(define-object curves-model (outline-specialization-mixin)
  
  :input-slots (control-points)
  
  :objects
  ((curves :type 'b-spline-curve
           :sequence (:size 5)
           :control-points (the control-points)
	   :degree (1+ (the-child :index))
	   :display-controls (list :line-thickness (* .3 (1+ (the-child index)))
				   :color (ecase (the-child index)
					    (0 :red) (1 :orange) (2 :yellow) (3 :green)
					    (4 :blue) (5 :red-violet)))))
  
  :objects
  ((notes :type 'general-note
	  :sequence (:size (length (the control-points)))
	  :start (nth (the-child index) (the control-points))
	  :strings (format nil "C~a" (the-child index)))))







  
