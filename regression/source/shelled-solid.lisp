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

(define-object shelled-solid-test (base-object)
  
  :input-slots
  ((points-data '(((0 0 0) (0 1 0) (0 2 0) (0 3 0))
		  ((1 0 1) (1 1 1) (1 2 1) (1 3 1))
		  ((2 0 2) (2 1 2) (2 2 2) (2 3 2))
		  ((3 0 1) (3 1 1) (3 2 1) (3 3 1))
		  ((4 0 0) (4 1 0) (4 2 0) (4 3 0)))))
  
  :computed-slots ((surf-points (mapcar #'(lambda(row)
					    (mapcar #'(lambda(point)
							(apply-make-point point))
						    row))
					(the points-data)))
		   

		   (regression-test-data (append (multiple-value-list 
						  (the shell precise-properties))
						 (the shell %curves-to-draw%)
						 (the shell %lines-to-draw%)))
		   
		   
		   ;;(regression-test-data (the shell %native-brep%))
		   
		   
		   
		   )
  
  :objects
  ((box :type 'box-solid
	:display-controls (list :line-thickness 3 :color :green)
	:length 10 :width 10 :height 10)
   
   (fitted :type 'fitted-surface
	   :points (the surf-points))

   
   (b-spline :type 'b-spline-surface
	     :control-points (the surf-points))
   
   (shell :type 'shelled-solid
	  :tolerance 0.1
	  :display-controls (list :isos (list :n-v 8 :n-u 8))
	  :brep (the  b-spline)
	  :distance .1)
   

   (offset :type 'offset-solid
	   :sequence (:size 10)
	   :tolerance 0.001
	   :display-controls (list :isos (list :n-v 8 :n-u 8))
	   :brep (the  b-spline)
	   :distance (* (1+ (the-child index)) .1))))


;;(register-test-definition 'shelled-solid-test)
