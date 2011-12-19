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


(define-object stitched-solid-test (stitched-solid)
  :computed-slots
  ((length 20) 
   (width 10) 
   (height 10)
   
   (faces-in (list (the top-face) (the bottom-face)
		   (the right-face) (the left-face)
		   (the rear-face) (the front-face)))
   
   (regression-test-data (append (multiple-value-list (the precise-properties))
				 (the %curves-to-draw%)
				 (the %lines-to-draw%))))
  
  :hidden-objects
  ((box :type 'box)
   
   (top-face :type 'planar-surface
	     :p00 (the box (vertex :top :right :rear))
	     :p01 (the box (vertex :top :right :front))
	     :p10 (the box (vertex :top :left :rear))
	     :p11 (the box (vertex :top :left :front)))
   
   
   (bottom-face :type 'planar-surface
		:p00 (the box (vertex :bottom :right :rear))
		:p01 (the box (vertex :bottom :right :front))
		:p10 (the box (vertex :bottom :left :rear))
		:p11 (the box (vertex :bottom :left :front)))
   
   
   (right-face :type 'planar-surface
		:p00 (the box (vertex :top :right :rear))
		:p01 (the box (vertex :top :right :front))
		:p10 (the box (vertex :bottom :right :rear))
		:p11 (the box (vertex :bottom :right :front)))
   
   
   (left-face :type 'planar-surface
		:p00 (the box (vertex :top :left :rear))
		:p01 (the box (vertex :top :left :front))
		:p10 (the box (vertex :bottom :left :rear))
		:p11 (the box (vertex :bottom :left :front)))
   
   
   (rear-face :type 'planar-surface
		:p00 (the box (vertex :top :right :rear))
		:p01 (the box (vertex :top :left :rear))
		:p10 (the box (vertex :bottom :right :rear))
		:p11 (the box (vertex :bottom :left :rear)))
   
   (front-face :type 'planar-surface
	       :p00 (the box (vertex :top :right :front))
	       :p01 (the box (vertex :top :left :front))
	       :p10 (the box (vertex :bottom :right :front))
	       :p11 (the box (vertex :bottom :left :front)))))



;(register-test-definition 'stitched-solid-test)
