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


(define-object box-faces (base-object)

  :objects 
  ((top-face :type 'planar-surface
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


(define-object merged-solid-test (merged-solid)
  :computed-slots
  ((length 20) 
   (width 10) 
   (height 10)
   (sew-and-orient? nil)
   (other-brep (list (the box-faces top-face) (the box-faces bottom-face)
		     (the box-faces right-face) (the box-faces left-face)
		     (the box-faces rear-face) (the box-faces front-face)))
   
   (regression-test-data (append (multiple-value-list (the precise-properties))
				 (the %curves-to-draw%)
				 (the %lines-to-draw%))))
  
  :hidden-objects
  ((box :type 'box)
   
   (box-faces :type 'box-faces)))




(register-test-definition 'merged-solid-test)


(define-object merged-solid-test-bt (surf::boolean-tree)
  :computed-slots
  ((length 20) 
   (width 10) 
   (height 10)
   (sew-and-orient? nil)
   (breps (list (the box-faces top-face brep) (the box-faces bottom-face brep)
		(the box-faces right-face brep) (the box-faces left-face brep)
		(the box-faces rear-face brep) (the box-faces front-face brep)))
   
   (regression-test-data (append (multiple-value-list (the precise-properties))
				 (the %curves-to-draw%)
				 (the %lines-to-draw%))))
  
  :hidden-objects
  ((box :type 'box)
   
   (box-faces :type 'box-faces)))


(register-test-definition 'merged-solid-test-bt)

;;
;; FLAG -- try a sewn-solid or stitched-solid.
;;

#+nil
(define-object merged-solid-test (merged-solid)
  
  :computed-slots
  ((width 20)
   (length 20)
   (height 20)
   (sew-and-orient? nil)
   (brep (the plane1 brep))
   (other-brep (the plane2 brep)))
  
  :objects
  ((plane1 :type 'planar-surface
	   :width 20
	   :length 20
	   :p00 (translate (the center) :left (half (the width)) :rear (half (the length)))
	   :p01 (translate (the center) :left (half (the width)) :front (half (the length)))
	   :p10 (translate (the center) :right (half (the width)) :rear (half (the length)))
	   :p11 (translate (the center) :right (half (the width)) :front (half (the length))))
   
   (plane2 ::type 'planar-surface
	   :width 20
	   :height 20
	   :p00 (translate (the center) :left (half (the width)) :bottom (half (the height)))
	   :p01 (translate (the center) :left (half (the width)) :top (half (the height)))
	   :p10 (translate (the center) :right (half (the width)) :bottom (half (the height)))
	   :p11 (translate (the center) :right (half (the width)) :top (half (the height))))))
