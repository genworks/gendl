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

(in-package :genworks.demos.bus)

(define-object axle (base-object)

  :input-slots
  (kingpin-inclination-angle)

  :computed-slots
  ((kingpin-vectors (list :left (the (:kingpins 0) (:face-normal-vector :front))
			  :right
			  (the (:kingpins 1) (:face-normal-vector :front)))))

  :objects
  ((kingpins :type 'cylinder
	     :sequence (:size 2)
	     :radius 1.5
	     :length (the :height)
	     :center (the (:face-center (ecase (the-child :index) (0 :front) (1 :rear))))
	     :transformation-matrix 
	     (alignment :front
			(rotate-vector-d (the
					     (:face-normal-vector :top))
					 (funcall
					  (ecase
					      (the-child :index)
					    (0 #'identity)
					    (1 #'-))
					  (the
					      :kingpin-inclination-angle))
					 (the
					     (:face-normal-vector
					      :left))))
	     :orientation (the-child :transformation-matrix)
	     :display-controls (list :color :red))
   
   
   (extrusion :type 'global-filleted-polygon-projection
	      :radius-list (list 5 5 5 5 1 1 5 5 5 5 1 1)
	      :projection-depth (the :width)
	      :projection-vector (the (:face-normal-vector :right))
	      :display-controls (list :color :blue-steel :shininess 7 )
	      :vertex-list 
	      (let* ((d1 9)
		     (d2 8)
		     (d3 5)
		     (d4 7)
		     (top-edge
		      (let* ((a (the (:vertex :front :top :left)))
			     (b (translate a :rear d1))
			     (c (translate b :down d2 :rear d3))
			     (d
			      (translate c :rear
					 (- (the :length) (twice (+ d1 d3)))))
			     (e (translate d :up d2 :rear d3))
			     (f (translate e :rear d1)))
			(list a b c d e f)))
		     (bottom-edge
		      (mapcar #'(lambda (point) (translate point :down d4))
			      top-edge)))
		(append top-edge (nreverse bottom-edge))))))
