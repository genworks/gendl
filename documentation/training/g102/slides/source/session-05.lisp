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

(in-package :training-g102)

(define-object session-05 (slide-show-leaf)

  :computed-slots
  ((slide-data `((:title "Points" :bullet-points
			 ((:description "<i>Points</i> represent Cartesian Coordinates in 3-dimensional space")
			  (:description "A <i>Point</i> in GDL is represented as an Object
of type <i>3D-point</i>")
			  (:description "Because points are used so frequently, they 
have their own instantiation function, <i>make-point</i>:" :examples
((:code (make-point 0 0 0) :return-value "#(0.0 0.0 0.0)")
 (:code (make-point 10 20 30) :return-value "#(10.0 20.0 30.0)")))
			  (:description "The coordinates in a <i>3D-point</i> are just
numbers -- they have no inherent dimensions")))
(:title "Points, Cont'd" :bullet-points
	((:description "The <i>3D-point</i> object supports the following <i>accessor functions</i>:" :examples
		       ((:code (get-x (make-point 10 20 30)) :return-value 10.0)
			(:code (get-y (make-point 10 20 30)) :return-value 20.0)
			(:code (get-z (make-point 10 20 30)) :return-value 30.0)))
	 (:description "And you can get the 3-dimensional distance between two <i>3D-points</i>
with the function <i>3D-distance</i>:" :examples
((:code (3d-distance (make-point 0 0 0) (make-point 10 20 30)) :return-value 37.416573867739416)))))
(:title "Vectors" :bullet-points
	((:description "A <i>Vector</i> represents a <i>direction</i> and <i>magnitude</i>
 in 3-dimensional space")
	 (:description "A Vector in GDL is represented as an Object
of type <i>Precise-float-vector</i>")
	 (:description "Because <i>Vectors</i> are used so frequently, they 
have their own instantiation function, <i>make-vector</i>:" :examples
((:code (make-vector 0 0 0) :return-value #(0.0 0.0 0.0))
 (:code (make-vector 10 20 30) :return-value #(10.0 20.0 30.0))))
	 (:description "The coordinates in a <i>Vector</i>  are just
numbers -- they have no inherent dimensions")))
(:title "Vectors vs. Points" :bullet-points
	((:description "Technically, <i>3D-points</i> and <i>Vectors</i> 
are implemented the same way, thus they are interchangeable")
	 (:description "However, <i>semantically</i> you can
think of a <i>Vector</i> as an ``arrow-line'' in space with 
its tail at the Origin (i.e. #(0.0 0.0 0.0)) and its head
at the point represented in the vector")
	 (:description "Remember, a <i>Vector</i> has only
<i>direction</i> and <i>magnitude</i>, so although it
has its tail nominally at the Origin, it can be considered
as being <i>anywhere</i> in 3-dimensional space, as long
as it maintains the same <i>direction</i> and <i>magnitude</i>")))
(:title "Manipulating Vectors" :bullet-points
	((:description "Vectors support standard <i>vector arithmetic</i>,
including <i>add-vectors</i> and <i>subtract-vectors</i>:" :examples
((:code (add-vectors (make-vector 0 0 1) (make-vector 0 1 0)) :return-value #(0.0 1.0 1.0))
 (:code (subtract-vectors (make-vector 0 1 1) (make-vector 0 1 0)) :return-value
	#(0.0 0.0 1.0))))))

(:title "Creating Vectors from Points" :bullet-points
	((:description "Often, it is useful to obtain
a vector which points in the direction from one point to another" 
		       :description "For this purpose, we can exploit the
fact that Vectors and Points are interchangeable:" 
		       :examples ((:define-object point-demo)))))

(:title "Translating Points" 
	:bullet-points
	((:description "The function <i>Translate-along-vector</i> is commonly
used to compute a <i>3D-point</i> which is the result of 
translating a ``start point'' along a certain <i>Vector</i> by
a specified <i>distance</i>:")
	 (:description "<i>Translate-along-vector point vector distance</i>" :examples
		       ((:define-object translation-demo)))))


(:title "Using Points to specify a Child Part's Center" 
	:bullet-points
	((:description "You can pass the special input
<i>:center</i> into a child part to specify where its center
 (and thus, the entire part) should be positioned:" 
		       :examples
		       (
			(:define-object city-4))))))))

  :functions
  ((strings-for-display
nil
"Points and Vectors")))
