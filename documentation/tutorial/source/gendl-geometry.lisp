;;
;; Copyright 2002, 2009, 2012 Genworks International
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

(in-package :gendl-doc)

(defparameter *gendl-geometry*
  `((:chapter :title "Working with Geometry in GDL")
    (:p "Although Genworks GDL is a powerful framework for all kinds
  of general-purpose computing, one of its particular strong points is
  generating geometry and processing geometric entities. Geometric
  capabilities are provided by a library of "
	(:emph "low-level primitives")
	", or LLPs. LLPs are pre-defined GDL objects which you can
	extend by ``mixing in'' with your own definitions, and/or
	instantiate as child objects in your definitions.")

    (:p "The names of the geometric LLPs are in the "
	(:texttt ":geom-base")
	" package, and here are some examples:"
	((:list :style :itemize)
	 (:item (:texttt "base-coordinate-system") " provides an empty 3D Cartesian coordinate system"
		(:footnote (:texttt "base-coordinate-system") " is also known by its legacy name "
			   (:texttt "base-object") "."))

	 (:item "Simple 2-dimensional primitives include "
	   (:texttt "line")
	   ", "
	   (:texttt "arc")
	   ", and " (:texttt "ellipse") ".")

	 (:item "Simple 3-dimensional primitives include "
	   (:texttt "box")
	   ", "
	   (:texttt "sphere")
	   ", and " 
	   (:texttt "cylinder")
	   ".")

	 (:item "Advanced 3-dimensional primitives (which depend on optional add-on Geometry Kernel module) include "
	   (:texttt "b-spline-curve")
	   ", "
	   (:texttt "b-spline-surface")
	   ", and "
	   (:texttt "merged-solid") "."))

	"This chapter will cover the default coordinate system of GDL as well as the built-in simple 2D and 3D LLPs. Chapter "
	(:ref "chap:workingwithsurfacesandsolids")
	" will cover the advanced Surfaces and Solids primitives.")


    ((:section :title "The Default Coordinate System in GDL")
     (:p "GDL's default coordinate system comes with the standard mixin "
	 (:texttt "base-coordinate-system")
	 " and represents a standard three-dimensional Cartesian
	 Coordinate system with X, Y, and Z dimensions.")


     ((:image-figure :image-file "coord-sys-tri.png" :caption "Coordinate System in Trimetric View" 
			  :width "3in" :height "3in"
			  :label "fig:coord-sys-tri"))


     ((:image-figure :image-file "coord-sys-front.png" :caption  "Coordinate System in Front View"
			  :width "3in" :height "3in"
			  :label "fig:coord-sys-front"))

     ((:image-figure :image-file "coord-sys-top.png" :caption "Coordinate System in Top View"
			  :width "3in" :height "3in"
			  :label "fig:coord-sys-top"))
     

     ((:image-figure :image-file "coord-sys-labeled-faces.png" :caption "Coordinate System with Symbolically Labeled Faces"
			  :width "3in" :height "3in"
			  :label "fig:coord-sys-labeled-faces"))
     

     (:p "Figure " (:ref "fig:coord-sys-tri") " shows the coordinate system in a 3D Trimetric view.")



     (:p "Figure " (:ref "fig:coord-sys-front") " shows the coordinate system in a Front View.")


     
     (:p "Figure " (:ref "fig:coord-sys-top") " shows the coordinate system in a Top View.")

     (:p "Figure " (:ref "fig:coord-sys-labeled-faces") " shows each face of the reference box labeled with its symbolic direction:"
	 ((:list :style :itemize)
	  (:item (:texttt "Right") " for the " (:textbf "positive X") " direction")
	  (:item (:texttt "Left") " for the " (:textbf "negative X") " direction")
	  (:item (:texttt "Rear") " for the " (:textbf "positive Y") " direction")
	  (:item (:texttt "Front") " for the " (:textbf "negative Y") " direction")
	  (:item (:texttt "Top") " for the " (:textbf "positive Z") " direction")
	  (:item (:texttt "Bottom") " for the " (:textbf "negative Z") " direction"))))


    ((:section :title "Building a Geometric GDL Model from LLPs")

     "The simplest geometric entity in GDL is a "
     (:texttt "box")
     ", and in fact all entities are associated with an imaginary "
     (:emph "reference box")
     " which shares the same slots as a normal box. The "
     (:texttt "box")
     " primitive type in GDL inherits its inputs from "
     (:texttt "base-coordinate-system")
     ", and the fundamental inputs are:"
     ((:list :style :itemize)
      (:item (:texttt "center") " Default: " (:texttt "#(0.0 0.0 0.0)"))
      (:item (:texttt "orientation") " Default: " (:texttt "nil"))
      (:item (:texttt "height") " Default: " (:texttt "0"))
      (:item (:texttt "length") " Default: " (:texttt "0"))
      (:item (:texttt "width") " Default: " (:texttt "0")))
     (:p "Figure "
	 (:ref "fig:box-code")
	 " defines an example box, and Figure "
	 (:ref "fig:tasty-box")
	 " shows how it will display in tasty.")

     (:p "Note the following from the example in "
	 (:ref "fig:box-code")
	 ":"
	 ((:list :style :itemize)
	  (:item "The symbol "
	    (:texttt "+phi+")
	    (:footnote "By convention, constants in Common Lisp are
	    named with a leading and trailing "
		       (:texttt "+")
		       " as a way to make them recognizable as constants.")
	    " holds a global constant containing the ``golden ratio''
	    number, which is approximated as 1.618.")
	  (:item "The slots " 
	    (:texttt "length")
	    ", "
	    (:texttt "width")
	    ", and "
	    (:texttt "height"))
	  " are defined in "
	  (:texttt "base-object")
	  " as "
	  (:emph "trickle-down-slots")
	  ". For this reason, they are automatically being passed down into into the "
	  (:texttt "box")
	  " child object. Therefore it is not necessary to pass them down explicitly."))

     ((:boxed-figure :caption "Definition of a Box" :label "fig:box-code")
      (:verbatim (:include "~/gendl/documentation/tutorial/examples/box-1.gdl")))
     ((:image-figure :image-file "tasty-box-1.png" :caption "Simple box displayed in tasty"
		     :width "4in" :height "3in"
		     :label "fig:tasty-box")))


    ((:subsection :title "Positioning a child object using the center input")

     ((:rendered-figure :caption "Positioned Boxes" :object "positioned-boxes"))

     (:p "By default, a child object will be positioned at the same "
     (:texttt "center")
     " as its parent, and the "
     (:texttt "center") 
     " defaults to the point "
     (:texttt "#(0.0 0.0 0.0)")
     ". Figure "
     (:ref "fig:positioned-boxes-source")
     " (rendered in Figure "
     (:ref "fig:positioned-boxes")
     ") shows a second box being positioned next to the first, by using the "
     (:texttt ":center")
     " input. "))

    ((:subsection :title "Positioning Sequence Elements using (the-child index)")
     
     "When specifying a sequence of child objects, each individual sequence element
can be referenced from within its "
     (:texttt ":objects")
     " section using the operator "
     (:texttt "the-child")
     ". By using "
     (:texttt "the-child")
     " to send the "
     (:texttt "index")
     " message, you can obtain the index"
     (:footnote "Indices in GDL ``size'' sequences are integers which start with 0 (zero).")
     " of each individual child object as it is being processed. 
In this manner it is possible to compute a distinct position for each child, as a function of its
index, as demonstrated in Figures "
     (:ref "fig:positioned-by-index")
     " and "
     (:ref "fig:positioned-by-index")
     "."
     ((:rendered-figure :caption "Positioned by Index" 
			:object "positioned-by-index")))


    
    ((:subsection :title "Relative positioning using the translate operator")
     (:p "It is usually wise to position child objects in a "
	 (:emph "relative")
	 " rather than "
	 (:emph "absolute") 
	 " manner with respect to the parent. For example, in our positioned-by-index example in Figure "
	 (:ref "fig:positioned-by-index-source")
	 ", each child box object is being positioned using an absolute coordinate produced by "
	 (:texttt "make-point")
	 ". This will work fine as long as the center of the current parent is "
	 (:texttt "#(0.0 0.0 0.0)")
	 " (which it is, by default). But imagine if this parent
     itself is a child of a larger assembly. Imagine further that the
     larger assembly specifies a non-default center for this instance
     of "
	 (:texttt "positioned-by-index")
	 ". At this point, the strategy falls apart.")
     (:p "The solution is to adhere to a consistent Best Practice of positioning child objects according to the "
	 (:texttt "center") 
	 " (or some other known datum point) of the parent
	 object. This can easily be accomplished through the use of
	 the "
	 (:emph "translate")
	 " operator. The "
	 (:texttt "translate")
	 " operator works within the context of a GDL object, and
	 allows a 3D point to be translated in up to three directions, chosen from: "
	 (:texttt ":up")
	 ", "
	 (:texttt ":down")
	 ", "
	 (:texttt ":left")
	 ", "
	 (:texttt ":right")
	 ", "
	 (:texttt ":front")
	 ", "
	 (:texttt ":rear")
	 ". Figures "
	 (:ref "fig:translate-by-index-source")
	 " and "
	 (:ref "fig:translate-by-index")
	 " show the equivalent of our positioned-by-index example, but
with all the positioning done relative to the parent's center."
	 ((:rendered-figure :caption "Translated by Index" 
			:object "translate-by-index"))))))


