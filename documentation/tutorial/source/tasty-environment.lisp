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

(defparameter *tasty-environment*
  `((:chapter :title "The Tasty Development Environment")
    
    ((:section :title "The Tasty Interface")
     (:p (:emph "Tasty")
	 (:footnote "``Tasty'' is a tortured acronym of acronyms - it stands for
TAtu with STYle (sheets), where tatu comes from Testing And Tracking
Utility.")
	 " is a web based testing and tracking utility. Note that Tasty is
designed for developers of GenDL applications --- it is not intended
as an end-user application interface (see the "
	 (:ref "chap:userinterfacesingendl")
	 " section for the recommended steps to create end-user interfaces).")

     (:p "Tasty allows you to visualize and inspect any object defined in GenDL,
which mixes at least "
	 (:texttt "base-object")
	 " into the definition of its root"
	 (:footnote "base-object is the core mixin for all geometric objects
and gives them a coordinate system, length, width, and height. This
restriction in tasty will be removed in a future GenDL release so you
will be able to instantiate non-geometric root-level objects in tasty
as well, for example to inspect objects which generate a web page but
no geometry."))
     
     (:p "First, make sure you have compiled and loaded the code for
the Chapter 5 examples, contained in "

	 (:verbatim ".../src/documentation/tutorial/examples/chapter-5/")

" in your Gendl distribution. If you are not sure how to do this,
please stop reading this section now, review Section "
	 (:ref "compiling-and-loading-files-and-systems")
	 ", then return here...")
     
     (:p "Now you should have the Chapter 5 example definitions
compiled and loaded into the system. To access Tasty, point your web
browser to the URL in figure"
	 (:ref "fig:tasty-toplevel-url")
	 "."
	 ((:boxed-figure :caption "Web Browser address for Tasty development environment"
			 :label "fig:tasty-toplevel-url")
	  (:verbatim "
 http://<host>:<port>/tasty

   \emph{by default, this URL is:}

 http://localhost:9000/tasty"))

	  "This will bring up the start-up page, as seen in Figure "
	  (:ref "fig:tasty-startup")
	  (:footnote "This page may look slightly different, e.g. different
icon images, depending on your specific Gendl version.")
	  ". To access an instance of a specific object definition,
you specify the class package and the object type, separated by a
colon (``:'') (or a double-colon (``::'') in case the symbol naming
the type is not exported from the package). For example, consider the
simple "
	  (:texttt "tower1")
	  " definition in Figure "
	  (:ref "fig:tower1-code")
	  "This definition is in the "
	  (:texttt ":chapter-5")
	  " package. So the specification will be "
	  (:texttt "shock-absorber:assembly"))

     ((:image-figure :image-file "tasty-start.png"
		     :caption "Tasty start-up"
		     :label "fig:tasty-startup"))

     (:p "Note that if the "
	 (:texttt "assembly")
	 " symbol had not been exported from the "
	 (:texttt ":shock-absorber")
	 " package, then a double-colon would have been needed: "
	 (:texttt "shock-absorber::assembly")
	 (:footnote"use of a double-colon indicates dubious coding
practice, because it means that the code in quesion is accessing the
``internals'' or ``guts'' of another package, which may not have been
the intent of that other package's designer."))

     (:p "After you specify the class package and the object type and press the
``browse'' button, the browser will bring up the utility interface
with an instance of the specified type (see figure "
	 (:ref "fig:tastyshockabsorberpre") ".")

     (:p "The utility interface by default is composed of three toolbars and
three view frames (tree frame, inspector frame and viewport frame
``graphical view port'').")


     ((:image-figure :image-file "tasty-shock-absorber-pre.pdf"
		     :width "4in" :height "4in"
		     :caption "Tasty Interface"
		     :label "fig:tastyshockabsorberpre"))


     ((:subsection :title "The Toolbars")

      (:p "The first toolbar consists of two ``tabs'' which allow the
user to select between the display of the application itself or the
GenDL reference documentation.")

      (:p "The second toolbar is designed to select various ``click modes'' for
objects and graphical viewing, and to customize the interface in other
ways. It hosts five menus: edit, tree, view, windows and
help@footnote{A File menu will be added in a future release, to
facilitate saving and restoring of instance ``snapshots'' -- at
present, this can be done programmatically.}.")

      (:p "The "
	  (:textbf "tree menu")
	  " allows the user to customize the ``click mode'' of
the mouse (or ``tap mode'' for other pointing device) for objects in
any of the tree, inspector, or viewport frames. The behavior follows
the "
	  (:emph "select-and-match")
	  " paradigm -- you first "
	  (:emph "select")
	  " a mode of operation with one of the buttons or menu items, 
then "
	  (:emph "match")
	  " that mode to any object in the tree frame or inspector frame by
left-clicking (or tapping). These modes are as follows:")

      ((:list :style :itemize)
       (:item "Tree: Graphical modes"
	 ((:list :style :description)
	  ((:item :word "Add Node (AN)")
	   "Node in graphics viewport")
	  ((:item :word "Add Leaves (AL)")
	   "Add Leaves in graphics viewport")
	  ((:item :word "Add Leaves indiv. (AL*)")
	   "Add Leaves individually (so they can be deleted individually).")
	  ((:item :word "Draw Node (DN)")
	   "Draw Node in graphics view port (replacing any existing).")
	  ((:item :word "Draw Leaves (DL)")
	   "Draw Leaves in graphics view port (replacing any existing).")
	  ((:item :word "Clear Leaves (DL)")
	   "Delete Leaves")))

       (:item "Tree: Inspect \\& debug modes"
	 ((:list :style :description)
	  ((:item :word "Inspect object (I)")
	   "Inspect (make the inspector frame to show the selected object).")
	  ((:item :word "Set self to Object (B)")
	   "Break on selected object.")
	  ((:item :word "Set Root to Object (SR)")
	   "Set displayed root in Tasty tree to selected object.")
	  ((:item :word "Up Root (UR!)")
	   "Set displayed root in Tasty tree up one level (this is grayed out if already on root).")
	  ((:item :word "Reset Root (RR!)")
	   "Reset displayed root in Tasty to to the true root of the tree (this is grayed out if already on root).")))

       (:item "Tree: frame navigation modes"
	 ((:list :style :description)
	  ((:item :word "Expand to Leaves (L)") "Nodes expand to their deepest leaves when clicked. ")
	  ((:item :word "Expand to Children (C)") "Nodes expand to their direct children when clicked.")
	  ((:item :word "Auto Close (A)") "When any node is clicked to expand, all other nodes close automatically.")
	  ((:item :word "Remember State (R)") "Nodes expand to their previously expanded state when clicked.")))

       (:item "View: Viewport Actions"
	 ((:list :style :description)
	  ((:item :word "Fit to Window!") "Fits to the graphics viewport size the displayed objects (use after a Zoom)")
	  ((:item :word "Clear View! (CL!)") "Clear all the objects displayed in the graphics viewport.")))
      
       (:item "View: Image Format"
	 ((:list :style :description)
	  ((:item :word "PNG")
	   "Sets the displayed format in the graphics viewport to PNG (raster image with 
        isoparametric curves for surfaces and brep faces).")
	  ((:item :word "JPEG")
	   "Sets the displayed format in the graphics viewport to JPEG
         (raster image with isoparametric curves for surfaces and brep faces).")
	  ((:item :word "VRML/X3D")
	   "Sets the displayed format in the graphics viewport to
             VRML with default lighting and viewpoint (these can be changed
             programmatically). This requires a compatible plugin such as BS Contact")
	  ((:item :word "X3DOM")
	   "This experimental mode sets the displayed format in the graphics viewport to use the x3dom.js Javascript library,
which attempts to render X3D format directly in-browser without the need for plugins. This works best in WebGL-enabled
browsers such as a recent version of Google Chrome.")
	  ((:item :word "SVG/VML")
	   "Sets the displayed format in the graphics viewport to SVG/VML"
	   (:footnote "For complex objects with many display curves,
            SVG/VML can overwhelm the JavaScript engine in the web
            browser. Use PNG for these cases.") 
	   ", which is a vector graphics image format displaying 
            isoparametric curves for surfaces and brep faces.")))

       (:item "View: Click Modes"
	 ((:list :style :description)
	  ((:item :word "Zoom in")
	   "Sets the mouse left-click in the graphics viewport to zoom in.")
	  ((:item :word "Zoom out")
	   "Sets the mouse left-click in the graphics viewport to zoom out.")
	  ((:item :word "Measure distance")
	   "Calculates the distance between two selected points from the graphics viewport.")
	  ((:item :word "Get coordinates")
	   "Displays the coordinates of the selected point from the graphics viewport.")
	  ((:item :word "Select Object")
	   "Allows the user to select an object from the graphics
                  viewport (currently works for displayed curves and
                  in SVG/VML mode only).")))

       (:item "View: Perspective"
	 ((:list :style :description)
	  ((:item :word "Trimetric")
	   "Sets the displayed perspective in the graphics viewport to trimetric.")
	  ((:item :word "Front")
	   "Sets the displayed perspective in the graphics viewport to Front (-Y axis).")
	  ((:item :word "Rear")
	   "Sets the displayed perspective in the graphics viewport to Rear (+Y axis).")
	  ((:item :word "Left")
	   "Sets the displayed perspective in the graphics viewport to Left (-X axis).")
	  ((:item :word "Right")
	   "Sets the displayed perspective in the graphics viewport to Right (+X axis).")
	  ((:item :word "Top")
	   "Sets the displayed perspective in the graphics viewport to Top (+Z axis).")
	  ((:item :word "Bottom")
	   "Sets the displayed perspective in the graphics viewport to Bottom (-Z axis)."))))

      (:p "Third toolbar hosts the most frequently used buttons. This
buttons have tooltips which will pop up when you hover the mouse over
them. However, these buttons are found in the second toolbar too,
except line thickness and color buttons. The line thickness and color
buttons"
	  (:footnote "the design of the line thickness and color buttons is
being refined and may appear different in your installation.")
	  " expand and contract when clicked on and allows the user to
select a desired line thickness and color for the objects displayed in
the graphics viewport."))

     ((:subsection :title "View Frames")

      (:p "@b{The tree frame} is a hierarchical representation of your defined object. For example for 
the  shock-absorber @b{assembly} this will be as depicted 
in figure 8.
@sp 1
@center @image{images/tree-shock-absorber,,3.2in  }
@center Figure 8: Tasty shock-absorber tree.
@sp 1")

      (:p "To draw the graphics (geometry) for the shock-absorber leaf-level
objects, you can select the ``Add Leaves (AL)'' item from the Tree
menu, then click the desired leaf to be displayed from the tree or
select the @i{rapid} button from third toolbar which is symbolized by
a pencil @image{images/tbar_pen,,0.2in}. Because this operation (draw
leaves) is frequently used, the tree leaves has this operation
directly available as a tooltip, which will pop up when you hover the
mouse over them.  To perform this operation on the fly, you simply
have to click the pencil icon; it does not require a second click on
the leaf in the tre (see figure 8).")

      (:p "The ``on the fly'' feature is available also for ``inspect
object''(second icon when you hover the mouse over a leaf) and
``highlight object'' (third icon when you hover the mouse over a
leaf). For safety reasons highlight object requires a second click on
the leaf (a confirmation that the user wants to remove that leaf from
the graphics viewport).")

      (:p "@b{The inspector frame} allows the user to inspect (and in some cases
modify) object instance being inspected.  Following are some examples
of how the inspector frame may be used.")

      (:p "To prepare for the first example, we will modify the assembly
definition by adding a settable @b{input-slot} for the piston radius.
In GenDL, the @b{:input-slots} are made up of a list, each of whose
elements is either a symbol (for required inputs) or a list expression
beginning with a symbol (for optional inputs). In either case, the
symbol represents a value which can be supplied either:"

	  ((:list :style :itemize)
	   (:item "into the toplevel object of an object hierarchy, when the object is instanted, or")
	   (:item "into a child object, using a @b{:objects} specification by definition in the parent."))

	  "Inputs are specified in the definition either as a symbol by itself
 (for required inputs), or as an expression whose @b{first} is a symbol
and whose @b{second} is an expression which returns a value which will
be the default value for the input slot.")

      (:p "Optionally, additional keywords can be supplied:"
	  ((:list :style :itemize)
	   (:item "the keyword @b{:defaulting}, which indicates that if a slot by this name is
contained in any ancestor object's list of
:trickle-down-slots@footnote{trickle-down slots will be introduced
later}, the value from the ancestor will take precedence over the
local default expression.")
	   (:item "@item The keyword @b{:settable}, which indiates that the default value of 
this slot can be ``bashed,'' or overridden, at runtime after the
object has been instantiated, using the special object function
@b{set-slot!}@footnote{Any other slots depending on settable slots (directly or
indirectly) will become unbound when the @b{:set-slot!} function is
called to change the slot's value, and these will be recomputed the
next time they are demanded.}.")))
      
      (:p "For this example, we will supply piston-radius as an input symbol with
a default expression and with the optional keyword @b{:settable}. We
will also pass the piston-radius down into the child @b{piston}
object, rather than using a hard-coded value of 12 as previously. The
new assembly definition is now:"

	  ((:boxed-figure :caption "Shock Absorber Assembly V0.1"
			  :label "fig:shockabsorberassemblyv01")
	   (:small
	    (:verbatim "
 (in-package :shock-absorber)

 (define-object  assembly (base-object)
   :input-slots ((piston-radius 12 :settable)) ;;;----modification ;
   :computed-slots ()
   :objects 
   ((pressure-tube :type 'cone
		   :center (make-point 0 70 0)
		   :length 120
		   :radius-1 13
		   :inner-radius-1 12
		   :radius-2 13
		   :inner-radius-2 12)
   
    (tube-cap :type 'cone
	      :center (make-point 0 5 0)
	      :length 10
	      :radius-1 5
	      :inner-radius-1 0
	      :radius-2 13
	      :inner-radius-2 0)

    (seal-cap :type 'cone
	      :center (make-point 0 135 0)
	      :length 10
	      :radius-1 13
	      :inner-radius-1 2.5
	      :radius-2 5
	      :inner-radius-2 2.5)
   
    (floating-piston :type 'cylinder
		     :center (make-point 0 35 0)
		     :radius 12
		     :length 10)

    (blocking-ring :type 'cone
		   :center (make-point 0 42.5 0)
		   :length 5
		   :radius-1 12
		   :inner-radius-1 10
		   :radius-2 12
		   :inner-radius-2 10)
   
    (piston :type 'cylinder
	    :center (make-point 0 125 0)
	    :radius (the piston-radius) ;;;----modification ;
	    :length 10)
   
    (rod :type 'cylinder
	 :center (make-point 0 175 0)
	 :radius 2.5
	 :length 90))
  
   :hidden-objects ()
   :functions ()
   :methods ()) "))))

      (:p "In this new version ``V0.1'' of the assembly, the piston radius is a
settable slot, and its value can be modified (i.e. ``bashed'') as
desired, either programmatically from the command-line, in an end-user
application, or from @b{Tasty}.")

      (:p "
@sp 1
@center @image{images/tasty-inspector,,3.2in}
@center Figure 9: Tasty inspector.
@sp 1")

      (:p "
@center @image{images/tasty-s-slots,,3.2in}
@center Figure 10: Tasty settable slots.
@sp 1")

      (:p "To modify the value in Tasty: select ``Inspect'' mode from the Tree
menu, then select the root of the @b{assembly} tree to set the
inspector on that object (see figure 9). Once the inspector is set to
this object, it is possible to expand its settable slots by clicking
on the ``Show Settables!'' link. (use the ``X'' link to collapse the
settable slots view). When the settable slots area is open the user
may set the values as desired by inputting the new value and pressing
the OK button (see figure 10).")

      (:p "As it can be observed from Figure 10, there is no dependency between
the piston radius and the rest of the shock-absorber objects
(components). If such a dependency is desired, the definition has to
be modified to include it. For the moment the piston radius will be
considered as the leading parameter, the rod radius constant and the
piston Y position will be added as a settable input slot. In addition
to this dimensional dependency, two computed slots will be added to the
previous code in order to compute the volume A and B (see figure 5) as
follows:")))))
#|



@sp 1
@cartouche
@smallexample
;;--assembly.lisp--
;; this is a new version V0.2 of the file assembly.lisp
(in-package :shock-absorber)
(define-object  assembly (base-object)
  
  :input-slots 
  ((piston-radius 12 :settable)
   (piston-y-position 125 :settable))
  

  :computed-slots 
  ((volume-a  (* 2 pi (- (* (the piston-radius) 
                            (- (- (get-y (the piston center)) 
                                  (get-y (the floating-piston center))) 
                               (/ (+ (the floating-piston length)  
                                     (the piston length)) 2))) 
                         (*  (the blocking-ring  length) 
                             (- (the piston-radius) 
                                (the blocking-ring  inner-radius-1))))))
   
   (volume-b (* (- (- (get-y (the floating-piston center)) 
                      (/ (the floating-piston length)2))
                   (+ (get-y (the tube-cap center)) 
                      (/ (the tube-cap length)2)))
                (* 2 pi (the  floating-piston radius )))))

  :objects 
  ((pressure-tube :type 'cone
                  :center (make-point 0 70 0)
                  :length 120
                  :radius-1 (+ 1 (the piston-radius))
                  :inner-radius-1 (the piston-radius)
                  :radius-2 (+ 1 (the piston-radius))
                  :inner-radius-2 (the piston-radius))
   
   (tube-cap :type 'cone
             :center (make-point 0 5 0)
             :length 10
             :radius-1 5
             :inner-radius-1 0
             :radius-2 (+ 1 (the piston-radius))
             :inner-radius-2 0)
@end smallexample
@end cartouche 
@sp 1  

@cartouche
@smallexample  
   (seal-cap :type 'cone
             :center (make-point 0 135 0)
             :length 10
             :radius-1 (+ 1 (the piston-radius))
             :inner-radius-1 2.5
             :radius-2 5
             :inner-radius-2 2.5)
   
   (floating-piston :type 'cylinder
                    :center (make-point 0 35 0)
                    :radius (the piston-radius)
                    :length 10)
   
   (blocking-ring :type 'cone
                  :center (make-point 0 42.5 0)
                  :length 5
                  :radius-1 (the piston-radius)
                  :inner-radius-1 (* 0.8 (the piston-radius))
                  :radius-2 (the piston-radius)
                  :inner-radius-2 (* 0.8 (the piston-radius)))
   
   (piston :type 'cylinder
           :center (make-point 0 (the piston-y-position ) 0)
           :radius (the piston-radius)  
           :length 10)
   
   (rod :type 'cylinder
        :center (make-point 0 175 0)
        :radius 2.5
        :length 90))  
  :hidden-objects ()
  :functions ()
  :methods ())

@end smallexample
@end cartouche 
@sp 1   

The dimensional dependency is a straight forward operation similar to
the @b{piston-radius} being passed in for the @b{piston} @b{radius} in
v0.1. The only difference in this case is that, for some of the
objects, a coefficient was added to account for a wall thickness
(e.g. 0.8) which is multiplied with the @b{piston-radius}.

Regarding the volume A and B, these are computed in such a manner to
account for any geometry variation, including relative movement of the
piston and floating-piston.  While the Lisp expression may not be
immediately obvious, in practice it is easy to build up such an
expression incrementally at the prompt level until a valid expression
is obtained. See example below:

Assuming that the @b{assembly version V0.2} is compiled and loaded,
you can update @b{Tasty} by clicking the ``update'' button
@image{images/tbar_update,,0.2in}.To start implementing the volume-a
sequentially (although the volume-a and volume-b is already
implemented in V0.2, this is a good exercise to go through), select
the click mode to ``break'' @image{images/tbar_break,,0.2in} and ``set
self'' to the @b{shock-absorber:assembly} by clicking the assembly
root. This will allow you to interact directly with the shock-absorber
assembly at the prompt level where you can type the following:

@sp 1
@cartouche
@smallexample

!!! see figure 11 for more details regarding R, H,  HFP, HSC, etc. 

gdl-user(38): (setq R (the piston-radius))

12

gdl-user(39): (setq rsc (the blocking-ring  inner-radius-1))

9.600000000000001

gdl-user(40): (setq HSC  (the blocking-ring  length))

5

gdl-user(41): (setq HFP  (the floating-piston length))

10

gdl-user(42): (setq HP  (the piston length))

10

gdl-user(43): (setq H (- (- (get-y (the piston center)) 
                           (get-y (the floating-piston center))) 
                        (/ (+ HFP HP) 2)))
80.0

@end smallexample
@end cartouche 
@sp 1  

Mathematically, volume-a is defined as follows:

@tex
 $ \ V_A =  [2 \pi R H - (2 \pi R H_{SC} - 2\pi r_{SC} H_{SC} )] $
@end tex

or 

@tex
 $ \ V_A =  2 \pi [R H - H_{SC} (R - r_{SC} )] $
@end tex

Where H is:

@tex
 $ \ H = [( Y^* - Y) - 2^{-1} (H_{FP} + H_P ] $
@end tex

@sp 1
@center @image{images/volume-a-b,,3.3in}
@center Figure 11: The definition of volume-a and volume-b.
@sp 1

In GenDL S-expression (i.e. Lisp) notation, the formula for volume-a is
equivalent to:

@sp 1
@cartouche
@smallexample
gdl-user(35):  (* 2 pi (- (* R H) (* HSC (- R rsc))))

;; or expanded as implemented in the shock-absorber assembly version V0.2:

gdl-user(46): (* 2 pi (- (* (the piston-radius) 
                            (- (- (get-y (the piston center)) 
                                  (get-y (the floating-piston center))) 
                               (/ (+ (the floating-piston length)  
                                     (the piston length)) 2))) 
                         (* (the blocking-ring length) 
                            (- (the piston-radius) 
                               (the blocking-ring  inner-radius-1)))))

;; for volume-b the approach is similar to volume-a 

@end smallexample
@end cartouche 
@sp 1 

Note that the implementation of @b{volume-a} and @b{volume-b} in the
shock-absorber assembly are non-settable @b{computed-slots}. This 
means that they are, in fact, messages of the object which are strictly computed based
on their default expression.

Computed-slots will only be computed when called (``demanded''), then
the resultant values will be cached (memorized)in memory. Only if
another slot on which they depend becomes modified will they become
unbound, then their values will be recomputed from their expressions
when demanded next time.

At prompt you can ask for @b{volume-a} and @b{volume-b} value by typing:

@sp 1
@cartouche
@smallexample
gdl-user(47): (the volume-a)

5956.459671206248   

or 

gdl-user(48): (the volume-b)

1507.9644737231006
@end smallexample
@end cartouche 
@sp 1 

To test the volume dependency on relative movement of the piston, set the value of @b{piston-y-position} to 
60 (see figure 12) and ask at prompt the value of volume-a.

@sp 1
@cartouche
@smallexample
gdl-user(52): (the volume-a)

2563.539605329271
@end smallexample
@end cartouche 
@sp 1 

@center @image{images/piston-relative-m,,3.4in}
@center Figure 12: Relative movement of the piston in the Y direction 
@sp 1

Adding a relative movement to the piston by using the slot
@b{piston-y-position}, will implicitly alter the dimensional
connection in the Y direction between the piston and the rod - see
figure 12. To account for this, a dimensional dependency must be added
in the y direction. The best practice for this particular case is to
define a piston assembly as a separate object definition (and in a
separate file --- piston-assembly.lisp) which will contain both the
definition of piston/rod and the dimensional dependency as presented
below:

@sp 1
@cartouche
@smallexample
;;-- piston-assembly.lisp--

(in-package :shock-absorber)

(define-object piston-assembly (base-object)

  :input-slots (piston-length piston-radius rot-length piston-y-position)
  
  :computed-slots
  ((rot-center (make-point 0 (+ (get-y (the piston center )) 
                                (/ (+ (the piston-length) 
                                      (the rot-length)) 2)) 0)))

  :objects
  ((piston :type 'cylinder
           :center (make-point 0 (the piston-y-position ) 0)
           :radius (the piston-radius)  
           :length (the piston-length ))
   
   (rod :type 'cylinder
        :center (the rot-center)
        :radius 2.5
        :length (the rot-length))))

@end smallexample
@end cartouche 
@sp 1 

@sp 1
@cartouche
@smallexample
;;--assembly.lisp-- this is a new version V0.3 of the file
;; assembly.lisp

(in-package :shock-absorber)

(define-object  assembly (base-object)
  
  :input-slots 

  ((piston-radius 12 :settable)
   (piston-y-position 125 :settable)
   (piston-length 10 :settable)
   (rot-length 90 :settable))

 :computed-slots 
 ((volume-a 
   (* 2 pi (- (* (the piston-radius) 
                 (- (- (get-y (the rod-piston-assembly piston center)) 
                       (get-y (the rod-piston-assembly floating-piston center))) 
                    (/ (+ (the rod-piston-assembly  floating-piston length) 
                          (the rod-piston-assembly  piston length)) 2))) 
              (*  (the blocking-ring  length) 
                  (-  (the piston-radius)
                      (the blocking-ring  inner-radius-1))))))
@end smallexample
@end cartouche 

@cartouche
@smallexample  
  (volume-b 
   (* (- (- (get-y (the rod-piston-assembly floating-piston center)) 
            (/ (the  rod-piston-assembly floating-piston length)2))
         (+ (get-y (the tube-cap center)) (/ (the tube-cap length)2)))
      (* 2 pi (the  rod-piston-assembly  floating-piston radius )))))

  :objects 
  ((pressure-tube :type 'cone
                  :center (make-point 0 70 0)
                  :length 120
                  :radius-1 (+ 1 (the piston-radius))
                  :inner-radius-1 (the piston-radius)
                  :radius-2 (+ 1 (the piston-radius))
                  :inner-radius-2 (the piston-radius))
   (tube-cap :type 'cone
             :center (make-point 0 5 0)
             :length 10
             :radius-1 5
             :inner-radius-1 0
             :radius-2 (+ 1 (the piston-radius))
             :inner-radius-2 0)
   (seal-cap :type 'cone
             :center (make-point 0 135 0)
             :length 10
             :radius-1 (+ 1 (the piston-radius))
             :inner-radius-1 2.5
             :radius-2 5
             :inner-radius-2 2.5)
   (floating-piston :type 'cylinder
                    :center (make-point 0 35 0)
                    :radius (the piston-radius)
                    :length 10)
   (blocking-ring :type 'cone
                  :center (make-point 0 42.5 0)
                  :length 5
                  :radius-1 (the piston-radius)
                  :inner-radius-1 (* 0.8 (the piston-radius))
                  :radius-2 (the piston-radius)
                  :inner-radius-2 (* 0.8 (the piston-radius)))

   (rod-piston-assembly :type 'piston-assembly
                        :pass-down (piston-radius piston-y-position 
                                    piston-length rot-length)))



@end smallexample
@end cartouche 

In this version of the assembly, @b{:pass-down} was used to pass the
@b{piston-radius}, @b{piston-y-position}, @b{piston-length}, and
@b{rot-length} into the child piston-assembly.  @b{:pass-down} is a
shorthand way of passing inputs into a child object where the
input-slots in the child definition have the same names as the
messages in the parent.  The following child specification would be
exactly equivalent (but more verbose) to the above @b{:pass-down}
clause:

@sp 1

@cartouche
@smallexample

  (rod-piston-assembly :type 'piston-assembly
                       :piston-radius (the piston radius)
                       :piston-y-position (the piston-y-position)
                       :piston-length (the piston-length)
                       :rot-length (the rot-length))


@end smallexample
@end cartouche
  


Basically, the shock-absorber assembly is fully constrained in the
above example, except for the floating piston position.  In a real
application, the piston and floating piston relative movement in the Y
direction is determined by the absorbed energy at shock. However, we
will not detail the full shock absorber physics in this
manual. Instead, we will provide an ``ideal case'' example by adding a
function to determine the equilibrium between the two pistons when
loaded. The following ideal case will be considered:

The shock-absorber volume-a is filled with an ideal gas and
experiences an isothermal transformation T=const, pV=const.
In volume-b a incompressible liquid is used. Based on this assumptions the 
volume-a variation at load can be defined as follows:

@tex
$ \ pV = \ nRT = \ ct.$

$ \ p_{A} V_{A} = \ p_{A0} V_{A0} $

@end tex

@sp 1
If :
@sp 1

@tex
$ p_A > 1.101325 \ X \ 10^5  \ N/m^2 $
@end tex

@sp 1
Then:
@sp 1
@tex

$ V_{A0} = \left (p_A V_A
          \over \ p_A + \left (m g
              \over \ 2 \pi R \right) \right) $

$ Y^*_0 = Y^* - \left (1
              \over \ 2 \pi R \right) 
              (V_A - V_{A0})  $
         
@end tex

Based on this, a function can be defined as follows, which will
automatically compute the new piston y position when loaded:

@cartouche
@smallexample

;;-- main.lisp--
;; this is a new version V0.4 of the file main.lisp

(in-package :shock-absorber)

(define-object  assembly (base-object)

   :input-slots 
  ((piston-radius 12 :settable);;[SI]
   (piston-y-position 125 :settable);;[SI]
   (piston-length 10 :settable);;[SI]
   (rot-length 90 :settable);;[SI]
   (pressure-a 1.5e5 :settable);;[SI]
   (loaded-mass 10e5 :settable);;[SI] )

  :computed-slots 
  ((volume-a 
    (* 2 pi (- (* (the piston-radius) 
                  (- (- (get-y (the rod-piston-assembly piston center)) 
                        (get-y (the rod-piston-assembly floating-piston center))) 
                     (/ (+ (the rod-piston-assembly  floating-piston length) 
                           (the rod-piston-assembly  piston length)) 2))) 
               (*  (the blocking-ring  length) 
                   (-  (the piston-radius)
                       (the blocking-ring  inner-radius-1))))))
   
   (volume-a0 (/ (* (the pressure-a) (the volume-a ))
                 (+ (the pressure-a) (/ (* (the loaded-mass) 9.8) 
                                        (* 2 pi (the piston-radius))))))
   
   (volume-b 
    (* (- (- (get-y (the rod-piston-assembly floating-piston center)) 
             (/ (the  rod-piston-assembly floating-piston length)2))
          (+ (get-y (the tube-cap center)) (/ (the tube-cap length)2)))
       (* 2 pi (the  rod-piston-assembly  floating-piston radius )))))      
  
  :objects 
  ((pressure-tube :type 'cone
                  :center (make-point 0 70 0)
                  :length 120
                  :radius-1 (+ 1 (the piston-radius))
                  :inner-radius-1 (the piston-radius)
                  :radius-2 (+ 1 (the piston-radius))
                  :inner-radius-2 (the piston-radius))
   
   (tube-cap :type 'cone
             :center (make-point 0 5 0)
             :length 10
             :radius-1 5
             :inner-radius-1 0
             :radius-2 (+ 1 (the piston-radius))
             :inner-radius-2 0)
   
   (seal-cap :type 'cone
             :center (make-point 0 135 0)
             :length 10
             :radius-1 (+ 1 (the piston-radius))
             :inner-radius-1 2.5
             :radius-2 5
             :inner-radius-2 2.5)
@end smallexample
@end cartouche

@cartouche
@smallexample  
   (floating-piston :type 'cylinder
                    :center (make-point 0 35 0)
                    :radius (the piston-radius)
                    :length 10)
   
   (blocking-ring :type 'cone
                  :center (make-point 0 42.5 0)
                  :length 5
                  :radius-1 (the piston-radius)
                  :inner-radius-1 (* 0.8 (the piston-radius))
                  :radius-2 (the piston-radius)
                  :inner-radius-2 (* 0.8 (the piston-radius)))
   
   (rod-piston-assembly :type 'piston-assembly
                        :pass-down (piston-radius piston-y-position 
                                                  piston-length rot-length))
   
   (loaded-piston :type 'piston-assembly
                  :piston-y-position (l_p_p (the piston-radius)
                                            (the piston-y-position)
                                            (the volume-a)
                                            (the volume-a0))
                  :pass-down (piston-radius 
                              piston-length 
                              rot-length)))
  
  :functions ((l_p_p ;;;loaded_piston_position
               (R Y-star Va Va0)
               (- Y-star (* (/ 1 (* 2 pi R)) (- Va Va0))))))

@end smallexample
@end cartouche 
  
    

  ))
|#