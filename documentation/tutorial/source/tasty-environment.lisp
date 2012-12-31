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
    
    (:p (:emph "Tasty")
	(:footnote "``Tasty'' is an acronym of acronyms - it stands
for TAtu with STYle (sheets), where tatu comes from Testing And
Tracking Utility.")
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
help"
	 (:footnote "A File menu will be added in a future release, to
facilitate saving and restoring of instance ``snapshots'' --- at
present, this can be done programmatically.") ".")

     (:p "The "
	 (:emph "tree menu")
	 " allows the user to customize the ``click mode'' of the
mouse (or ``tap mode'' for other pointing device) for objects in the
tree, inspector, or viewport frames. The behavior follows the "
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
	  "Sets a global "
	  (:texttt "self")
	  " variable to the selected object, so you can interact by sending messages to the object at the command prompt e.g. by typing "
	  (:texttt "(the length)")
	  " or "
	  (:texttt "(the children)")
	  ".")
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
browsers such as a recent version of Google Chrome"
	  (:footnote "Currently, it is necessary to ``Reload'' or
	   ``Refresh'' the browser window to display the geometry in
	   this mode.")
	  ".")
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
	  "Sets the displayed perspective in the graphics viewport to Front (negative Y axis).")
	 ((:item :word "Rear")
	  "Sets the displayed perspective in the graphics viewport to Rear (positive Y axis).")
	 ((:item :word "Left")
	  "Sets the displayed perspective in the graphics viewport to Left (negative X axis).")
	 ((:item :word "Right")
	  "Sets the displayed perspective in the graphics viewport to Right (positive X axis).")
	 ((:item :word "Top")
	  "Sets the displayed perspective in the graphics viewport to Top (positive Z axis).")
	 ((:item :word "Bottom")
	  "Sets the displayed perspective in the graphics viewport to Bottom (negative Z axis)."))))

     (:p "The third toolbar hosts the most frequently used buttons. This
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

     (:p "The " (:emph "tree frame")
	 " is a hierarchical representation of your defined
object. For example for the shock-absorber assembly this will be as
depicted in figure "
	 (:ref "fig:tree-shock-absorber"))

     (:p "To draw the graphics (geometry) for the shock-absorber
leaf-level objects, you can select the ``Add Leaves (AL)'' item from
the Tree menu, then click the desired leaf to be displayed from the
tree. Alternatively, you can select the ``rapid'' button from third
toolbar which is symbolized by a pencil icon. Because this
operation (draw leaves) is frequently used, this operation is directly
available as a tooltip which will pop up when you hover the mouse over
any leaf or node in the tree.")

     (:p "The ``on the fly'' feature is available also for ``inspect
object,'' as the second icon when you hover the mouse over a leaf or
node.")

     (:p "The ``inspector'' frame allows the user to inspect (and in
some cases modify) the object instance being inspected.")

     (:p "For example, let's make the "
	 (:texttt "piston-radius")
	 " of the shock-absorber ``settable,'' by adding the keyword "
	 (:texttt ":settable")
	 "after its default expression (please review Chapter "
	 (:ref "chap:advancedgendl")
	 " if you are not familiar or need a refresher on this GenDL
syntax). We will also pass the piston-radius down into the child "
	 (:texttt "piston")
	 " object, rather than using a hard-coded value of 12 as
previously. The new assembly definition is now:"

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
	 :length 90))"))))


     (:p "In this new version ``V0.1'' of the assembly, the piston radius is a
settable slot, and its value can be modified (i.e. ``bashed'') as
desired, either programmatically from the command-line, in an end-user
application, or from the Tasty environment.")

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
inspector on that object (see Figure "
	 (:ref "fig:tasty-inspector")
	 "). Once the inspector is displaying this object, it is
possible to expand its settable slots by clicking on the ``Show
Settables!''  link. (use the ``X'' link to collapse the settable slots
view). When the settable slots area is open, the user may set the
values as desired by inputting the new value and pressing the OK
button (see Figure "
	 (:ref "fig:tasty-s-slots")
	 ")."))))

    
