;;
;; Copyright 2002, 2009 Genworks International and Genworks BV 
;;
;; This source file is part of the General-purpose Declarative
;; Language project (Gendl).
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

(define-object geometry (slide-show-leaf)

  :computed-slots
  ((strings-for-display "Geometry")
   
   (slide-data 
    `((:title 
       "<span class=gdl-object-def style=\"font-size: 1em;\">
base-object</span>"
       :bullet-points
       ((:description "You can mix in <span class=gdl-object-def>base-object</span> 
to get a basic coordinate system.")
        (:description 
         "This includes input messages for <span class=gdl-message-name>length</span>,
<span class=gdl-message-name>width</span>,
<span class=gdl-message-name>height</span>,
<span class=gdl-message-name>center</span>,
and <span class=gdl-message-name>orientation</span>,
and Gendl functions for each <span class=gdl-message-name>vertex</span>,
<span class=gdl-message-name>face-center</span>,
<span class=gdl-message-name>edge-center</span>,
and <span class=gdl-message-name>face-normal-vector</span>.")))
      
      (:title 
       "<span class=gdl-object-def style=\"font-size: 1em;\">box</span>"
       :bullet-points
       ((:description
        "All Gendl geometry primitives mix in (inherit from) 
<span class=gdl-object-def>base-object</span>. Perhaps the 
simplest geometry primitive is <span class=gdl-object-def>box</span>:"
        
        :examples 
        ((:define-object single-box
             :include-sample-drawing? t)))
        (:description 
         "Typically, your the actual geometry will end up in 
leaf-level child objects, while higher-level nodes might be simple
<span class=gdl-object-def>base-object</span>s.")))

      
      (:title
       "<span class=gdl-section-keyword style=\"font-size: 1em;\">
trickle-down-slots</span>"
       :bullet-points
       ((:description 
         "<span class=gdl-message-name>length</span>,
<span class=gdl-message-name>width</span>,
<span class=gdl-message-name>height</span>,
<span class=gdl-message-name>center</span>,
and <span class=gdl-message-name>orientation</span> are actually
defined as <span class=gdl-section-keyword>:trickle-down-slots</span>
in <span class=gdl-object-def>base-object</span>, which means that
you do not have to pass these 
down (they \"trickle down\" automatically). So the previous example
is equivalent to:"
         :examples 
         ((:define-object single-box-trickle)))
        (:description
         "If you are ever tempted to define your own 
<span class=gdl-section-keyword>:trickle-down-slots</span> in your 
definitions, do it with caution; they can make it difficult to trace 
where values are coming from.")))
      
      (:title 
       "Working with points and vectors"
       :bullet-points
       (
        
        (:description "3D points are represented in Gendl as a <i>vector</i> of three numbers")
        (:description 
         "You can make a point from three numbers with the
<span class=gdl-operator>make-point</span> operator:"
         :examples
         ((:code (make-point 0 0 0)
                 :return-value #(0.0 0.0 0.0))
          (:code (make-point 10 20 30)
                 :return-value #(10.0 20.0 30.0))))
        (:description 
         "Or by translating another point along a vector with
<span class=gdl-operator>translate-along-vector</span> operator:"
         :examples
         ((:code (translate-along-vector (make-point 0 0 0)
                                         (make-vector 0 0 1)
                                         10)
                 :return-value #(0.0 0.0 10.0))
                                         
          (:code (translate-along-vector (make-point 0 0 0)
                                         (make-vector 1 1 1)
                                         1)
                 :return-value 
                 #(0.5773502691896258 0.5773502691896258 0.5773502691896258))))
        
        (:description "Other operators for manipulating points and vectors include 
<span class=gdl-operator>rotate-point</span>, 
<span class=gdl-operator>rotate-point-d</span>, 
<span class=gdl-operator>rotate-vector</span>, 
<span class=gdl-operator>rotate-vector-d</span>, 
<span class=gdl-operator>subtract-vectors</span>, 
<span class=gdl-operator>add-vectors</span>, 
and <span class=gdl-operator>scalar*vector</span>")
        
        (:description 
         "See the Documentation tab in Tasty for full reference on these operators")))
      
      
      (:title 
       "Positioning a child object with 
<span class=gdl-message-name style=\"font-size: 1em;\">center</span>"
       :bullet-points
       ((:description "Objects can be positioned by specifying their 
 <span class=gdl-message-name>center</span>, which is expected to be a 
 <i>3D point</i>.")
        
        (:description "Within the context of an object definition, you can 
simply use the <span class=gdl-operator>translate</span> operator to translate
a point by a distance in any of the six direction keywords: 
<span class=lisp-code>:right</span>,
<span class=lisp-code>:left</span>,
<span class=lisp-code>:rear</span>,
<span class=lisp-code>:front</span>,
<span class=lisp-code>:top</span>,
and <span class=lisp-code>:down</span>")
        
        (:description 
         "<span class=lisp-code>:right</span> is the X axis, 
<span class=lisp-code>:rear</span> is the Y axis, 
and <span class=lisp-code>:top</span> is the Z axis:"
         :examples 
         ((:define-object five-boxes
              :include-sample-drawing? t)))))
      
      
      (:title 
       "Orienting a child object with 
<span class=gdl-message-name style=\"font-size: 1em;\">orientation</span>"
       :bullet-points
       ((:description "You can use the 
<span class=gdl-operator>alignment</span> operator to produce a 3x3 orthonormal
transformation matrix suitable for use as an 
<span class=gdl-message-name>orientation</span>.")
        
        (:description 
         "Alignment takes up to three direction keywords (the third
is only necessary if you want to force a left-handed coordinate system):"
         :examples
         ((:define-object tilted-monolith
              :include-sample-drawing? t)))))
      
      (:title
       "<i>Exercises</i> 4 and 5"
       :bullet-points
       ((:description 
         "4. Make a twisty tower of boxes, resembling the image below:"
         :image-url "tower-ex.png")
        (:description 
         "5. Make a traditional brick wall, with alternating rows shifted by 1/2 brick width,
and flush sides (you need half-width bricks on the ends in alternate rows).")))
      
      (:title 
       "Other Wireframe Objects"

       :bullet-points
       ((:description "Many mechanical and layout applications do not require 
surfaces and solids")
        (:description "Built-in wireframe objects are in the
<span class=lisp-code>:geom-base</span> package")
        (:description "Wireframe primitives do not support generic intersections, mass 
properties, parametric evaluation")
        (:description 
         "Common wireframe primitives:"
         :examples
         ((:define-object cylinder-sample
              :include-sample-drawing? t)
          (:define-object cone-sample 
              :include-sample-drawing? t)
          (:define-object sphere-sample 
              :include-sample-drawing? t)
          (:define-object spherical-cap-sample 
              :include-sample-drawing? t)
          (:define-object torus-sample 
              :include-sample-drawing? t)
          (:define-object global-filleted-polygon-projection-sample
              :include-sample-drawing? t)))
        
        (:description
         "See full documentation for these in 
<a href=http://localhost:9000/yadd>YADD</a> (also available as <i>Documentation</i> 
tab in tasty)")))
      
      
      
      
      (:title 
       "Curves, Surfaces, and Solids"
       :bullet-points
       ((:description ":surf package supports NURBS curves and surfaces")
        (:description "If you can understand Curves then you can understand Surfaces")
        (:description "The NURBS Book (Piegl, Tiller) is the definitive reference")
        (:description "<b>N</b>on-<b>U</b>niform <b>R</b>ational <b>B</b>-<b>S</b>plines")
        (:description 
         "<b>Non-Uniform</b> means a curve (or surface) can have a knot vector (or knot 
grid) which induces non-uniform parameterization.")
        (:description "<b>Rational</b> means a curve or surface can have 
weighting factors (one per control point) to affect the geometry. Weights 
are required to represent analytics (e.g. arcs (spheres)) using NURBS.")
        (:description "<b>B-Splines</b> \"B\" from \"Bezier\" (I think), and it
means the curve (surface) is controlled by a list (grid) of 3D control-points.")
        (:description "Sometimes the control-points are combined with the weights
and written as 4D  points.")))
      
      (:title
       "Curve data structure in Gendl"        
       :bullet-points
       ((:description "NURBS curves data structure contains four pieces of data:
<ol>
<li><span class=gdl-message-name>control-points</span> (list of 3D points)</li>
<li><span class=gdl-message-name>weights</span> (list of numbers, 

one per control point)</li>
<li><span class=gdl-message-name>degree</span> (a single integer, degree of 
polynomial curve function)</li>
<li><span class=gdl-message-name>knot-vector</span> (list of numbers representing 
parameter values, affect the shape of the curve)</li>
</ol>")))
      
      (:title 
       "<span class=gdl-object-def style=\"font-size: 1em;\">b-spline-curve</span>"
       :bullet-points
       ((:description 
         "defines a NURBS curve which accepts the four components of curve data directly"
         :examples
         ((:define-object simple-curve
              :include-sample-drawing? t)))
        
        (:description "Note that with default degree (3) and default weights
and knot vector, this is a Cubic Bezier curve and interpolates (i.e. touches) 
its end points.")))

      (:title 
       "Probing the curve object"
       :bullet-points
       ((:description "Basic NURBS curve input components"
         :examples ((:code (make-self 'simple-curve)
                           :return-value "#<simple-curve @ #x740cbf22>")
                    (:code (the curve control-points)
                           :return-value (#(-2.0 0.0 0.0) #(-1.0 1.0 0.0) 
                                          #(1.0 1.0 0.0) #(2.0 0.0 0.0)))
                    (:code (the curve knot-vector)
                           :return-value (0.0 0.0 0.0 0.0 1.0 1.0 1.0 1.0))

                    (:code (the curve weights)
                           :return-value (1.0 1.0 1.0 1.0))
                    (:code (the curve degree)
                           :return-value 3)))
        
        (:description 
         "Pulling info from the curve"
         :examples ((:code (the curve total-length)
                           :return-value 4.381037353487265)
                    (:code (the curve u-min)
                           :return-value 0.0)
                    (:code (the curve u-max)
                           :return-value 1.0)
                    (:code (the curve (tangent 0.5))
                           :return-value #(1.0 0.0 0.0))
                    (:code (the curve (radius-of-curvature 0.5))
                           :return-value 3.375)))))
      
      
      (:title 
       "<i>Exercise</i> 6"
       :bullet-points
       ((:description "Change the <span class=gdl-object-def>simple-curve</span>
 definition from Slide 3.11 to contain a sequence of four (4) child curves. 
 Each child should receive a <span class=lisp-code>:degree</span> passed in which 
is one greater than its index number (i.e. the degree will start with 1 and work up
to 4). Then use an instance of this definition to answer the following questions:
<ol type = \"a\">
<li>Is it even possible to produce a curve with the given control
points and with each of the specified degree values?
<li>Ask each child for its 
<span class=gdl-message-name>total-length</span>.
<li>Does the total-length increase or decrease with an
increasing degree?
</ol>")))
      
      
      (:title
       "Surface data structure in Gendl"        
       :bullet-points
       ((:description "NURBS surface data structure contains six pieces of data:
<ol>
<li><span class=gdl-message-name>control-points</span> --- list of lists of 3D points</li>
<li><span class=gdl-message-name>weights</span> --- list of lists of numbers, 
one per control point</li>
<li><span class=gdl-message-name>u-degree</span> --- a single integer, degree of 
polynomial curve function in u direction</li>
<li><span class=gdl-message-name>v-degree</span> --- a single integer, degree of 
polynomial curve function in v direction</li>
<li><span class=gdl-message-name>u-knot-vector</span> --- list of numbers representing 
parameter values in u direction, affect the shape of the surface in u direction</li>
<li><span class=gdl-message-name>v-knot-vector</span> --- list of numbers representing 
parameter values in v direction, affect the shape of the surface in v direction</li>
</ol>")))
      
      (:title 
       "<span class=gdl-object-def>b-spline-surface</span>"
       :bullet-points
       ((:description 
         "defines a NURBS surface which accepts the six components 
of surface data directly"
         :examples
         ((:define-object simple-surface
              :include-sample-drawing? t)))
        
        (:description "Note that with default degree (3) and default weights
and knot vector, this is a Cubic Bezier surface and interpolates (i.e. touches) 
its control points at its corners.")))

      (:title 
       "Probing the surface object"
       :bullet-points
       ((:description "Basic NURBS surface input components"
         :examples ((:code (make-self 'simple-surface)
                           :return-value "#<simple-surface @ #x740cbf22>")
                    (:code (the surface control-points)
                           :return-value ((#(-2.0 0.0 -2.0) #(-1.0 1.0 -2.0) 
                                           #(1.0 1.0 -2.0) #(2.0 0.0 -2.0))
                                          (#(-2.0 0.0 -1.0) #(-1.0 2.0 -1.0) 
                                           #(1.0 2.0 -1.0) #(2.0 0.0 -1.0))
                                          (#(-2.0 0.0 1.0) #(-1.0 2.0 1.0) 
                                           #(1.0 2.0 1.0) #(2.0 0.0 1.0))
                                          (#(-2.0 0.0 2.0) #(-1.0 1.0 2.0) 
                                           #(1.0 1.0 2.0) #(2.0 0.0 2.0))))
                    (:code (the surface u-knot-vector)
                           :return-value (0.0 0.0 0.0 0.0 1.0 1.0 1.0 1.0))
                    
                    (:code (the surface v-knot-vector)
                           :return-value (0.0 0.0 0.0 0.0 1.0 1.0 1.0 1.0))
                    (:code (the surface weights)
                           :return-value ((1.0 1.0 1.0 1.0) 
                                          (1.0 1.0 1.0 1.0) 
                                          (1.0 1.0 1.0 1.0) (1.0 1.0 1.0 1.0)))
                    (:code (the surface u-degree)
                           :return-value 3)
                    (:code (the surface v-degree)
                           :return-value 3)))))
      
      (:title 
       "Probing the surface object (cont'd)"
       :bullet-points
       ((:description 
         "Pulling info from the surface"
         :examples ((:code (the surface area)
                           :return-value 19.745028077271574)
                    (:code (the surface u-min)
                           :return-value 0.0)
                    (:code (the surface u-max)
                           :return-value 1.0)
                    (:code (the surface v-min)
                           :return-value 0.0)
                    (:code (the surface v-max)
                           :return-value 1.0)
                    (:code (the surface (normal 0.5 0.5))
                           :return-value #(0.0 -1.0 0.0))
                    (:code (the surface u-iso-curves first)
                           :return-value "#<curve @ #x74d5050a>")))))
      
      (:title 
       "<i>Exercise</i> 7"
       :bullet-points
       ((:description "Change the <span class=gdl-object-def>simple-surface</span>
 definition from Slide 3.15 to contain a sequence of four (4) child surfaces 
 Each child should receive a <span class=gdl-message-name>:u-degree</span> passed in which 
is one greater than its index number (i.e. the 
<span class=gdl-message-name>u-degree</span> will start with 1 and work up
to 4). Then use an instance of this definition to answer the following questions:
<ol type = \"a\">
<li>Is it even possible to produce a surface with the given control
points and with each of the specified <span class=gdl-message-name>u-degree</span> values?
<li>Ask each child for its <span class=gdl-message-name>area</span>.
<li>Does the area increase or decrease with an increasing 
<span class=gdl-message-name>u-degree</span>?</ol>")))
      
      (:title 
       "Transforming Curves and Surfaces"
       
       :bullet-points
       ((:description "Pure curves, surfaces and solids get their position, shape,
and dimensions directly from global control points")
        (:description "They <i>do not</i> respect the reference-box from 
<span class=gdl-object-def>base-object</span>.")
        (:description "For transforming curves and surfaces, you can use 
<span class=gdl-object-def>boxed-curve</span> and 
<span class=gdl-object-def>boxed-surface</span>.")
        (:description 
         "Example translated, rotated, and mirrored:"
         :examples 
         ((:define-object boxed-curves-example
              :include-sample-drawing? t
              :projection-direction :top
              )))))
      
      (:title 
       "<i>Exercise</i> 8"
       :bullet-points
       ((:description "Use the <span class=gdl-message-name>wavy</span> object
from the previous example for this exercise.
<ol type=\"a\">
<li>Check the reference documentation for 
<span class=gdl-object-def>boxed-curve</span> and see how to add a scale in
X, Y, and Z axes.</li>
<li>Add a sequence of three (3) <span class=gdl-object-def>boxed-curve</span>s to the 
<span class=gdl-object-def>boxed-curves-example</span> and scale them
by a factor of 2 in the X, Y, and Z axes, respectively. </li>
<li>How does the scaling in each direction affect the 
<span class=gdl-message-name>total-length</span>?</li>
</ol>
")))
      
      (:title 
       "Solids and Booleans"
       
       :bullet-points 
       ((:description "Solids are one or more regions bounded by trimmed surfaces (faces).")
        
        (:description "Connected faces form a shell.")
        (:description "A closed shell forms a region.")
        (:description "A <i>brep</i> can have zero or more shells and one or more regions")
        (:description "There is always the <i>infinite</i> 
region (all of space outside the brep).")))
      
      (:title 
       "Boolean operations"
       :bullet-points 
       ((:description "Brep operations are <i>closed</i> - breps in, and you always 
get one or more breps out")
        
        (:description 
         "United-solids example"
         :examples ((:define-object box-cone-unite
                        :include-sample-drawing? t
                        :side-by-side "united.png")))
        
        (:description 
         "Subtracted-solids example"
         :examples ((:define-object box-cone-subtract
                        :include-sample-drawing? t
                        :side-by-side "subtract.png")))

        
        (:description 
         "Intersected-solids example"
         :examples ((:define-object box-cone-intersect
                        :include-sample-drawing? t
                        )))))
      
      
      (:title 
       "Non-manifold Breps"
       :bullet-points
       ((:description "In <i>manifold</i> Breps, all edges are shared 
by exactly two faces")
        (:description "Gendl can handle <i>non-manifold</i> Breps, which can be
 useful as construction geometry")
        (:description 
         "The <i>merge</i> operation can join together surfaces and 
faces into a solid, and get rid of extra pieces of faces, in one operation."
         :examples 
         ((:define-object airfoil
              :include-sample-drawing? t)
          (:define-object merged-airfoil
              :include-sample-drawing? t
              )
          (:define-object regioned-airfoil
              :include-sample-drawing? t)))
        
        (:description 
         "Note that breps only display their edges by default. You can set <span class=lisp-code>*brep-isos-default*</span> to adjust this:"
         :examples
         ((:code *brep-isos-default*
                 :return-value nil)
          (:code (setq *brep-isos-default* (list :n-u 8 :n-v 8))
                 :return-value (:n-u 8 :n-v 8))))))
      
      (:title 
       "<i>Exercise 9</i>"
       
       :bullet-points
       ((:description "Add a second spar two-thirds (2/3) of the way from trailing edge to 
leading edge, and move the first spar to one-third (1/3) of the way. Your 
<span class=gdl-object-def>regioned-solid</span> should now have three 
regions instead of two.")
        (:description "Check that the total 
<span class=gdl-message-name>volume</span> of the three regions equals 
the total of the two regions with the single spar.")
        (:description "Scale the profiles with 10% decreasing chord length as 
they go from root to tip.")
        (:description "Re-orient the spars so they follow the leading edge and 
trailing edge, respectively.")
        (:description "Make a <span class=gdl-section-keyword>computed-slot</span> 
which holds the interior (i.e. middle) region (call this the \"fuel tank\").
Add a slot which computes its volume.")))))))
          


       
       


