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

(in-package :gdl-tutorial)

(defparameter *example-2*
    `((:chapter :title "Example 2: Simplified Android Robot")
      
      "This chapter describes and shows the complete code for a Simplified Android Robot"
      (:footnote "The ``Simplified Android Robot'' is a traditional example used for pedagogical 
purposes in computer graphics, and as far as we know it has its origins in "
		 (:underline "Computer Graphics: Principles and Practice")
		 " by Foley, Feiner, and Van Dam.")
      (:index "objects!primitive!geometric")
      " implemented in GDL/GWL. Here we introduce the use of geometric primitive objects, 
as well as the use of the higher-level "
      (:texttt (:indexed "application-mixin"))
      " first introduced in Chapter "
      (:ref "chap:gwlsyntax")
      ".

We also introduce the concept of the "
      (:emph (:indexed "view"))
      ", which allows separation of presentation functions (e.g. for HTML output) from the
core object definition."
      
      ((:section :title "Main UI Sheet for the Robot")
       "Figure "
       (:ref "code:robot-toplevel")
       " defines the toplevel user interface sheet for the robot.
It specifies several "
       (:texttt ":settable")
       " computed-slots which the user will end up being able to set through an HTML form.
It also specifies the "
       (:texttt "robot")
       " child object, whose leaves contain the actual geometry (boxes in this
case) of the robot.

Figure "
       (:ref "code:robot-model-inputs")
       " shows the definition of a "
       (:emph "view")
       " which is defined for the "
       (:texttt "html-format")
       " output format, and the "
       (:texttt "robot-assembly")
       " GDL object. Rather than being associated with a single type
as with normal GDL objects, views are associated with "
       (:emph "two")
       " types -- an output format and a normal GDL object type. The "
       (:texttt ":output-functions")
       " defined within the view will therefore be associated with the "
       (:emph "combination")
       " of the given output-format and the given GDL object type. In 
this case, we are specifying the "
       (:texttt (:indexed "model-inputs"))
       " "
       (:texttt ":output-function")
       " to be applied to the combination of the "
       (:texttt "html-format")
       " output-format and the "
       (:texttt "robot-assembly")
       " GDL object type.

The "
       (:texttt "application-mixin")
       " contains a default (essentially blank) "
       (:texttt "model-inputs")
       " function, and here we are overriding it to do something specific, namely
to display html input fields for the slots in our "
       (:texttt "robot-assembly")
       " which we wish the user to be able to alter through a form.

By default, the "
       (:texttt "application-mixin")
       " will display the output from the "
       (:texttt "model-inputs")
       " function in the upper-right area of the user interface sheet, as
seen in Figure "
       (:ref "fig:robot")
       ". These input fields are automatically contained inside an appropriate
HTML form entity - when using "
       (:texttt "application-mixin")
       ", there is no need for application-level code to generate the HTML form
tag or the "
       (:texttt (:indexed ":respondant"))
       " or "
       (:texttt (:indexed ":bashee"))
       " hidden fields described in Chapter "
       (:ref "chap:gwlsyntax")
       " with plain "
       (:texttt (:indexed "base-html-sheet"))
       "."
      
       ((:boxed-figure :caption "UI Sheet for Robot"
		       :label "code:robot-toplevel")
	(:verbatim  "


 (define-object robot-assembly (application-mixin)
  
   :computed-slots
   ((width 5 :settable)
    (length 2 :settable)
    (height 10 :settable)
    (head-angle 0 :settable)
    (body-angle 0 :settable)
    (arm-angle-right 0 :settable)
    (arm-angle-left 0 :settable)
    (pincer-distance-right (to-single-float 
                            (number-round (* .15 3/5 (the width)) 3)) 
                           :settable)
    (pincer-distance-left (to-single-float 
                           (number-round (* .15 3/5 (the width)) 3)) 
                          :settable)
    (image-format (the view-object image-format))
    (strings-for-display \"Robot Assembly\")
    (ui-display-list-objects (the robot)))

   :objects
   ((robot :type 'robot
           :pass-down (:head-angle 
                       :body-angle :arm-angle-right :arm-angle-left
                       :pincer-distance-right :pincer-distance-left))))
"))
      
       ((:boxed-figure :caption "Inputs Section for UI Sheet"
		       :label "code:robot-model-inputs")
	(:small (:verbatim "


 (define-view (html-format robot-assembly)()
   :output-functions
   ((model-inputs
     ()
     (html 
      ((:table  :cellpadding 0)
       (:tr ((:td :colspan 2) (:b \"Dimensions:\")))
       (dolist (slot (list :width :length :height))
         (html 
          (:tr ((:td :bgcolor :yellow)
                (:princ (string-capitalize slot)))
               (:td ((:input :type :string :name slot :size 5
                             :value (format nil \"~a\" 
                                            (the (evaluate slot)))))))))
       (:tr ((:td :colspan 2) :br))
       (:tr ((:td :colspan 2) (:b \"Angles:\")))
       (dolist (angle '((\"Head\" :head-angle)(\"Body\" :body-angle)
                        (\"Left Arm\" :arm-angle-left) 
                        (\"Right Arm\" :arm-angle-right)))
         (html
          (:tr ((:td :bgcolor :yellow) (:princ (first angle)))
               (:td ((:input 
                      :type :string :name (second angle) 
                      :size 5 :value 
                      (format nil \"~a\" (the (evaluate (second angle))))))))))
       (:tr ((:td :colspan 2) :br))
       (:tr ((:td :colspan 2) (:b \"Grip Opening:\")))
       (dolist (side (list :left :right))
         (html
          (:tr 
           ((:td :bgcolor :yellow) (:princ (string-capitalize side)))
           (:td 
            ((:input 
              :type :string 
              :name (format nil \"pincer-distance-~a\" side) 
              :size 5 
              :value 
              (the (evaluate (make-keyword 
                              (format nil \"pincer-distance-~a\" side))))))))))
       (:tr ((:td :colspan 2) :br))
       (:tr ((:td :colspan 2 :align :center)
             ((:input :type :submit :value \" OK \" :name :refresh)))))))))
      ")))
      
       ((:image-figure :image-file "robot.png"
                       :caption "Default Robot"
                       :label "fig:robot")))
      
      
      ((:section :title "Robot Geometry")
       
       "The actual robot is made up of two child objects, its "
       (:texttt "base")
       " and its "
       (:texttt "body")
       ", as shown in Figure "
       (:ref "code:robot-geometry-toplevel")
       ". Figure "
       (:ref "code:robot-body")
       " shows the definition of the body, and "
       (:ref "code:robot-base")
       " shows the definition of the base. The body is made
up of a torso (a box), a head (a box), and two arms, whose
definition is shown in Figure "
       (:ref "code:robot-arm")
       ". The "
       (:texttt ":settable")
       " :computed-slots from the toplevel UI sheet come into the "
       (:texttt "robot")
       " as input-slots. These serve as parameters for the rest of the robot hierarchy.

The positioning and orientation of each child object are specified by 
passing "
       (:texttt (:indexed ":center"))
       " and "
       (:texttt (:indexed ":orientation"))
       " into the child part:"
       ((:list :style :description)
	((:item :word ":center")
	 "is given as a 3D point, and causes the child object to treat this
point as its center.")
	((:item :word ":orientation")
	 "is given as a 3-by-3 rotational transformation matrix, and causes 
the child object to adjust its six "
	 (:texttt (:indexed "face-normal-vector"))
	 "s accordingly. As with this example, this transformation matrix is 
usually created using the "
	 (:texttt (:indexed "alignment"))
	 " function, which allows you to align up to three faces of the child
object with up to three given vectors. The first vector will be taken exactly, 
and the second and third vectors will be taken for their orthogonal components 
to the previous ones."))
       
       ((:boxed-figure :caption "Toplevel of Robot Geometry"
		       :label "code:robot-geometry-toplevel")
	(:small (:verbatim "


 (define-object robot (base-object)

   :input-slots
   (head-angle
    body-angle
    arm-angle-right
    arm-angle-left
    pincer-distance-right
    pincer-distance-left)

   :computed-slots
   ((display-controls (list :color :green-lime)))

   :objects
   ((base :type 'robot-base
          :height (* (the :height) 0.4)
          :width (* (the :width) 0.2)
          :length (* (the :length) 0.2)
          :center (translate (the :center) :down (* (the :height) 0.3)))
    (body :type 'robot-body
          :height (* (the :height) 0.6)
          :center (translate (the :center) :up (* (the :height) 0.2))
          :orientation 
          (alignment :right
                     (rotate-vector-d 
                      (the (:face-normal-vector :right))
                      (the :body-angle)
                      (the (:face-normal-vector :top))))
          :pass-down (:head-angle 
                      :arm-angle-right :arm-angle-left 
                      :pincer-distance-left :pincer-distance-right))))      
      ")))
       
      ((:boxed-figure :caption "Robot Body"
                      :label "code:robot-body")
       (:small (:verbatim "


 (define-object robot-body (base-object)
   :input-slots
   (head-angle arm-angle-left arm-angle-right
    pincer-distance-left pincer-distance-right
    (shoulder-height (* (the :torso :height) 0.1)))

   :computed-slots
   ((display-controls (list :color :blue-steel-light)))

   :objects
   ((torso :type 'box :height (* (the :height) 0.85)
           :width (* (the :width) 0.7)
           :center (translate (the :center) 
                              :down (- (half (the :height)) 
                                       (half (the-child :height)))))
    (head :type 'box :display-controls (list :color :magenta)
          :height (- (the :height) (the :torso :height))
          :width (* (the :width) 0.25) :length (half (the :length))
          :center (translate (the :center) :up
                             (- (half (the :height)) (half (the-child :height))))
          :orientation (alignment :right
                                  (rotate-vector-d 
                                   (the (:face-normal-vector :right))
                                   (the :head-angle)
                                   (the (:face-normal-vector :top)))))
    (arms :type 'robot-arm :sequence (:size 2)
          :side (ecase (the-child :index) (0 :left) (1 :right))
          :width (half (- (the :width) (the :torso :width)))
          :length (/ (the :length) 3)
          :height (- (the :torso :height) (twice (the :shoulder-height)))
          :center (translate-along-vector 
                   (the-child :shoulder-point)
                   (the-child (:face-normal-vector :bottom)) 
                   (half (the-child :height)))
          :orientation 
          (alignment :bottom
                     (rotate-vector-d (the (:face-normal-vector :bottom))
                                      (the-child :angle)
                                      (the (:face-normal-vector :left)))
                     :right (the (:face-normal-vector :right)))
          :shoulder-point 
          (translate (the :torso (:edge-center :top (the-child :side)))
                     (the-child :side) (half (the-child :width)) :down
                     (the :shoulder-height))
          :angle (ecase (the-child :side) (:left (the :arm-angle-left))
                        (:right (the :arm-angle-right)))
          :pincer-distance (ecase (the-child :side) 
                             (:left (the :pincer-distance-left))
                             (:right (the :pincer-distance-right))))))
      "))) 

      ((:boxed-figure :caption "Robot Arm"
                      :label "code:robot-arm")
       (:verbatim "


 (define-object robot-arm (base-object)

   :input-slots
   (side
    shoulder-point
    angle
    pincer-distance)

   :computed-slots
   ((display-controls (list :color :blue)))

   :objects
   ((arm :type 'box)
    (thumb :type 'box
           :display-controls (list :color :red)
           :width (the :hand :width)
           :height (the :hand :height)
           :length (the :hand :length)
           :center (translate (the :hand :center) 
                              (the :side) (the :pincer-distance)))
    (hand :type 'box
          :display-controls (list :color :green)
          :center (translate (the :center) :down
                             (+ (half (the :height)) 
                                (half (the-child :height)))
                             (the :side)
                             (- (- (half (the :width)) 
                                   (half (the-child :width)))))
          :height (* (the :height) 0.15)
          :width (* (the :width) 0.2)
          :length (half (the :length)))))      
      "))
      
      ((:boxed-figure :caption "Robot Base"
                      :label "code:robot-base")
       (:verbatim "


 (define-object robot-base (base-object)

   :objects
   ((leg :type 'box
         :height (* 0.9 (the :height))
         :center (translate (the :center) :up
                            (- (half (the :height)) 
                               (half (the-child :height)))))
    (foot :type 'box
          :height (* 0.1 (the :height))
          :width (twice (twice (the :width)))
          :length (twice (twice (twice (the :length))))
          :center (translate (the :center) :down
                             (- (half (the :height)) 
                                (half (the-child :height)))))))      
      ")))
      
      ((:section :title "Using the App")
       "Figures "
       (:ref "fig:robot-front")
       " and "
       (:ref "fig:robot-swing")
       " show some examples of a user having interacted with the application, resulting in
a specified standard 3D view of the robot or the robot's body parts rotated to specified
angles. Note also the extra hyperlinks at the upper-left in Figure "
       (:ref "fig:robot-front")
       ", which are a result of the GDL/GWL session being in "
       (:emph (:indexed "development mode"))
       ". Development mode can be entered as follows:"
       (:index "gwl:*developing?*")
       (:verbatim "(setq gwl:*developing?* t)")
       "The three standard links provided by development mode are as follows:"
       ((:list :style :description)
	((:item :word "Update!")
	 "will essentially re-instantiate the object hierarchy from the current object downward, taking into account any new or altered
definitions you have compiled since the objects were last demanded. However, any "
	 (:texttt ":settable")
	 " slots which have been altered will retain their values to the extent feasible.")
	((:item :word "Full Update!")
	 "will perform and Update all the way from the root object.")
	((:item :word "Break")
	 "will cause a Common Lisp break level to be entered, with the parameter "
	 (:texttt "self")
	 " set to the object instance corresponding to the current web page.")
	((:item :word (:indexed "TaTu"))
	 "will respond with the development view of the object, as described in the file "
	 (:texttt "tatu.txt") "."))
       ((:image-figure :image-file "robot-front.png"
		       :caption "Front View of Robot"
		       :label "fig:robot-front"))
      
       ((:image-figure :image-file "robot-swing.png"
                       :caption "Robot with some non-Default Angles"
                       :label "fig:robot-swing")))))


