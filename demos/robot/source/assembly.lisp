;;
;; Copyright 2002, 2009 Genworks International
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

#|

NOTE:

This is the original Robot example and uses the old-style user
interface approach. It is not recommended to continue with this
approach; the preferred approach is to use form-control objects for             
the form fields, as will be in other newer examples soon to be added
to the src/ directory distributed with GDL.  

|#


(in-package :robot)

(define-object army (base-object)
  :objects
  ((robots :type 'assembly
           :sequence (:size 50)
           :center (translate (the center) :right (* 10 (the-child index))))))

(define-object   assembly (application-mixin)
  
  :input-slots
  ((width 5 :settable)
   (length 2 :settable)
   (torso-type 'box :settable)
   (height 10 :settable)
   (head-angle 0 :settable)

   (body-angle 20 :settable)
   #+nil
   (body-angle (let ((angle (or (parse-integer (progn ;;(format t "~&Enter a number for body angle: ~%")
						      (let ((data (read-line)))
							(print-variables data)
							data)) :junk-allowed t) 20)))
		 (print-variables angle)
		 angle) :settable)
   (arm-angle-right 0 :settable)
   (arm-angle-left 0 :settable)
   (pincer-distance-right (number-round (* .15 3/5 (the width)) 3) :settable)
   (pincer-distance-left (number-round (* .15 3/5 (the width)) 3) :settable)
   
   
   
   (transparency .2)
   (shininess .8)
   
   (graphics-width 460)
   
   )

  :computed-slots
  (
   
   (image-file "~/tmp/slack_in.gif")

   (strings-for-display "Robot Assembly")
   (ui-display-list-objects (list ;;(the bounding-bbox) 
                             (the robot)))
   (background-color :white)
   (foreground-color :blue)
   )

  :objects
  (
   (robot :type 'robot
          :pass-down (torso-type 
                      head-angle width length height transparency shininess
                      body-angle arm-angle-right arm-angle-left
                      pincer-distance-right pincer-distance-left))))

#+nil
(define-lens (html-format assembly)()
  
  :skin infinite
  
  :output-functions
  ((model-inputs
    ()
    (with-html-output (*stream* nil :indent t)
      (htm ((:table  :cellpadding 0)
            (:tr ((:td :colspan 2) (:b "Dimensions:")))
            (dolist (slot (list :width :length :height))
              (htm 
               (:tr ((:td :bgcolor :yellow)
                     (str (string-capitalize slot)))
                    (:td ((:input :type :string :name slot :size 5
                                  :value (format nil "~a" 
                                                 (the (evaluate slot)))))))))
      
            ((:input :type :hidden :value "hidden value" :name :hidden-field))
      
            (:tr ((:td :colspan 2) :br))
            (:tr ((:td :colspan 2) (:b "Angles:")))
            (dolist (angle '(("Head" :head-angle)("Body" :body-angle)
                             ("Left Arm" :arm-angle-left) 
                             ("Right Arm" :arm-angle-right)))
              (htm
               (:tr ((:td :bgcolor :yellow) 
                     (htm (str (first angle))))
                    (:td ((:input 
                           :type :string :name (second angle) 
                           :size 5 :value 
                           (format nil "~a" (the (evaluate (second angle))))))))))
            (:tr ((:td :colspan 2) :br))
            (:tr ((:td :colspan 2) (:b "Grip Opening:")))
            (dolist (side (list :left :right))
              (htm
               (:tr 
                ((:td :bgcolor :yellow) (str (string-capitalize side)))
                (:td 
                 ((:input 
                   :type :string 
                   :name (format nil "pincer-distance-~a" side) 
                   :size 5 
                   :value 
                   (the (evaluate (make-keyword 
                                   (format nil "pincer-distance-~a" side))))))))))
            (:tr ((:td :colspan 2) :br))
            (:tr ((:td :colspan 2 :align :center)
                  ((:input :type :submit :value " OK " :name :refresh))))))))))


(define-lens (html-format assembly)()
  :output-functions
  ((model-inputs
    ()
    (html 
     ((:table  :cellpadding 0)
      (:tr ((:td :colspan 2) (:b "Dimensions:")))
      (dolist (slot (list :width :length :height))
        (html 
         (:tr ((:td :bgcolor :yellow)
               (:princ (string-capitalize slot)))
              (:td ((:input :type :string :name slot :size 5
                            :value (format nil "~a" 
                                           (the (evaluate slot)))))))))
      
      ((:input :type :hidden :value "hidden value" :name :hidden-field))
      
      (:tr ((:td :colspan 2) :br))
      (:tr ((:td :colspan 2) (:b "Angles:")))
      (dolist (angle '(("Head" :head-angle)("Body" :body-angle)
                       ("Left Arm" :arm-angle-left) 
                       ("Right Arm" :arm-angle-right)))
        (html
         (:tr ((:td :bgcolor :yellow) 
               (html (:princ (first angle))))
              (:td ((:input 
                     :type :string :name (second angle) 
                     :size 5 :value 
                     (format nil "~a" (the (evaluate (second angle))))))))))
      (:tr ((:td :colspan 2) :br))
      (:tr ((:td :colspan 2) (:b "Grip Opening:")))
      (dolist (side (list :left :right))
        (html
         (:tr 
          ((:td :bgcolor :yellow) (:princ (string-capitalize side)))
          (:td 
           ((:input 
             :type :string 
             :name (format nil "pincer-distance-~a" side) 
             :size 5 
             :value 
             (the (evaluate (make-keyword 
                             (format nil "pincer-distance-~a" side))))))))))
      (:tr ((:td :colspan 2) :br))
      (:tr ((:td :colspan 2 :align :center)
            ((:input :type :submit :value " OK " :name :refresh)))))))))



(define-object robot (base-object)

  :input-slots
  ((torso-type 'box :settable)
   head-angle
   body-angle
   arm-angle-right
   arm-angle-left
   pincer-distance-right
   pincer-distance-left
   shininess transparency
   )

  :computed-slots
  ((display-controls (list  :color :orange :line-thickness 2 
                            :transparency (the transparency) 
                            :shininess (the shininess) )))


  :objects
  ((base :type 'robot-base
         :display-controls (merge-display-controls (list :color :blue-sky))
         :height (* (the height) 0.4)
         :width (* (the width) 0.2)
         :length (* (the length) 0.2)
         :center (translate (the :center) :down (* (the :height) 0.3)))
   (body :type 'robot-body
         :height (* (the :height) 0.6)
         :center (translate (the :center) :up (* (the :height) 0.2))
         :orientation (alignment :right
                                 (rotate-vector-d 
                                  (the (:face-normal-vector :right))
                                  (the :body-angle)
                                  (the (:face-normal-vector :top))))
         :pass-down (head-angle torso-type
                     arm-angle-right arm-angle-left 
                     pincer-distance-left pincer-distance-right))


   ))


(define-object robot-body (base-object)
  :input-slots
  ((torso-type 'box)
   head-angle arm-angle-left arm-angle-right
   pincer-distance-left pincer-distance-right
   (shoulder-height (* (the :torso :height) 0.1)))


  :objects
  ((torso :type (the torso-type)
          :height (* (the :height) 0.85)
          :width (* (the :width) 0.7)
          :radius (half (the-child width))
          :center (translate (the :center) 
                             :down (- (half (the :height)) 
                                      (half (the-child :height)))))
   
   (arms :type 'robot-arm :sequence (:size 2)
         :side (ecase (the-child :index) (0 :left) (1 :right))
         :display-controls (merge-display-controls 
                            (list :transparency 0 
                                  :color (ecase (the-child index)
                                           (0 :green-spring-medium)
                                           (1 :red))))
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
                            (:right (the :pincer-distance-right))))
   
   
   (head :type 'box 
         :height (- (the :height) (the :torso :height))
         :width (* (the :width) 0.25) :length (half (the :length))
         :center (translate (the :center) :up
                            (- (half (the :height)) (half (the-child :height))))
         
         
         :display-controls (merge-display-controls (list :color :black :transparency 0))
         :orientation (alignment :right
                                 (rotate-vector-d 
                                  (the (:face-normal-vector :right))
                                  (the :head-angle)
                                  (the (:face-normal-vector :top)))))



   
   ))


(define-object robot-arm (base-object)

  :input-slots
  (side
   shoulder-point
   angle
   pincer-distance
   
 )

  :objects
  ((arm :type 'box)
   

   (thumb :type 'box
          :width (the :hand :width)
          :height (the :hand :height)
          :length (the :hand :length)
          :center (translate (the :hand :center) 
                             (the :side) (the :pincer-distance)))
   (hand :type 'box
         :display-controls (merge-display-controls (list :color :black :transparency 0))
         :center (translate (the :center) :down
                            (+ (half (the :height)) 
                               (half (the-child :height)))
                            (the :side)
                            (- (- (half (the :width)) 
                                  (half (the-child :width)))))
         :height (* (the :height) 0.15)
         :width (* (the :width) 0.2)
         :length (half (the :length)))))
  

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

#+nil
(publish :path "/demos/roboti"
         :function 
         #'(lambda(req ent)
             (gwl-make-object req ent "robot:assembly"  :skin 'gwl:infinite)))



(publish :path "/demos/robot"
         :function 
         #'(lambda(req ent)
             (gwl-make-object req ent "robot:assembly")))



