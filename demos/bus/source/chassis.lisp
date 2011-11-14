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

(define-object chassis (application-mixin)

  :input-slots
  (wheelbase
   track
   datum
   height
   (turn-angle 0 :settable)   
   )

  :computed-slots
  ((ui-display-list-objects (remove-if
			     #'(lambda (item) (typep item 'base-rule-object))
			     (the :children)))
   (frame-width (the :frame :width))
   (frame-height (the :frame :height))
   (left-rotation (- (ecase (the :turn-type)
		       (:left (the :turn-angle))
		       (:right
			(if (the :left-hit-point)
			    (angle-between-vectors-d
			     (the (:knuckles-straight 0) :tie-rod-arm-vector)
			     (subtract-vectors (the :left-hit-point)
					       (the
						   (:knuckles-straight 0)
						 :tie-rod-arm-axis-point))
			     (the (:knuckles-straight 0) :top-vector) t)
			  0)))
		     (the :toe-in-angle)))
   (right-rotation (+ (ecase (the :turn-type)
			(:right (the :turn-angle))
			(:left
			 (if (the :right-hit-point)
			     (angle-between-vectors-d
			      (the (:knuckles-straight 1) :tie-rod-arm-vector)
			      (subtract-vectors (the :right-hit-point)
						(the
						    (:knuckles-straight 1)
						  :tie-rod-arm-axis-point))
			      (the (:knuckles-straight 1) :top-vector) t)
			   0)))
		      (the :toe-in-angle)))
   (tie-rod-length (3d-distance (the (:knuckles-straight 0) :tie-rod-arm-end)
				(the (:knuckles-straight 1) :tie-rod-arm-end)))
   (left-hit-point (let ((circle (the (:knuckles-straight 0) :tie-rod-arm-circle))
			 (sphere (the (:knuckles 1) :tie-rod-sphere)))
		     (inter-circle-sphere (getf circle :center)
					  (getf circle :radius)
					  (getf circle :axis-vector)
					  (getf sphere :center)
					  (getf sphere :radius) nil)))
   (right-hit-point (let ((circle
			   (the (:knuckles-straight 1) :tie-rod-arm-circle))
			  (sphere (the (:knuckles 0) :tie-rod-sphere)))
		      (inter-circle-sphere (getf circle :center)
					   (getf circle :radius)
					   (getf circle :axis-vector)
					   (getf sphere :center)
					   (getf sphere :radius) t)))
   
   (front-overhang 25 :settable)
   (rear-overhang 72 :settable)
   (frame-rail-height 6 :settable)
   (frame-rail-thickness 2.5 :settable)
   (turn-type :left :settable)
   
   (toe-in-angle 0.5 :settable)
   (camber-angle 1.0 :settable)
   (caster-angle -1.0 :settable)
   (kingpin-inclination-angle 7 :settable)
   (tie-rod-arm-length :medium :settable)
   (tie-rod-arm-setting :medium :settable)
   
   (ackermann-data 
    (let ((chassis (make-object 'chassis 
				:wheelbase (the wheelbase)
				:track (the track)
				:datum (the datum)
				:height (the height)))
	  (left-angles (list-of-numbers 1 35)) right-angles)
      (the-object chassis (set-slots! 
			   (list :front-overhang (the front-overhang)
				 :rear-overhang (the rear-overhang)
				 :frame-rail-height (the frame-rail-height)
				 :frame-rail-thickness (the frame-rail-thickness)
				 :toe-in-angle 0
				 :camber-angle (the camber-angle)
				 :caster-angle (the caster-angle)
				 :kingpin-inclination-angle (the kingpin-inclination-angle)
				 :tie-rod-arm-length (the tie-rod-arm-length)
				 :tie-rod-arm-setting (the tie-rod-arm-setting)
				 :turn-type :left)))
      (setq right-angles
	(let (result ideal-result)
	  (dolist (left-angle left-angles (list (nreverse result) (nreverse ideal-result)))
	    (the-object chassis (set-slot! :turn-angle left-angle))
	    (push (the-object chassis right-rotation) result)
			   
	    (let ((rear-axle-intersect (inter-line-plane (the (axles 1) center)
							 (the (face-normal-vector :rear))
							 (the-object chassis (knuckles 0) center)
							 (the-object chassis (knuckles 0) 
								     (face-normal-vector :right)))))
			     
	      (push (angle-between-vectors-d (the (face-normal-vector :rear))
					     (subtract-vectors (the (knuckles 1) center)
							       rear-axle-intersect)) 
		    ideal-result)))))
			 
      (list :left (cons 0 left-angles) 
	    :right (cons 0 (first right-angles))
	    :ideal-right (cons 0 (second right-angles)))))
   
   
   )

  
  :objects
  ((knuckles :type 'knuckle
	     :sequence (:size 2)
	     :side (ecase (the-child :index) (0 :left) (1 :right))
	     :top-vector (getf (the (:axles 0) :kingpin-vectors) (the-child :side))
	     :front-vector (cross-vectors (the-child :top-vector)
					  (the (:face-normal-vector (the-child :side))))
	     :left-vector (cross-vectors (the-child :top-vector)
					 (ecase (the-child :side)
					   (:left
					    (reverse-vector (the-child :front-vector)))
					   (:right (the-child :front-vector))))
	     :pass-down (:tie-rod-length :tie-rod-arm-length :tie-rod-arm-setting)
	     :spindle-downward-pitch (+ (the :kingpin-inclination-angle) 
					(the :camber-angle))
	     :center (the (:axles 0) (:kingpins (the-child :index)) :center)
	     :transformation-matrix 
	     (alignment :top (the-child :top-vector) :front
			(rotate-vector-d (the-child :front-vector)
					 (ecase
					     (the-child :side)
					   (:left
					    (the :left-rotation))
					   (:right
					    (the :right-rotation)))
					 (the-child :top-vector))
			:left
			(rotate-vector-d (the-child :left-vector)
					 (ecase
					     (the-child :side)
					   (:left
					    (the :left-rotation))
					   (:right
					    (the :right-rotation)))
					 (the-child :top-vector)))
	     :orientation (the-child :transformation-matrix))
   (tie-rod :type 'c-cylinder
	    :radius 1.5
	    :start (the (:knuckles 0) :tie-rod-arm-end)
	    :end (the (:knuckles 1) :tie-rod-arm-end))
   (frame :type 'frame
	  :width (+ (the :wheelbase) (the :front-overhang) (the :rear-overhang))
	  :length (* 4/9 (the :track))
	  :height (the :frame-rail-height)
	  :thickness (the :frame-rail-thickness)
	  :center (translate (the :datum) :right (half (the-child :width))))
   (axles :type (:sequence '(axle rear-axle))
	  :sequence (:size 2)
	  :center (translate (the :datum) :down
			     (+ (half (the :frame :height)) (half (the-child :height)))
			     :right
			     (ecase (the-child :index)
			       (0 (the :front-overhang))
			       (1 (+ (the :front-overhang) (the :wheelbase)))))
	  :transformation-matrix (alignment :left
					    (rotate-vector-d (the
								 (:face-normal-vector
								  :left))
							     (the :caster-angle)
							     (the
								 (:face-normal-vector
								  :front))))
	  :orientation (the-child :transformation-matrix)
	  :height 10
	  :length (- (the :track)
		     (ecase (the-child :index)
		       (0 (twice (the (:wheels-front 0) :length)))
		       (1 (twice (the (:wheels-rear 0) :length)))))
	  :pass-down (:kingpin-inclination-angle)
	  :width 10
	  :display-controls (list :color :black :line-thickness 2))
   (wheels-front :type 'wheel
		 :sequence (:size 2)
		 :center (the (:knuckles (the-child :index)) :spindle-center)
		 :transformation-matrix (alignment :front
						   (the (:knuckles (the-child :index))
						     :spindle-vector))
		 :orientation (the-child :transformation-matrix)
		 :length 9
		 :radius 19
		 :inner-radius 7
		 :display-controls (list :color "#333333"  :shininess 0.6 :specular-color "#999999")
		 )
   
   (wheels-rear :type 'wheel
		:sequence (:size 2)
		:center (translate (getf (the (:axles 1) :wheel-centers)
					 (ecase (the-child :index) (0 :left) (1 :right)))
				   (ecase (the-child :index) (0 :front) (1 :rear))
				   (half (the-child :length)))
		:transformation-matrix (alignment :front
						  (the (:face-normal-vector
							(ecase
							    (the-child :index)
							  (0 :front)
							  (1 :rear)))))
		:orientation (the-child :transformation-matrix)
		:display-controls (list :color "#333333"  :shininess 0.6 :specular-color "#999999")
		:length 15
		:radius 19
		:inner-radius 7)
   

   (rule-ackermann :type 'rule-ackermann
		   :ackermann-data (list :left (getf (the ackermann-data) :left)
					 :right (getf (the ackermann-data) :right))
		   :ackermann-data-ideal (list :left (getf (the ackermann-data) :left)
					       :right (getf (the ackermann-data) :ideal-right))))

  :hidden-objects
  ((knuckles-straight 
    :type 'knuckle
    :sequence (:size 2)
    :center (the (:axles 0) (:kingpins (the-child :index)) :center)
    :side (ecase (the-child :index) (0 :left) (1 :right))
    :top-vector (getf (the (:axles 0) :kingpin-vectors) (the-child :side))
    :front-vector (cross-vectors (the-child :top-vector)
				 (the (:face-normal-vector (the-child :side))))
    :left-vector (cross-vectors (the-child :top-vector)
				(ecase (the-child :side)
				  (:left
				   (reverse-vector (the-child :front-vector)))
				  (:right (the-child :front-vector))))
    :spindle-downward-pitch (+ (the :kingpin-inclination-angle) (the :camber-angle))
    :pass-down (:tie-rod-arm-length :tie-rod-arm-setting)
    :transformation-matrix (alignment :top (the-child :top-vector) :front
				      (rotate-vector-d (the-child :front-vector)
						       (the :toe-in-angle)
						       (ecase
							   (the-child :side)
							 (:left
							  (reverse-vector
							   (the-child :top-vector)))
							 (:right
							  (the-child :top-vector))))
				      :left
				      (rotate-vector-d (the-child :left-vector)
						       (the :toe-in-angle)
						       (ecase
							   (the-child :side)
							 (:left
							  (reverse-vector
							   (the-child :top-vector)))
							 (:right
							  (the-child :top-vector)))))
    :orientation (the-child :transformation-matrix))))
  
