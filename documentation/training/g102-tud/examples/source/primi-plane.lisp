;;
;; Copyright 2012 Genworks International and the Delft University of
;; Technology
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

(in-package :gdl-user)

(defparameter *data-folder* 
  (make-pathname :name nil 
		 :type nil 
		 :defaults (merge-pathnames "../../data/" 
					    #+allegro excl:*source-pathname*
					    #+lispworks dspec:*source-pathname*
					    ;; in future: (glisp:source-pathname)
					    )))

(define-object primi-plane (base-object)

  :input-slots
  ((data-folder *data-folder*)
   (aircraft-data-file (merge-pathnames "aircraft-1.dat" (the data-folder)))
   (points-data-file (merge-pathnames "NACA_0012.dat" (the data-folder))))

  :objects
  ((data :type 'aircraft-data
	 :hidden? t
	 :parameters (with-open-file (in (the aircraft-data-file)) (read in))
	 :points-data (with-open-file (in (the points-data-file)) (read in)))
   
   (canonical-profile :type 'profile-curve
		      :hidden? t
		      :points-data (the data points-data))

   (wing-assy :type 'box-wings
	      :c-root (the data wing-c-root)
	      :c-tip (the data wing-c-tip)
	      :span (the data wing-span)
	      :root-center (translate (the center) 
				      :down (- (the fuselage radius) (half (the-child thickness)))
				      :front (* 1/6 (the fuselage length)))
	      :thickness (the data wing-thickness)
	      :dihedral (the data wing-dihedral)
	      :canonical-profile (the canonical-profile)
	      :display-controls (list :color :green)
	      )
   
   (tail-assy :type 'box-tail
	      :c-root (the data tail-c-root)
	      :c-tip (the data tail-c-tip)
	      :span (the data tail-span)
	      :root-center (translate (the center) 
				      :down (- (the fuselage radius) (half (the-child thickness)))
				      :rear (- (half (the fuselage length))
					       (half (the-child c-root))))
	      :rudder-root-center (translate (the-child root-center) :up 
					     (- (twice (the fuselage radius))
						(half (the-child thickness))))
	      :thickness (the data tail-thickness)
	      :dihedral (the data tail-dihedral)
	      :rudder-span  (the data rudder-span)
	      :rudder-thickness (the data rudder-thickness)
	      :rudder-c-root (the data rudder-c-root)
	      :rudder-c-tip (the data rudder-c-tip)
	      :display-controls (list :color :blue)
	      )


   (fuselage :type 'cylinder-fuselage
	     :d (the data fuselage-diameter)
	     :l (the data fuselage-length)
	     :display-controls (list :color :red)
	     )))


(define-object aircraft-data ()
  :input-slots (wing-span wing-thickness wing-dihedral
			  wing-c-root wing-c-tip Tmax rho C-of-F 
			  fuselage-diameter fuselage-length
			  tail-c-root tail-c-tip tail-span
			  tail-thickness tail-dihedral
			  points-data
			  ))


(define-object profile-curve (fitted-curve)
  :input-slots (points-data)
  
  :computed-slots ((data-name (string-append (first (the points-data))
					     (second  (the points-data))))

		   (point-coordinates (rest (rest (the points-data))))

		   (x-coords (plist-keys (the point-coordinates)))
		   (y-coords (plist-values (the point-coordinates)))
		   
		   (max-x (most 'get-x (the points)))
		   (min-x (least 'get-x (the points)))
		   (max-y (most 'get-y (the points)))
		   (min-y (least 'get-y (the points)))

		   (chord (- (get-x (the max-x))
			     (get-x (the min-x))))

		   (max-thickness (- (get-y (the max-y)) 
				     (get-y (the min-y))))
		   
		   (points (mapcar #'(lambda(x y) (make-point x y 0)) 
				   (the x-coords)
				   (the y-coords)))))


(define-object cylinder-fuselage (cylinder)
  :input-slots (d l)

  :computed-slots ((radius (half (the d)))
		   (length (the l))))

(define-object box-wings (base-object)
  :input-slots (root-center span c-root c-tip thickness dihedral canonical-profile)
  

  :objects
  ((wings :type 'box-wing
	  :sequence (:size 2)
	  :root-point (the root-center)
	  :side (ecase (the-child index) (0 :right) (1 :left))
	  :span (the span)
	  :c-root (the c-root)
	  :c-tip (the c-tip)
	  :thickness (the thickness)
	  :canonical-profile (the canonical-profile)
	  ;;
	  ;; Left wing will get a left-handed coordinate system and be a mirror of the right.
	  ;;
	  :orientation (let* ((hinge (the (face-normal-vector (ecase (the-child side)
								(:right :front)
								(:left :rear)))))
			      (right (rotate-vector-d (the (face-normal-vector (the-child side)))
						      (the dihedral) 
						      hinge)))
			 (alignment :right right
				    :top (cross-vectors hinge right)
				    :front (the (face-normal-vector :front)))))))


(define-object box-tail (box-wings)
  :input-slots (rudder-root-center rudder-span rudder-thickness 
				   rudder-c-root rudder-c-tip)

  :objects ((rudder :type 'box-wing
		    :root-point (the rudder-root-center)
		    :span (the rudder-span)
		    :c-root  (the rudder-c-root)
		    :c-tip (the rudder-c-tip)
		    :thickness (the rudder-thickness)
		    :orientation (alignment :right (the (face-normal-vector :top))
					    :top (the (face-normal-vector :left))))))



(define-object box-wing (box)
  :input-slots (root-point side span c-root c-tip thickness canonical-profile)
  
  :computed-slots ((width (the span))
		   (length (the c-root))
		   (height (the thickness))

		   (center (translate-along-vector (the root-point) 
						   (the (face-normal-vector :right))
						   (half (the width))
						   )))

  :objects ((box :type 'box
		 :hidden? t
		 :display-controls (list :color :yellow :transparency 0.7))


	    (root-profile :type 'boxed-curve
			  :curve-in (the canonical-profile)
			  :orientation (alignment :top (the (face-normal-vector :right))
						  :rear (the (face-normal-vector :top))
						  :right (the (face-normal-vector :rear)))
			  :scale-y (/ (the thickness) (the canonical-profile max-thickness))
			  :scale-x (/ (the c-root) (the canonical-profile chord))
			  :center (the (edge-center :left :front))
			  :hidden? t)
	    
	    (tip-profile :type 'boxed-curve
			 :curve-in (the canonical-profile)
			 :orientation (alignment :top (the (face-normal-vector :right))
						 :rear (the (face-normal-vector :top))
						 :right (the (face-normal-vector :rear)))
			 :scale-y (/ (the thickness) (the canonical-profile max-thickness))
			 :scale-x (/ (the c-tip) (the canonical-profile chord))
			 :center (translate (the (edge-center :right :front))
					    :rear (- (the length) (the c-tip)))
			 :hidden? t)

	    (loft :type 'lofted-surface
		  :curves (list (the root-profile) (the tip-profile)))))
						   
