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


(define-object primi-plane (base-object)
  
  :input-slots ((data-folder *data-folder*)
		(data-file-name "aircraft-3.dat")
		(points-file-name "NACA_0012.dat"))


  :computed-slots ((data-file-path (merge-pathnames (the data-file-name)
						    (the data-folder)))

		   (points-file-path (merge-pathnames (the points-file-name)
						      (the data-folder)))

		   (dihedral (the data wing-dihedral) :settable))

  :hidden-objects
  ((data :type 'aircraft-data
	 :parameters (with-open-file (in (the data-file-path))
		       (read in))
	 :points-data (with-open-file (in (the points-file-path))
				       (read in)))

   (canonical-profile :type 'profile-curve
		      :points-data (the data points-data)))

  :objects
  ((wing-assy :type 'box-wings
	      :c-root (the data wing-c-root)
	      :c-tip (the data wing-c-tip)
	      :span (the data wing-span)
	      :root-center (translate (the center) 
				      :down (- (the fuselage radius) 
					       (half (the-child thickness)))
				      :front (* 1/6 (the fuselage length)))
	      :thickness (the data wing-thickness)
	      :dihedral (the dihedral)
	      :display-controls (list :color :green)
	      :canonical-profile (the canonical-profile))

   (tail-assy :type 'box-tail
	      :c-root (the data tail-c-root)
	      :c-tip (the data tail-c-tip)
	      :span (the data tail-span)
	      :root-center (translate (the center) 
				      :down (- (the fuselage radius) 
					       (half (the-child thickness)))
				      :rear (- (half (the fuselage length))
					       (the-child c-root)))
				      

	      :fin-root-center (translate (the-child root-center)
					  :up (* 4/3 (the fuselage radius)))


	      :thickness (the data tail-thickness)
	      :dihedral (the data tail-dihedral)
	      
	      :fin-span (the data fin-span)
	      :fin-c-root (the data fin-c-root)
	      :fin-c-tip (the data fin-c-tip)
	      :fin-thickness (the data fin-thickness)

	      :display-controls (list :color :blue)
	      :canonical-profile (the canonical-profile))

   (fuselage :type 'cylinder-fuselage
	     :d (the data fuselage-diameter)
	     :l (the data fuselage-length)
	     :cross-section-percents (the data fuselage-cross-section-percents)
	     :display-controls (list :color :red))))


(define-object profile-curve (fitted-curve)

  :input-slots (points-data)
  
  :computed-slots ((point-coordinates (rest (rest (the points-data))))

		   (x-coords (plist-keys (the point-coordinates)))
		   (y-coords (plist-values (the point-coordinates)))
		   

		   (x-max (most 'get-x (the points)))
		   (x-min (least 'get-x (the points)))

		   (chord (- (get-x (the x-max))
			     (get-x (the x-min))))

		   (y-max (most 'get-y (the points)))
		   (y-min (least 'get-y (the points)))

		   (max-thickness (- (get-y (the y-max))
				     (get-y (the y-min))))
		   
		   (points (mapcar #'(lambda (x y) (make-point x y 0))
				   (the x-coords)
				   (the y-coords)))))


(define-object aircraft-data ()
  :input-slots (wing-span 
		wing-c-root 
		wing-c-tip 
		wing-thickness 
		wing-dihedral 
		Tmax 
		rho 
		C-of-F 
 		fuselage-diameter
		fuselage-length
		fuselage-cross-section-percents 
		tail-span
		tail-c-root
		tail-c-tip
		tail-thickness
		tail-dihedral
		fin-span 
		fin-c-root
		fin-c-tip
		fin-thickness
		
		points-data
		))



(define-object cylinder-fuselage (cylinder)
  :input-slots (d l cross-section-percents)

  :computed-slots ((radius (half (the d)))
		   (length (the l))
		   
		   (section-offset-percentages (plist-keys (the cross-section-percents)))

		   (section-centers (let ((nose-point (the (face-center :front))))
				      (mapcar #'(lambda(percentage)
						  (translate nose-point :rear
							     (* percentage 1/100
								(the length))))
					      (the section-offset-percentages))))

		   (section-diameter-percentages (plist-values (the cross-section-percents)))
		   
		   (section-radii (mapcar #'(lambda(percentage)
					      (* 1/100 percentage (the radius)))
					  (the section-diameter-percentages))))


  :hidden-objects ((section-curves 
		    :type 'arc-curve
		    :sequence (:size (length (the section-centers)))
		    :center (nth (the-child index) (the section-centers))
		    :radius (nth (the-child index) (the section-radii))
		    :orientation (alignment :top (the (face-normal-vector :rear)))))

  :objects
  ((floor-plane :type 'rectangular-surface
		:display-controls (list :color :black)
		:width (* (the radius) 4)
		:length (* (the length) 3/2))

   (merged :type 'merged-solid
	   :brep (the floor-plane)
	   :other-brep (the loft)
	   :make-manifold? t)
   
   (regioned :type 'regioned-solid
	     :display-controls nil
	     :brep (the merged))

   (loft :type 'lofted-surface
	 :end-caps-on-brep? t
	 :curves (list-elements (the section-curves)))))


(define-object box-wings (base-object)
  :input-slots (root-center span c-root c-tip thickness dihedral
			    canonical-profile)
  
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
			      (right (rotate-vector-d (the (face-normal-vector 
							    (the-child side)))
						      (the dihedral) 
						      hinge)))
			 (alignment :right right
				    :top (cross-vectors hinge right)
				    :front (the (face-normal-vector :front)))))))


(define-object box-tail (box-wings)
  :input-slots (fin-span fin-c-root fin-c-tip fin-thickness
			 fin-root-center)

  :objects
  ((fin :type 'box-wing
	:root-point (the fin-root-center)
	:span (the fin-span)
	:c-root (the fin-c-root)
	:c-tip (the fin-c-tip)
	:thickness (the fin-thickness)
	:orientation (alignment :right (the (face-normal-vector :top))
				:top (the (face-normal-vector :left))))))


(define-object box-wing (box)

  :input-slots (root-point side span c-root c-tip thickness
			   canonical-profile)
  
  :computed-slots ((width (the span))
		   (length (the c-root))
		   (height (the thickness))

		   (center (translate-along-vector (the root-point) 
						   (the (face-normal-vector :right))
						   (half (the width))
						   )))

  :hidden-objects ((box :type 'box
			:display-controls (list :color :orange :transparency 0.7))

		   (root-profile 
		    :type 'boxed-curve
		    :curve-in (the canonical-profile)
		    :scale-y (/ (the thickness) (the canonical-profile max-thickness))
		    :scale-x (/ (the c-root) (the canonical-profile chord))
		    :center (the (edge-center :left :front))
		    :orientation (alignment :top (the (face-normal-vector :right))
					    :right (the (face-normal-vector :rear))
					    :rear (the (face-normal-vector :top))))
	    
		   (tip-profile 
		    :type 'boxed-curve
		    :curve-in (the canonical-profile)
		    :scale-y (/ (the thickness) (the canonical-profile max-thickness))
		    :scale-x (/ (the c-tip) (the canonical-profile chord))
		    :center (translate (the (edge-center :right :front))
				       :rear
				       (- (the c-root) (the c-tip)))
		    :orientation (the root-profile orientation)))

  :objects ((loft :type 'lofted-surface
		  :end-caps-on-brep? t
		  :curves (list (the root-profile) (the tip-profile)))))





;;
;; For convenience, look for data folder two levels up, one level up,
;; and in same directory as source file.
;;
(defparameter *data-folder* 
  (let* ((source-pathname #+allegro excl:*source-pathname*
			  #+lispworks dspec:*source-pathname*
			  ;; in future: (glisp:source-pathname)
			  )
	 (base-path (make-pathname :name nil
				   :type nil
				   :defaults source-pathname
				   ;; in future: (glisp:source-pathname)
				   )))
    (or (probe-file 
	 (merge-pathnames "../../data/" base-path))
	(probe-file 
	 (merge-pathnames "../data/"  base-path))
	(probe-file 
	 (merge-pathnames "data/" base-path))
	
	(error "Your data folder is not there. Please make a data folder with 
aircraft and points data, two or one or zero directory levels up from the
directory of this source file.~%"))))
      
