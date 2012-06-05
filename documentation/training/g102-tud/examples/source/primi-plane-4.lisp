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

(defparameter *data-folder* (merge-pathnames "../../data/" 
					     (make-pathname :name nil
							    :type nil
							    :defaults excl:*source-pathname*
							    ;; in future: (glisp:source-pathname)
							    )))
  
  

(define-object primi-plane (base-object)
  
  :input-slots ((data-folder *data-folder*)
		(data-file-name "aircraft-3.dat")
		(points-file-name "NACA_0012.dat"))


  :computed-slots ((data-file-path (merge-pathnames (the data-file-name)
						    (the data-folder)))

		   (points-file-path (merge-pathnames (the points-file-name)
						      (the data-folder)))

		   (dihedral (the data wing-dihedral) :settable))

  :objects
  ((data :type 'aircraft-data
	 :parameters (with-open-file (in (the data-file-path))
		       (read in))
	 :points-data (with-open-file (in (the points-file-path))
				       (read in)))

   (canonical-profile :type 'profile-curve
		      :points-data (the data points-data)
		      

		      )

   (wing-assy :type 'box-wings
	      :c-root (the data wing-c-root)
	      :c-tip (the data wing-c-tip)
	      :span (the data wing-span)
	      :root-center (translate (the center) 
				      :down (- (the fuselage radius) (half (the-child thickness)))
				      :front (* 1/6 (the fuselage length)))
	      :thickness (the data wing-thickness)
	      :dihedral (the dihedral)
	      :display-controls (list :color :green)

	      :canonical-profile (the canonical-profile)
	      )

   (fuselage :type 'cylinder-fuselage
	     :d (the data fuselage-diameter)
	     :l (the data fuselage-length)
	     :display-controls (list :color :red))))


(define-object profile-curve (fitted-curve)

  :input-slots (points-data)
  
  :computed-slots ((point-coordinates (rest (rest (the points-data))))

		   (x-coords (plist-keys (the point-coordinates)))
		   (y-coords (plist-values (the point-coordinates)))
		   
		   (points (mapcar #'(lambda (x y) (make-point x y 0))
				   (the x-coords)
				   (the y-coords)))
		   
		   (max-x (most 'get-x (the points)))

		   (min-x (least 'get-x (the points)))

		   (max-y (most 'get-y (the points)))

		   (min-y (least 'get-y (the points)))
		   
		   (chord (- (get-x (the max-x))
			     (get-x (the min-x))))
		   (max-thickness (- (get-y (the max-y))
				     (get-y (the min-y))))))


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
		 :display-controls (list :color :orange :transparency 0.7))
	    

	    (root-profile :type 'boxed-curve
			  :curve-in (the canonical-profile)
			  :orientation (alignment :top (the (face-normal-vector :right))
						  :rear (the (face-normal-vector :top))
						  :right (the (face-normal-vector :rear)))
			  :scale-y (/ (the thickness) (the canonical-profile max-thickness))
			  :scale-x (/ (the c-root) (the canonical-profile chord))
			  :center (the (edge-center :left :front))
			  )
	    
	    (tip-profile :type 'boxed-curve
			 :curve-in (the canonical-profile)
			 :orientation (the root-profile orientation)
			 :scale-y (/ (the thickness) (the canonical-profile max-thickness))
			 :scale-x (/ (the c-tip) (the canonical-profile chord))
			 :center (translate (the (edge-center :right :front))
					    :rear (- (the length) (the c-tip))))

	    (loft :type 'lofted-surface
		  :end-caps-on-brep? t
		  :curves (list (the root-profile) (the tip-profile)))))


;;
;; For convenience, look for data folder two levels up, one level up,
;; and in same directory as source file.
;;
(defparameter *data-folder* 
  (or (probe-file 
       (merge-pathnames "../../data/" 
			(make-pathname :name nil
				       :type nil
				       :defaults excl:*source-pathname*
				       ;; in future: (glisp:source-pathname)
				       )))
      (probe-file 
       (merge-pathnames "../data/" 
			(make-pathname :name nil
				       :type nil
				       :defaults excl:*source-pathname*
				       ;; in future: (glisp:source-pathname)
				       )))

      (probe-file 
       (merge-pathnames "data/" 
			(make-pathname :name nil
				       :type nil
				       :defaults excl:*source-pathname*
				       ;; in future: (glisp:source-pathname)
				       )))
      
      (error "Your data folder is not there. Please make a data folder with 
aircraft and points data, two or one or zero directory levels up from the 
directory of this source file.~%")))
      
