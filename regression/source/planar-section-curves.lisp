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


(in-package :gdl-lift-tests)


(define-object planar-section-curves-test (planar-section-curves)
  :computed-slots
  ((points-data '(((0 0 0)(4 1 0)(8 1 0)(10 0 0)(8 -1 0)(4 -1 0)(0 0 0))
		  ((0 0 2) (4 2 2) (8 2 2) (10 0 2) (8 -2 2) (4 -2 2) (0 0 2))
		  ((0 0 4) (4 2 4) (8 2 4) (10 0 4) (8 -2 4) (4 -2 4) (0 0 4))
		  ((0 0 7) (4 1 7) (8 1 7) (10 0 7) (8 -1 7) (4 -1 7) (0 0 7))))
   (surface (the test-surf))
   (plane-normal (the (face-normal-vector :front)))
   (plane-point (make-point 0 0.5 0))
   
   (regression-test-data (mapcar #'(lambda(curve)
				     (multiple-value-list (the-object curve b-spline-data)))
				 (the curves list-elements))))
  
  :objects
  ((test-surf :type 'b-spline-surface
	      :control-points (mapcar #'(lambda(list) (mapcar #'apply-make-point list)) (the points-data)))))
  
(register-test-definition 'planar-section-curves-test)

(define-object planar-section-curves-test-2 (base-object)

  :input-slots ((span 0.5) (c-root-profile 0.3) (c-tip-profile 0.05)
		(large-scale-factor 1000))


  :computed-slots ((regression-test-data (list (the sectioned-skin-small regression-test-data)
					       (the sectioned-skin-small regression-test-data))))
  
  :objects
  ((sectioned-skin-small :type 'sectioned-foil
			 :section-approximation-tolerance 1.0e-12
			 :display-controls (list :color :green)
			 :pass-down (span c-root-profile c-tip-profile))

   (sectioned-skin-large :type 'sectioned-foil
			 :section-approximation-tolerance 1.0e-9
			 :scale-factor (the large-scale-factor)
			 :display-controls (list :color :green)
			 :span (* (the large-scale-factor) (the span))
			 :c-root-profile (* (the large-scale-factor) (the c-root-profile))
			 :c-tip-profile (* (the large-scale-factor) (the c-tip-profile)))))


(define-object sectioned-foil (lofted-surface-test-2)

  :input-slots
  ((section-approximation-tolerance 1.0e-12 :settable)
   (rib-approximation-tolerance 0.001 :settable)
   (spar-positions (list 0.2 0.5 0.7 0.9 0.98))
   (rib-positions '(0.0 0.33 0.66 1.0))
   (scale-factor 1))


  :computed-slots
  ((regression-test-data (mapcar #'(lambda(section-curves)
				     (mapcar #'(lambda(curve)
						 (multiple-value-list
						     (the-object curve b-spline-data)))
					     (list-elements
					      (the-object section-curves curves))))
				 (append (list-elements (the diagonal-curves))
					 (list-elements (the spar-curves))
					 (list-elements (the rib-curves))))))
  
  
  :objects
  ((diagonal-curves :type 'planar-section-curves
		    :hidden? nil
		    :surface self
		    :3d-approximation-tolerance 1.0e-2
		    :display-controls (list :color :purple)
		    :sequence (:size (length (the spar-positions)))
		    :plane-point (translate (the (point 0  0 ))
					    :left (* (- 1 (nth (the-child index) 
							       (the spar-positions)))
						     (the c-root-profile)))
		 
		    :plane-normal (rotate-vector-d
				   (cross-vectors (subtract-vectors (the-child end)
								    (the-child start))
						  (the (face-normal-vector :top)))
				   -.001 
				   (the (face-normal-vector :top)) ) 
				   
		    :pseudo-inputs (start end)
		    :start (translate (the (point 0  0 ))
				      :left (* (- 1 (nth (the-child index) 
							 (the spar-positions)))
					       (the c-root-profile)))   
		    :end (translate (the (point 1  0 ))
				    :left (* (- 1 (nth (the-child index) 
						       (the spar-positions)))
					     (the c-tip-profile))))


   (spar-curves :type 'planar-section-curves
		:hidden? nil
		:surface self
		:3d-approximation-tolerance (the section-approximation-tolerance)
		:display-controls (list :color :red)
		:sequence (:size (length (the spar-positions)))
		:plane-point (translate (the (point 0  0 ))
					:left (* (- 1 (nth (the-child index) 
							   (the spar-positions)))
						 (the c-root-profile)))
		 
		:plane-normal (cross-vectors (subtract-vectors (the-child end)
							       (the-child start))
					     (the (face-normal-vector :top)))
		 
		:pseudo-inputs (start end)
		:start (translate (the (point 0  0 ))
				  :left (* (- 1 (nth (the-child index) 
						     (the spar-positions)))
					   (the c-root-profile)))   
		:end (translate (the (point 1  0 ))
				:left (* (- 1 (nth (the-child index) 
						   (the spar-positions)))
					 (the c-tip-profile))))


   (rib-planar-contours :type 'planar-contour-surface
			:display-controls (list :isos (list :n-u 8 :n-v 8))
			:sequence (:size (the rib-curves number-of-elements))
			:island-curve (the (rib-curves (the-child index)) (curves 0)))

   (rib-shells :type 'shelled-solid
	       :sequence (:size (the rib-curves number-of-elements))
	       :display-controls (list :isos (list :n-u 8 :n-v 8))
	       :brep (the (rib-planar-contours (the-child index)))
	       :distance (* (the scale-factor) 0.001))
   
   (rib-curves :type 'planar-section-curves
	       :hidden? nil
	       ;;:3d-approximation-tolerance (* (the section-approximation-tolerance) 100)
	       :3d-approximation-tolerance (the rib-approximation-tolerance)
	       :display-controls (list :color :blue)
	       :surface self
	       :sequence (:size (length (the rib-positions)))
	       :plane-point (translate (the (point 0  0 ))
				       :rear (* (- 1 (nth (the-child index)
							  (the rib-positions)))
						(the span)))   
	       :plane-normal (the (face-normal-vector :rear)))))

#+nil
(register-test-definition 'planar-section-curves-test-2)
