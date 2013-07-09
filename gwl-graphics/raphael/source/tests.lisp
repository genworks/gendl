;;
;; Copyright 2002-2011 Genworks International 
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

(in-package :gwl-user)

(define-object dd (base-ajax-sheet base-object)
 
  :computed-slots ((use-raphael? t)

		   (too-far? (> (3d-distance (first (the poly-1 vertex-list))
					     (first (the poly-2 vertex-list)))
				3))

		   (main-sheet-body (with-cl-who-string ()
				      (str (the development-links))
				      (str (the viewport main-div))
				      (str (the drop-coord-section main-div))))

		   (datum (when (the viewport dropped-x-y)
			    (ecase (the poly-type)
			      ((global-polyline rounded-corner-triangle)
			       (make-point (get-x (the viewport dropped-x-y))
					   (- (get-y (the viewport dropped-x-y)) 1) 0))
			      (tri-cyl
			       (make-point (+ (get-x (the viewport dropped-x-y))
					      (the poly-0 radius))
					   (- (get-y (the viewport dropped-x-y)) 
					      (the poly-0 radius)) 0)))))
		   
		   (poly-1-datum (if (and (the datum) (eql (the dropped-object) (the poly-1)))
				     (the datum) (make-point 0 0 0)))

		   (poly-2-datum (if (and (the datum) (eql (the dropped-object) (the poly-2)))
				     (the datum) (make-point 2 0 0)))

		   (dropped-object (when (the viewport dropped-object)
				     (the  (follow-root-path  (the viewport dropped-object)))))

		   ;;(poly-type 'tri-cyl)
		   (poly-type 'global-polyline)
		   ;;(poly-type 'rounded-corner-triangle)

		   (top-vector (the (face-normal-vector :top))))
  


  :functions ((triangle-vertices 
	       (datum)
	       (list datum (translate datum :right 1)
		     (translate datum :right 1 :rear 1) datum)))


  :objects ((poly-0 :type (the poly-type)
		    :pass-down (top-vector)
		    :display-controls (list :color :red :fill-color :orange
					    )
		    :vertex-list (the (triangle-vertices (translate (the center) :left 2))))

	    
	    (poly-1-up :type (the poly-type) 
		       :pass-down (top-vector)
		       :display-controls (list :color :red :fill-color :orange)
		       :vertex-list (the (triangle-vertices (translate (the center) :rear 2))))

	    (poly-1-down :type (the poly-type)
			 :pass-down (top-vector) 
			 :display-controls (list :color :red :fill-color :orange)
			 :vertex-list (the (triangle-vertices (translate (the center) :front 2))))

	    (poly-1 :type (the poly-type) 
		    :pass-down (top-vector)
		    :display-controls (list :color :red :fill-color :blue
					    :drag-controls :drag-and-drop
					    ;; other possible values are :drag, :drop
					    )
		    :vertex-list (the (triangle-vertices (the poly-1-datum))))

	    (poly-2 :type (the poly-type) 
		    :pass-down (top-vector)
		    :display-controls (list :color :black 
					    :fill-color (if (the too-far?) 
							    :red
							    :green)
					    :drag-controls :drag-and-drop
					    ;; other possible values are :drag, :drop
					    )
		    :vertex-list (the (triangle-vertices (the poly-2-datum))))

	    (drop-coord-section :type 'sheet-section
				:inner-html (with-cl-who-string ()
					      ((:table  :border 1)
						  (:tr (:td "dropped coord:")
						       (:td (str (the viewport dropped-x-y))))
						(:tr (:td "dropped dimensions:")
						       (:td (str (the viewport dropped-height-width))))
						(:tr (:td "dropped object:")
						       (:td (fmt "~s" (the viewport dropped-object)))))))
            
	    (viewport :type 'base-ajax-graphics-sheet
		      :respondent self

		      :on-drop-function 
		      #'(lambda()
			  (format t "Just called the on-drop hook with ~s, ~s, and ~s.~%"
				  (the viewport dropped-x-y)
				  (the viewport dropped-height-width)
				  (the viewport dropped-object)))

		      #+nil
		      :on-drag-function 
		      #+nil
		      #'(lambda()
			  (format t "Just called the on-move hook with ~s, ~s, and ~s.~%"
				  (the main-area dropped-x-y)
				  (the main-area dropped-height-width)
				  (the main-area dropped-object)))

		      :vector-graphics-onclick? nil
		      :length 500 :width 500
		      :view-direction-default :top
		      :display-list-objects (list (the poly-0) (the poly-1-up) (the poly-1-down) 
						  (the poly-1) (the poly-2)))))



(define-object tri-cyl (cylinder)
  :input-slots (vertex-list top-vector)

  :computed-slots 
  ((center (first (the vertex-list)))
   (orientation (alignment :rear (the top-vector)))
   (length (3d-distance (first (the vertex-list)) (second (the vertex-list))))
   (radius (half (3d-distance (first (the vertex-list)) (third (the vertex-list)))))))

            
(define-object rounded-corner-triangle (global-filleted-polyline)
  :computed-slots ((default-radius 0.1)))


