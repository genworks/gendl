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

(in-package :geom-base)


(define-object leader-line (base-object)
  
  :documentation (:description "Creates a leader line with arrows on zero, one, or both ends")
  
  :input-slots
  ("List of 3D Points. Leader-line is rendered as a polyline going through these points."
   path-points
   
   
   
   ("List of two points or nil. 
The start and end of the break in the leader line to accomodate the dimension-text, 
in cases where there is overlap."
    break-points  nil)

   
   ("Number. The width of the arrows. Defaults to (* (the line-thickness) 5)."
    arrowhead-width (* (the line-thickness) 5))
   
   ("Number. The length of the arrows. Defaults to (* (the arrowhead-width) 2)"
    arrowhead-length (* (the arrowhead-width) 2))
   
   ("Keyword. Controls the style of first arrowhead. Currently only :wedge is supported. Default is :wedge."
    arrowhead-style :wedge)
   
   ("Keyword. Controls the style and presence of second arrowhead. 
Currently only :wedge is supported. Default is :none."
    arrowhead-style-2 :none))
  
  
  :computed-slots
  ((leader-vector (subtract-vectors (lastcar (the path-points)) (first (the path-points))))
   (arrowhead-vector (subtract-vectors (second (the path-points)) 
                                       (first (the path-points))))
   (leader-length (3d-distance (first (the path-points)) (lastcar (the path-points))))
   

   (path-info (append (the arrowhead path-info)
		      (the polyline path-info)
		      (apply #'append (mapsend (the polylines) :path-info))
		      (the arrowhead-2 path-info)))

   (display-controls (merge-display-controls (list :fill-color :black)))

		    
   
   (%lines-to-draw% (remove nil
			    (append (the polyline %lines-to-draw%)
				    (apply #'append (mapsend (the polylines) :%lines-to-draw%))))))

  
  :hidden-objects
  ((polyline :type (if (null (the break-points)) 'global-polyline 'null-part)
             :vertex-list (the path-points))
   
   
   (polylines :type (if (the break-points) 'global-polyline 'null-part)
              :sequence (:size 2)
              :vertex-list (ecase (the-child index)
                             (0  (list (first (the path-points)) (first (the break-points))))
                             
                             (1  (list (second (the break-points)) (lastcar (the path-points))))
                             
                             ))

   
   (arrowhead :type (if (not (eql (the arrowhead-style) :none)) 'arrowhead 'null-part)
	      :center (first (the path-points))
	      :length (the arrowhead-length)
	      :width (the arrowhead-width)
	      :style (the arrowhead-style)
	      :orientation (alignment :top (if (parallel-vectors? 
						(the (face-normal-vector :top)) (the arrowhead-vector))
					       (the (face-normal-vector :rear))
					       (the (face-normal-vector :top)))
				      :rear (reverse-vector (the arrowhead-vector))))
       
    (arrowhead-2 :type (if (not (eql (the arrowhead-style-2) :none))
                           'arrowhead 'null-part)
                 :center (lastcar (the path-points))
                 :length (the arrowhead-length)
                 :width (the arrowhead-width)
                 :style (the arrowhead-style-2)
                 :orientation (alignment :top (if (parallel-vectors? 
                                                   (the (face-normal-vector :top)) (the arrowhead-vector))
                                                  (the (face-normal-vector :rear))
                                                (the (face-normal-vector :top)))
                                         :rear (the arrowhead-vector)))))


(define-object arrowhead (base-object)
  
  :input-slots
  ((style :wedge)
   (length (* .2 72))
   (width (* .1 72))
   (wedge-ratio 1/3))
   
  
  :computed-slots
  ((height 0)
   (arrow-center (translate (the center)
                            :front (* (the length) (- 1 (the wedge-ratio)))))
   ;;
   ;; FLAG -- remove this when we have more general 2d bounding box machinery
   ;;
   (%lines-to-draw% (append
                     (destructuring-bind (p0 p1 p2 p3)
                         (the outline-points)
                       (list (list p0 p1)
                             (list p1 p2)
                             (list p2 p3)
                             (list p3 p0)))
                     (when (the second-outline-points)
                       (destructuring-bind (p0 p1 p2 p3)
                         (the second-outline-points)
                       (list (list p0 p1)
                             (list p1 p2)
                             (list p2 p3)
                             (list p3 p0))))))

   (path-info (append (list :move (first (the outline-points)))
		      (mapcan #'(lambda(point) (list :line point))
			      (rest (the outline-points)))
		      (list :line (first (the outline-points)))))
    
   (outline-points (ecase (the style)
                     ((:wedge :double-wedge :triangle)
                      (when (eql (the style) :triangle)
                        (warn "*** ~%~%Triangle not yet supported~%~%"))
                      (list (the center)
                            (translate (the (vertex :front :right :top))
                                       :front (half (the length)))
                            (translate (the center) :front
                                       (* (the length) (- 1 (the wedge-ratio))))
                            (translate (the (vertex :front :left :top))
                                       :front (half (the length)))))))
   
   
   (second-outline-points (when (member (the style) (list :double-wedge))
                            (mapcar #'(lambda(point)
                                        (translate point :front (* (the length) 
								   (- 1 (the wedge-ratio)))))
                                    (the outline-points))))))


;;
;; FLAG -- update for auto-scaling outside base-view
;;
(define-lens (2d-output leader-line)()
  :output-functions
  ((cad-output
    ()
    (unless (typep (the polyline) 'null-part) (write-the polyline (cad-output)))
    (mapc #'(lambda(polyline) 
              (unless (typep polyline 'null-part)
                (write-the-object polyline cad-output)))
          (list-elements (the polylines)))
    (write-the arrowhead (cad-output))
    (write-the arrowhead-2 (cad-output))
    )))
