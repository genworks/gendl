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


(define-object cylinder (ifs-output-mixin arcoid-mixin base-object)
  
  :documentation (:description "An extrusion of circular cross section in which the 
centers of the circles all lie on a single line (i.e., a right circular cylinder).
Partial cylinders and hollow cylinders are supported."
                  
                  :examples "<pre>

 (in-package :gdl-user)

 (define-object cylinder-sample (cylinder)
   :computed-slots
   ((display-controls (list :color :pink-spicy))
    (length 10)
    (radius 3)
    (number-of-sections 25)))


 (generate-sample-drawing :objects (make-object 'cylinder-sample)
                          :projection-direction (getf *standard-views* :trimetric))
   

</pre>")

  
  :input-slots ("Number. Distance from center of start cap to center of end cap."
                length 
                
                "Number. Radius of the circular cross section of the cylinder."
                radius
                
                
                ("Number. Radius of the hollow inner portion for a hollow cylinder."
                 inner-radius nil)
                
                ("Integer. Number of vertical sections to be drawn in wireframe rendering mode."
                 number-of-sections 8)
                
                ("Boolean. Determines whether to include bottom cap in shaded renderings. Defaults to T."
                 bottom-cap? t)
                
                ("Boolean. Determines whether to include bottom cap in shaded renderings. Defaults to T."
                 top-cap? t)
                
                (radius-1 (the :radius)) 
                (radius-2 (the :radius))
		
		("Boolean. Indicates that a partial cylinder (or cone) should have a closed gap."
		 closed? nil)

                (inner-radius-1 (the :inner-radius))
                (inner-radius-2 (the :inner-radius))
                (inner? nil)
                
                (width (twice (the radius)))
                (height (the width)))

  :computed-slots
  ((%renderer-info% (list :vrml? t :view-default :trimetric))
   
   (start-line (let* ((center (the :center))
                      (radius-1 (the :radius-1))
                      (radius-2 (the :radius-2))
                      (half-length (half (the :length)))
                      (start-arc-center (translate center :front half-length))
                      (end-arc-center (translate center :rear half-length))
                      (start-3-o-clock (translate start-arc-center :right radius-1))
                      (end-3-o-clock (translate end-arc-center :right radius-2)))
                 (list start-3-o-clock end-3-o-clock)))

   
   
   (%lines-and-curves% 
    (let* ((center (the :center))
           (radius-1 (the :radius-1))
           (radius-2 (the :radius-2))
           (half-length (half (the :length)))
           (start-arc-center (translate center :front half-length))
           (end-arc-center (translate center :rear half-length))
           (start-3-o-clock (translate start-arc-center :right radius-1))
           (end-3-o-clock (translate end-arc-center :right radius-2))
           (front-vector (the (:face-normal-vector :front)))
           (section-arc (/ (min (the :arc)(twice pi)) (the :number-of-sections)))
           (arc (the arc))
           (face-ht (the :%face-ht%))
           result)
      (list 
       (append (dotimes (count (the :number-of-sections) (nreverse result))
                 (push (list (rotate-point start-3-o-clock 
                                           start-arc-center
                                           front-vector
                                           :angle (* count section-arc))
                             (rotate-point end-3-o-clock
                                           end-arc-center
                                           front-vector
                                           :angle (* count section-arc))) result))
               (when (< (the :arc) (twice pi)) 
                 (append
                  (unless (the :inner?)
                    (cons  (the :end-line) (the :radials)))
                  (unless (or (the :hollow?) (the :inner?))
                    (list (the :center-line)))))
               (when (not (typep (the :inner-cylinder) 'null-part))
                 (the :inner-cylinder :%lines-to-draw%)))
       (let ((end-arc-face-ht (make-hash-table :size 6)))
         (setf (gethash :top end-arc-face-ht) (gethash :front face-ht)
               (gethash :rear end-arc-face-ht) (gethash :top face-ht)
               (gethash :right end-arc-face-ht) (gethash :right face-ht)
               (gethash :left end-arc-face-ht) (gethash :left face-ht)
               (gethash :bottom end-arc-face-ht) (gethash :rear face-ht)
               (gethash :front end-arc-face-ht) (gethash :bottom face-ht))
         (let ((end-arc-0-curves 
                (arc-curves end-arc-face-ht radius-1 (translate-along-vector center (gethash :front face-ht) 
                                                                             half-length)
                            0 arc))
               (end-arc-1-curves 
                (arc-curves end-arc-face-ht radius-2 (translate-along-vector center (gethash :rear face-ht) 
                                                                             half-length)
                            0 arc)))
           (append end-arc-0-curves end-arc-1-curves
                   (when (not (typep (the :inner-cylinder) 'null-part))
                     (the :inner-cylinder :%curves-to-draw%))))))))
   
   (%curves-to-draw% (second (the :%lines-and-curves%)))
   
   (%lines-to-draw% (first (the :%lines-and-curves%)))
   
   ("3D Point. The center of the start cap."
    start (translate (the :center) :front (half (the :length))))
   
   ("3D Point. The center of the end cap."
    end (translate (the :center) :rear (half (the :length))))
   
   ("3D Vector. Points from the start to the end."
    direction-vector (subtract-vectors (the :end) (the :start)))
   
   ("Boolean. Indicates whether there is an inner-radius and thus the cylinder is hollow."
    hollow? (or (the :inner-radius) (and (the :inner-radius-1) (the :inner-radius-2))))

   ;;
   ;; FLAG -- pull (the (:end-arcs [0,1]) [:start, :end]) from %curves-to-draw% above computed from function.
   ;;
   (end-line (list (the (:end-arcs 0) :end) (the (:end-arcs 1) :end)))
   (%arcs% (append (list-elements (the end-arcs))
                   (when (not (typep (the inner-cylinder) 'null-part))
                     (list-elements (the inner-cylinder end-arcs)))))
   
   (radials (let ((hollow? (the :hollow?)))
              (list (list (if hollow? (the :inner-cylinder (:end-arcs 0) :start)
                            (the :start)) (the (:end-arcs 0) :start))
                    (list (if hollow? (the :inner-cylinder (:end-arcs 0) :end)
                            (the :start)) (the (:end-arcs 0) :end))
                    (list (if hollow? (the :inner-cylinder (:end-arcs 1) :start)
                            (the :end)) (the (:end-arcs 1) :start))
                    (list (if hollow? (the :inner-cylinder (:end-arcs 1) :end)
                            (the :end)) (the (:end-arcs 1) :end)))))
   
   (center-line (list (the (:end-arcs 0) :center) (the (:end-arcs 1) :center)))

   (top-points (the (end-arcs 1) (equi-spaced-points (max (the number-of-sections) 12))))
   (bottom-points (the (end-arcs 0) (equi-spaced-points (max (the number-of-sections) 12))))

   (polygons-for-ifs (unless (the simple?)
		       ;;
		       ;; FLAG -- For cones at least: fill in with more intermediate arcs, to get better aspect triangles. 
		       ;;
		       (let ((top-points (the top-points))
			     (bottom-points (the bottom-points)))
			 (append (mapcar #'(lambda(p1 p2 p3 p4) (list p1 p2 p4 p3))
					 top-points bottom-points (rest top-points) (rest bottom-points))
				 (when (and (the top-cap?) (not (or (the hollow?) (the inner?)))) (list top-points))
				 (when (and (the bottom-cap?) (not (or (the hollow?) (the inner?)))) (list bottom-points))
				 (when (and (the closed?) (not (or (the hollow?) (the inner?))))
				   (list (list (first top-points)
					       (first bottom-points)
					       (lastcar bottom-points)
					       (lastcar top-points))))
				 
				 (when (and (the hollow?) (not (the inner?)))
				   (mapcar #'list
					   top-points
					   (rest top-points)
					   (rest (the inner-cylinder top-points))
					   (the inner-cylinder top-points)))

				 (when (and (the hollow?) (not (the inner?)))
				   (mapcar #'list
					   bottom-points
					   (rest bottom-points)
					   (rest (the inner-cylinder bottom-points))
					   (the inner-cylinder bottom-points)))

				 (when (and (the hollow?) (not (the inner?)))
				   (list (list (first top-points)
					       (first (the inner-cylinder top-points))
					       (first (the inner-cylinder bottom-points))
					       (first bottom-points))
					 (list (lastcar top-points)
					       (lastcar (the inner-cylinder top-points))
					       (lastcar (the inner-cylinder bottom-points))
					       (lastcar bottom-points))))

				 (when (the hollow?) (the inner-cylinder polygons-for-ifs))))))


   (simple? (and (not (the inner-radius))
		 (near-to? (the arc) 2pi)))


   )

   
  :hidden-objects
  ((inner-cylinder :type (cond ((and (the :inner-radius-1) (the :inner-radius-2)) 'cone)
                               ((the inner-radius) 'cylinder)
                               (t 'null-part))
                   :radius (the :inner-radius)
                   :radius-1 (the :inner-radius-1)
                   :radius-2 (the :inner-radius-2)
                   :inner? t
                   :pass-down (:number-of-sections :arc))
   ;;
   ;; FLAG -- remove this child part when FLAG above is addressed.
   ;;
   (end-arcs :type 'arc
             :sequence (:size 2)
             :radius (ecase (the-child :index) (0 (the :radius-1)) (1 (the :radius-2)))
             :orientation (alignment :top (the (:face-normal-vector :front))
                                     :right (the (:face-normal-vector :right)))
             :center (translate (the :center) (ecase (the-child :index)
                                                (0 :front) (1 :rear)) (half (the :length)))
             :start-angle 0
             :end-angle (the :arc))))









