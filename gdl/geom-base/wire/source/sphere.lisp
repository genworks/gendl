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

(define-object sphere (ifs-output-mixin arcoid-mixin base-object)
  :documentation (:description "The set of points equidistant from a given center point."
                  
                  :examples "<pre>
  
  (in-package :gdl-user)
  
  (define-object sphere-sample (sphere)
    
   :computed-slots

    ((radius 150)
     (number-of-vertical-sections 10)
     (number-of-horizontal-sections 10)
     (display-controls (list :color :green-forest-medium))))

  (generate-sample-drawing :objects (make-object 'sphere-sample) 
                           :projection-direction :trimetric)





                  
</pre>")                  

  
  :input-slots ("Number. Distance from center to any point on the sphere." radius
                ("Angle in radians. Starting horizontal angle for a partial sphere. Default is 0."
                 start-horizontal-arc 0 :defaulting)
                ("Angle in radians. Starting vertical angle for a partial sphere. Default is -pi/2."
                 start-vertical-arc (- pi/2) :defaulting)
                ("Angle in radians. Ending horizontal angle for a partial sphere. Default is twice pi."
                 end-horizontal-arc 2pi :defaulting)
                ("Angle in radians. Ending vertical angle for a partial sphere. Default is pi/2."
                 end-vertical-arc pi/2 :defaulting)
                ("Number. Radius of inner hollow for a hollow sphere. Default is NIL, for a non-hollow sphere."
                 inner-radius nil :defaulting)
                ("Number. How many lines of latitude to show on the sphere in some renderings. Default value is 4."
                 number-of-horizontal-sections 8)
                ("Number. How many lines of longitude to show on the sphere in some renderings. Default value is 4."
                 number-of-vertical-sections 8)
                (inner? nil))
  
  :computed-slots
  ((length (twice (the radius)))
   (width (twice (the radius)))
   (height (twice (the radius)))

   
   (%renderer-info% (list :vrml? t :view-default :trimetric))
   
   (parallel-and-meridian-curves 
    (let* ((center (the :center))
           (radius (the :radius))
           (face-ht (the :%face-ht%))
           (right-vector (gethash :right face-ht))
           (front-vector (gethash :front face-ht))
           (top-vector (gethash :top face-ht))
           
           (vertical-arcs (the (normalize-start-end (the start-vertical-arc)
                                                    (the end-vertical-arc))))
           (start-vertical-arc (first vertical-arcs))
           (end-vertical-arc (second vertical-arcs))
           (horizontal-arcs (the (normalize-start-end (the start-horizontal-arc)
                                                      (the end-horizontal-arc))))
           (start-horizontal-arc (first horizontal-arcs))
           (end-horizontal-arc (second horizontal-arcs))
           
           (parallel-angle (the :parallel-angle))
           (parallel-curves (let (result)
                              (dotimes (index (1- (the :number-of-vertical-sections)) result)
                                (let* ((local-center 
                                        (let* ((ray (rotate-vector right-vector 
                                                                   (+ start-vertical-arc
                                                                      (* parallel-angle (1+ index)))
                                                                   front-vector))
                                               (sphere-point (inter-line-sphere center ray
                                                                                center radius ray)))
                                          (inter-line-plane center top-vector sphere-point top-vector)))
                                       (radius (3d-distance local-center
                                                            (inter-line-sphere local-center
                                                                               right-vector
                                                                               center
                                                                               radius
                                                                               right-vector))))
                                  (let ((new-curves (arc-curves face-ht radius local-center start-horizontal-arc
                                                                end-horizontal-arc)))
                                    (if result (nconc result new-curves) (setq result new-curves)))))))
                                             
           (meridian-curves (let (result
                                  (horizontal-start-to-end-angle (the :horizontal-start-to-end-angle))
                                  (number-of-horizontal-sections (the :number-of-horizontal-sections)))
                              (dotimes (index (1+ (the :number-of-horizontal-sections)) result)
                                (let ((face-ht 
                                       (make-face-ht 
                                        (alignment 
                                         :top 
                                         (rotate-vector front-vector
                                                        (+ start-horizontal-arc
                                                           (* index
                                                              (/ horizontal-start-to-end-angle
                                                                 number-of-horizontal-sections)))
                                                        top-vector)
                                         :rear (the (:face-normal-vector :top))))))
                                  (let ((new-curves (arc-curves face-ht radius center start-vertical-arc
                                                                end-vertical-arc)))
                                    (if result (nconc result new-curves) (setq result new-curves))))))))
      (list :parallels parallel-curves :meridian-curves meridian-curves)))


   (%curves-to-draw%
    (append 
     (when (the :inner-radius) (the :inner-sphere :%curves-to-draw%))
     (getf (the parallel-and-meridian-curves) :meridian-curves)
     (getf (the parallel-and-meridian-curves) :parallels)))

               
   (horizontal-start-to-end-angle (- (the :end-horizontal-arc)(the :start-horizontal-arc)))
               
   (vertical-start-to-end-angle (- (the :end-vertical-arc) (the :start-vertical-arc)))
               
   (parallel-angle (/ (the vertical-start-to-end-angle) (the number-of-vertical-sections)))

   (horizontal-increment-angle (/ (the horizontal-start-to-end-angle) (the number-of-horizontal-sections)))

   (simple? (and (not (the inner-radius))
		 (near-to? (the start-horizontal-arc) 0)
		 (near-to? (the end-horizontal-arc) 2pi)
		 (near-to? (the start-vertical-arc) (- pi/2))
		 (near-to? (the end-vertical-arc) pi/2)))

   
   (polygon-points (let ((right-vector (the (face-normal-vector :right)))
			 result)
		     (dotimes (m (1+ (the number-of-horizontal-sections)) result)
		       (let* ((tangent-vector (rotate-vector right-vector
							     (+ (the start-horizontal-arc)
								(* m (the horizontal-increment-angle)))
							     (the (face-normal-vector :top))))
			      (normal-vector (cross-vectors  tangent-vector (the (face-normal-vector :top))))
			      inner-result)
			 (dotimes (n (1+ (the number-of-vertical-sections)))
			   (let ((ray (rotate-vector tangent-vector
						     (+ (the start-vertical-arc)
							(* n (the parallel-angle)))
						     normal-vector)))
			     (push (inter-line-sphere (the center) ray
						      (the center) (the radius) ray) inner-result)))
			 (push inner-result result)))))
								 

   (polygons-for-ifs (unless (the simple?)
		       (append
			(apply #'append
			       (mapcar #'(lambda(list1 list2)
					   (mapcar #'(lambda(p1 p2 p3 p4)
						       (list p1 p2 p4 p3))
						   list1 (rest list1) list2 (rest list2)))
				       (the polygon-points) (rest (the polygon-points))))
			
			(when (and (the inner-radius) (not (the inner?)))
			  (mapcar #'(lambda (p1 p2 p3 p4)
				      (list p1 p2 p4 p3))
				  (first (the polygon-points))
				  (first (the inner-sphere polygon-points))
				  (rest (first (the polygon-points)))
				  (rest (first (the inner-sphere polygon-points)))))


			(when (and (the inner-radius) (not (the inner?)))
			  (mapcar #'(lambda (p1 p2 p3 p4)
				      (list p1 p2 p4 p3))
				  (lastcar (the polygon-points))
				  (lastcar (the inner-sphere polygon-points))
				  (rest (lastcar (the polygon-points)))
				  (rest (lastcar (the inner-sphere polygon-points)))))

			(when (and (the inner-radius) (not (the inner?)))
			  (mapcar #'(lambda (p1 p2 p3 p4)
				      (list p1 p2 p4 p3))
				  (mapcar #'first (the polygon-points))
				  (mapcar #'first (the inner-sphere polygon-points))
				  (rest (mapcar #'first (the polygon-points)))
				  (rest (mapcar #'first (the inner-sphere polygon-points)))))

			(when (and (the inner-radius) (not (the inner?)))
			  (mapcar #'(lambda (p1 p2 p3 p4)
				      (list p1 p2 p4 p3))
				  (mapcar #'lastcar (the polygon-points))
				  (mapcar #'lastcar (the inner-sphere polygon-points))
				  (rest (mapcar #'lastcar (the polygon-points)))
				  (rest (mapcar #'lastcar (the inner-sphere polygon-points)))))

			(when (the inner-radius) (the inner-sphere polygons-for-ifs))))))

  
  :hidden-objects
  ((inner-sphere :type (if (the :inner-radius) 'sphere 'null-part)
                 :radius (the :inner-radius)
                 :pass-down (start-horizontal-arc 
                             start-vertical-arc end-horizontal-arc
                             end-vertical-arc
                             number-of-horizontal-sections 
                             number-of-vertical-sections)
                 :inner? t
                 :inner-radius nil)))






