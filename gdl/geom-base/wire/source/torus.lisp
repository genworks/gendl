;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
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

(define-object torus (ifs-output-mixin arcoid-mixin base-object)
  
  :documentation (:description "A single-holed ``ring'' torus, also known as an ``anchor ring.''
This is basically a circular cylinder ``bent'' into a donut shape. Partial donuts (``elbows'') are supported.
Partial ``bent'' cylinders are not currently supported."
                  
                  :examples "<pre>
  
  (in-package :gdl-user)
  
  (define-object torus-sample (torus)
    :computed-slots
    ((major-radius 150)
     (minor-radius 42)
     (draw-centerline-arc? t)
     (number-of-longitudinal-sections 10)
     (number-of-transverse-sections 10)
     (display-controls (list :color :green-forest-medium)))

    :hidden-objects ((view :type 'base-view
                           :projection-vector (getf *standard-views* :trimetric)
                           :page-width (* 5 72) :page-length (* 5 72)
                           :objects (list self))))
  

  (generate-sample-drawing :objects (make-object 'torus-sample) 
                           :projection-direction :trimetric)
                  
</pre>")                  

  
  :input-slots
  ("Number. Distance from center of donut hole to centerline of the torus."
   major-radius 
   
   "Number. Radius of the bent cylinder making up the torus."
   minor-radius
   
   ("Angle in Radians. Indicates the end angle for the donut. Defaults to twice pi for a full-circle donut."
    arc 2pi :defaulting)
   
   ("Number. Radius of the inner hollow part of the bent cylinder for a hollow torus. Defaults to NIL for a solid cylinder"
    inner-minor-radius nil :defaulting)
   
   ("Boolean. Indicates whether the bent cylinder's centerline arc should be rendered in some renderings."
    draw-centerline-arc? nil)
   
   ("Integer. Indicates the number of arcs to be drawn on along ``surface'' of the torus in some wireframe renderings."
    number-of-longitudinal-sections 4)
   
   ("Integer. Indicates the number of circular cross-sections of the bent cylinder to show in some wireframe renderings." 
    number-of-transverse-sections 3)
   
   ("Boolean. Indicates whether to include end caps for a partial torus in some renderings. Defaults to T."
    end-caps? (< (the major-radius) 2pi)))

  
  :computed-slots
  ((%renderer-info% (list :vrml? t :view-default :trimetric))
   
   (%curves-to-draw% (let* ((center (the :center))
                            (face-ht (the :%face-ht%))
                            (arc (the :end-angle-normalized))
                            (section-angle (the :section-angle))
                            (torus-arc-angle (the :torus-arc-angle))
                            (right-vector (gethash :right face-ht))
                            (front-vector (gethash :front face-ht))
                            (rear-vector  (gethash :rear face-ht))
                            (top-vector   (gethash :top face-ht))
                            (major-radius (the major-radius))
                            (minor-radius (the :minor-radius))
                            
                            (torus-arc-curves (let (result)
                                                (dotimes (index (the :number-of-longitudinal-sections) result)
                                                  (let* ((reference-point (translate center :right major-radius))
                                                         (sample-point (translate-along-vector 
									reference-point
									(rotate-vector right-vector 
										       (* index torus-arc-angle)
										       front-vector)
									minor-radius))
                                                        
                                                         (center (inter-line-plane sample-point right-vector
                                                                                   center right-vector))
                                                         (radius (3d-distance sample-point center)))
                                                   
                                                    (let ((new-curves (arc-curves face-ht radius center 0 arc)))
                                                      (if result (nconc result new-curves) (setq result new-curves)))))))
                           
                            (section-circle-curves (let (result)
                                                     (dotimes (index (1+ (the :number-of-transverse-sections)) result)
                                                       (let ((orientation (alignment :top (rotate-vector rear-vector 
                                                                                                         (* section-angle index)
                                                                                                         top-vector)))
                                                             (center (translate-along-vector center
                                                                                             (rotate-vector right-vector
                                                                                                            (* section-angle index)
                                                                                                            top-vector)
                                                                                             major-radius)))
                                                         (let ((new-curves (arc-curves (make-face-ht orientation)
                                                                                       minor-radius
                                                                                       center 0 2pi)))
                                                           (if result (nconc result new-curves) (setq result new-curves))))))))
                       
                       (append (when (the :draw-centerline-arc?) (the :centerline-arc :curves))
                               torus-arc-curves 
                               section-circle-curves
                               (when (the :inner-minor-radius) (the :inner-torus :%curves-to-draw%)))))
   
   (section-angle (/ (the end-angle-normalized) (the number-of-transverse-sections)))
   
   (torus-arc-angle (/ (twice pi) (the :number-of-longitudinal-sections)))
   
   
   (width (twice (+ (the major-radius) (the minor-radius))))
   (length (the width)) (height (twice (the minor-radius)))
   (equi-points (the centerline-arc (equi-spaced-points (1+ (the number-of-transverse-sections)))))
   
   (polygon-points (mapcar #'(lambda(circle)
			       (the-object circle (equi-spaced-points (max 12 (the number-of-longitudinal-sections)))))
			   (list-elements (the transverse-circles))))

   (polygons-for-ifs (apply #'append
			    (mapcar #'(lambda(circle-points-1 circle-points-2)
					(mapcar #'(lambda (p1 p2 p3 p4)
						    (list p1 p2 p4 p3))
						circle-points-1
						(rest circle-points-1)
						circle-points-2
						(rest circle-points-2)))
				    (the polygon-points) (rest (the polygon-points))))))
  
  :hidden-objects
  ((inner-torus :type (if (the :inner-minor-radius) 'torus 'null-part)
                :pass-down (:arc 
                            :number-of-transverse-sections
                            :number-of-longitudinal-sections)
                :minor-radius (the :inner-minor-radius)
                :major-radius (- (the :major-radius)
                                 (- (the :inner-minor-radius)
                                    (the :minor-radius))))

   (transverse-circles :type 'circle
		       :radius (the minor-radius)
		       :sequence (:size (length (the equi-points)))
		       :center (nth (the-child index) (the equi-points))
		       :orientation (alignment :top (the centerline-arc (tangent (the-child center)))))
		       

   (start-circle :type 'circle
                 :radius (the minor-radius))
   
   (centerline-arc :type 'arc
                   :start-angle 0
                   :end-angle (the :end-angle-normalized)
                   :radius (the :major-radius))))



