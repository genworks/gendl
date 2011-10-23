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

(define-object spherical-cap (arcoid-mixin base-object)
  
  :documentation (:description "The region of a sphere which lies above (or below) a given plane. Although this
could be created with a partial sphere using the sphere primitive, the spherical cap allows for more convenient
construction and positioning since the actual center of the spherical cap is the center of its reference box."
                  
                  :examples "<pre>
  
 (in-package :gdl-user)

 (define-object spherical-cap-sample (spherical-cap)
   
  :computed-slots

   ((base-radius 150)
    (cap-thickness 7)
    (axis-length (* (the base-radius) +phi+))
    (number-of-vertical-sections 10)
    (number-of-horizontal-sections 10)
    (display-controls (list :color :orchid-medium :transparency 0.5))))
  

 (generate-sample-drawing :objects (make-object 'spherical-cap-sample) 
                          :projection-direction :trimetric)
                  
</pre>")


  :input-slots ("Number. The distance from the center of the base to the center of the dome."
                axis-length 
                
                "Number. Radius of the base."
                base-radius
  
                ("Number. Thickness of the shell for a hollow spherical-cap. Specify this 
                  or inner-base-radius, not both."
                 cap-thickness nil :defaulting)
                
                ("Number. Radius of base of inner for a hollow spherical-cap. Specify this 
                  or cap-thickness, not both."
                 inner-base-radius nil :defaulting)
  
                ("Integer. How many lines of latitude to show on the spherical-cap in some renderings. Default value is 2."
                 number-of-horizontal-sections 4)
                
                ("Integer. How many lines of longitude to show on the spherical-cap in some renderings. Default value is 2."
                 number-of-vertical-sections 4))

  
  :computed-slots
  ((%renderer-info% (list :vrml? t :view-default :trimetric))
   (length (twice (the sphere-radius)))
   (width (the length))
   (height (the axis-length))
   
   ("Number. Radius of the sphere containing the spherical-cap."
    sphere-radius (+ (/ (^2 (the :base-radius)) (twice (the :axis-length)))
                     (half (the :axis-length))))
   
   ("3D Point. Center of the sphere containing the spherical-cap."
    sphere-center (translate (the (:face-center :top)) :down (the sphere-radius)))
   
   (base-center (translate (the center) :down (half (the axis-length))))
   
   
   (cap-thickness-c (or (the :cap-thickness)
                        (when (the :inner-base-radius)
                          (- (the :sphere-radius)
                             (sqrt (+ (^2 (the :inner-base-radius))
                                      (^2 (- (the :sphere-radius) (the :axis-length)))))))))
   
   (inner-base-radius-c (or (the :inner-base-radius)
                            (when (the :cap-thickness)
                              (sqrt (- (^2 (- (the :sphere-radius) (the :cap-thickness)))
                                       (^2 (- (the :sphere-radius) (the :axis-length))))))))
   
   (%lines-to-draw% (the :radials))
   
   (%curves-to-draw% (append
                      (when (or (the :cap-thickness) (the :inner-base-radius))
                        (the :inner-cap :%curves-to-draw%))
                      (apply #'append (list-elements (the :meridians) (the-element :curves)))
                      (apply #'append (list-elements (the :parallels) (the-element :curves)))))
                       
   (start-angle (angle-between-vectors (subtract-vectors (translate (the :base-center) :right
                                                                    (the :base-radius))
                                                         (the :sphere-center))
                                       (subtract-vectors (translate (the :sphere-center) :right 
                                                                    (the :sphere-radius)) 
                                                         (the :sphere-center))
                                       (the (:face-normal-vector :rear)) :-ve t))
   
   (parallel-angle (/ (- (the :end-angle-normalized) (the :start-angle-normalized)) 
                      (the :number-of-vertical-sections)))
   
   (end-angle pi/2)
   
   (meridian-angle (/ (twice pi) (the :number-of-horizontal-sections)))
   
   (radials (let (result)
              (dotimes (count (the :meridians :number-of-elements) (nreverse result))
                (push (list (if (typep (the :inner-cap) 'spherical-cap)
                                (the :inner-cap (:meridians count) :start)
                              (the :base-center))
                            (the (:meridians count) :start)) result)))))
  
  :hidden-objects
  ((inner-cap :type (if (and (the :inner-base-radius-c)
                             (the :cap-thickness-c)) 'spherical-cap 'null-part)
              :base-radius (the :inner-base-radius-c)
              :center (translate (the center) :down (half (the cap-thickness-c)))
              :pass-down (:number-of-horizontal-sections :number-of-vertical-sections)
              :axis-length (- (the :axis-length) (the :cap-thickness-c)))
   
   (meridians :type 'arc
              :sequence (:size (the :number-of-horizontal-sections))
              :pass-down (:start-angle :end-angle)
              :center (the :sphere-center)
              :radius (the :sphere-radius)
              :orientation (alignment :top (rotate-vector (the (:face-normal-vector :rear))
                                                          (* (the :meridian-angle)
                                                             (the-child :index))
                                                          (the (:face-normal-vector :top)))
                                      
                                      :rear (the (:face-normal-vector :top))
                                      ))
   
   (parallels :type 'arc
              :sequence (:size (1+ (the :number-of-vertical-sections)))
              
              :center (let* ((ray (rotate-vector (the (:face-normal-vector :right))
                                                 (+ (the :start-angle-normalized)
                                                    (* (the :parallel-angle) (the-child :index)))
                                                 (the (:face-normal-vector :front))))
                             (sphere-point (inter-line-sphere (the :sphere-center) ray
                                                              (the :sphere-center) 
                                                              (the :sphere-radius) ray)))
                        (inter-line-plane (the :sphere-center) (the (:face-normal-vector :top))
                                          sphere-point (the (:face-normal-vector :top))))
              :radius (let ((center (the-child center))
                            (point (inter-line-sphere (the-child :center)
                                                      (the (:face-normal-vector :right))
                                                      (the :sphere-center)
                                                      (the :sphere-radius)
                                                      (the (:face-normal-vector :right)))))
                        (if (and center point)
                            (3d-distance center point)
                          0)))))



