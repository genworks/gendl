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

(in-package :gwl-user)

#+nil
(define-object assy (application-mixin)
  
  :computed-slots
  ((width (read-from-string (or (getf (the :query-plist) :width) "10")))
   (length (read-from-string (or (getf (the :query-plist) :length) "10")))
   (height (read-from-string (or (getf (the :query-plist) :height) "10")))
   
   (ui-display-list-objects (list (the box) 
                                  (the cone) 
                                  (the cylinder) 
                                  (the torus)
                                  (the sphere)
                                  ;;(the spherical-cap)
                                  (the box-2)
                                  (the box-3)
                                  (the arc)
                                  (the route-pipe)
                                  (the polyline-projection)
                                  (the polyline)
                                  )))
  
  :objects
  ((box :type 'box)
   
   (cone :type 'cone :radius-1 1.5 :radius-2 2 :arc (half pi) :inner-radius-1 1 :inner-radius-2 1.5)

   
   
   (cone-right :type 'cone :radius-1 0 :radius-2 5 :display-controls (list :color :green))
   
   (cone-left :type 'cone :radius-1 0 :radius-2 5 :display-controls (list :color :red)
              :center (translate (the center) :front 10)
              :orientation (alignment :left (the (face-normal-vector :left))
                                      :top (the (face-normal-vector :top))
                                      :rear (the (face-normal-vector :front))))
   
   (cylinder :type 'cylinder :radius 1 :center (translate (the :center) :down 3)
             :inner-radius 0.5)

   (torus :type 'torus 
          :center (translate (the :center) :up 5)
          :arc pi
          :major-radius (the :length) :minor-radius (* (the :length) 0.30)
          :inner-minor-radius (* (the :length) 0.40)
          :draw-centerline-arc? t
          :number-of-transverse-sections 10
          :number-of-longitudinal-sections 7)
   
   
   (sphere :type 'sphere
           :center (the :center)
           :radius (/ (the :box :length) 3)
           :number-of-horizontal-sections 21 
           :number-of-vertical-sections 10)
   
   
   ;;
   ;; FLAG -- this spherical cap is bugged.
   ;;
   #+nil(spherical-cap :type 'spherical-cap
                       :center (the :center)
                   
                       :axis-length (twice (the-child :base-radius))
                       :base-radius (/ (the :box :length) 3)
                   
                       ;;:cap-thickness (* (the-child :axis-length) 0.05)
                   
                       :number-of-horizontal-sections 21 
                       :number-of-vertical-sections 10)

   (box-2 :type 'box
          :display-controls (list :color :red)
          :length 2 :width 4 :height 8
          :orientation (alignment :top (the (:face-normal-vector :right))
                                  :right (rotate-vector-d (the (:face-normal-vector :rear))
                                                          45
                                                          (the (:face-normal-vector :right))))
           
          :center (the :center))
           
   
   (box-3 :type 'box
          :length 2 :width 2 :height 2
          :center (translate (the (:edge-center :rear :left)) :down 3))

           
   (arc :type 'arc :radius 3 :center (the (:face-center :rear))
        :orientation (alignment :top (the (:face-normal-vector :rear))))
   
   (route-pipe :type 'route-pipe
               :display-controls (list :color :blue)
               ;;:default-bend-radius 0.5
               :radius-list (list 0.5 1 2)
               :outer-pipe-radius .3
               :inner-pipe-radius .2
               :vertex-list (list (make-point -5 -5 -5)
                                  (make-point 0 0 0)
                                  (make-point 5 -5 -5)
                                  (make-point 5 5 5)))

   
   (polyline-projection :type 'global-filleted-polygon-projection
                        :display-controls (list :color :green)
                        :default-radius 0.5
                        :vertex-list (list (the  (:face-center :left))
                                           (the  (:face-center :right))
                                           (the  (:face-center :rear))
                                           (the  (:face-center :front))
                                           (the  (:face-center :left)))
              
                        :projection-vector (the (:face-normal-vector :top))
                        :projection-depth 3)

   
   (polyline :type 'global-filleted-polyline
             :default-radius 0.5
             :vertex-list (list (the  (:face-center :left))
                                (the  (:face-center :right))
                                (the  (:face-center :rear))
                                (the  (:face-center :front))
                                ))))

#+nil
(publish :path "/gwl/test-geom"
         :function #'(lambda(req ent)
                       (gwl-make-part req ent "gwl-user::assy")))
