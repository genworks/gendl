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



(in-package :surf) 

(define-object basic-surface (surface)  
  
  :documentation (:description "This routine constructs a 3D surface by interpolating
   a set of four boundary 3D curves."
                  :examples "
    <pre>
 
 (in-package :gdl-user)   

 (define-object test-basic-surface (base-object)
  
  :computed-slots
  ((control-points-u-min (list (make-point 0 0 0 )
                               (make-point 1 0 0 )
                               (make-point 2 0 1 )
                               (make-point 3 0 0 )
                               (make-point 4 0 0 )))
   
   (control-points-u-max (list (make-point 0 4 0 )
                               (make-point 1 4 0 )
                               (make-point 2 4 1 )
                               (make-point 3 4 0 )
                               (make-point 4 4 0 )))
   
   (control-points-v-min (list (make-point 0 0 0 )
                               (make-point 0 1 0 )
                               (make-point 0 2 1 )
                               (make-point 0 3 0 )
                               (make-point 0 4 0 )))
                              
   (control-points-v-max (list (make-point 4 0 0 )
                               (make-point 4 1 0 )
                               (make-point 4 2 1 )
                               (make-point 4 3 0 )
                               (make-point 4 4 0 ))))
  
  :objects
  ((surf-curve-u-min :type 'fitted-curve
                     :display-controls (list :color :green :line-thickness 2)
                     :points (the control-points-u-min))
   
   (surf-curve-u-max :type 'fitted-curve
                     :display-controls (list :color :green :line-thickness 2)
                     :points (the control-points-u-max))
     
   (surf-curve-v-min :type 'fitted-curve
                     :display-controls (list :color :blue :line-thickness 2)
                     :points (the control-points-v-min))
   
   (surf-curve-v-max :type 'fitted-curve
                     :display-controls (list :color :blue :line-thickness 2)
                     :points (the control-points-v-max))
      
   (surface :type 'basic-surface
            :display-controls (list :color :red :line-thickness 0.5)
            :curve-bottom (the surf-curve-u-min)
            :curve-top (the surf-curve-u-max)
            :curve-left (the surf-curve-v-min)
            :curve-right (the surf-curve-v-max))

   (arc-1 :type 'arc-curve
          :display-controls (list :color :green :line-thickness 2)
          :orientation (alignment :top (the (face-normal-vector :rear)))
          :center (make-point 1 0 0)
          :radius 1
          :start-angle 0
          :end-angle pi)
 
   (arc-2 :type 'arc-curve
          :display-controls (list :color :green :line-thickness 2)
          :orientation (alignment :top (the (face-normal-vector :rear)))
          :center (make-point 1 -2 0)
          :radius 1
          :start-angle 0
          :end-angle pi)
   
   (arc-3 :type 'arc-curve
          :orientation (alignment :rear (the (face-normal-vector :right)))
          :display-controls (list :color :blue :line-thickness 2)
          :center (make-point 0 -1 0)
          :radius 1
          :start-angle 0
          :end-angle pi)
   
   (arc-4 :type 'arc-curve
          :orientation (alignment :rear (the (face-normal-vector :left)))
          :display-controls (list :color :blue :line-thickness 2)
          :center (make-point 2 -1 0)
          :radius 1
          :start-angle 0
          :end-angle pi)
   
  (surf-arc :type 'basic-surface
            :display-controls (list :color :red :line-thickness 0.5) 
            :curve-bottom (the arc-1 )
            :curve-top (the arc-2 )
            :curve-left (the arc-3 reverse )
            :curve-right (the arc-4))))



 (generate-sample-drawing :objects (the-object (make-object 'test-basic-surface) surface)
                          :projection-direction (getf *standard-views* :tri-r-r))


</pre>")

  :input-slots 
  
  (test
   (with-suport-curve? t)
   "GDL curve object. The curve corresponding to the surface bottom boundary ." curve-bottom
   "GDL curve object. The curve corresponding to the surface top boundary." curve-top
   "GDL curve object. The curve corresponding to the surface left boundary." curve-left
   "GDL curve object. The curve corresponding to the surface right boundary." curve-right)
  
  :computed-slots 
  ((native-surface (the extended-v-max native-surface)))
  
 :hidden-objects
  ((suport-curve-u :type 'linear-curve
                   :start (the curve-bottom (point 0.5))
                   :end (the curve-top (point 0.5)))
   
   (suport-curve-v :type 'linear-curve
                   :start (the curve-left (point 0.5))
                   :end (the curve-right (point 0.5)))
   
   (suport-curve :type 'linear-curve 
                 :start (translate-along-vector 
                         (the curve-left (point 0.5))
                         (subtract-vectors (the suport-curve-u (point 0.5))
                                           (the suport-curve-v (point 0.5)))
                         (/(3d-distance (the suport-curve-u (point 0.5))
                                        (the suport-curve-v (point 0.5))) 2))
                 :end (translate-along-vector 
                       (the curve-right (point 0.5))
                       (subtract-vectors (the suport-curve-u (point 0.5))
                                         (the suport-curve-v (point 0.5)))
                       (/(3d-distance (the suport-curve-u (point 0.5))
                                      (the suport-curve-v (point 0.5))) 2)))
        
   (surface-u :type 'lofted-surface
              :display-controls (list :color :red :line-thickness 2)
              :curves (if (the with-suport-curve?) 
                          (list (the curve-bottom)
                              (the suport-curve) 
                              (the curve-top))
                        (list (the curve-bottom)
                                (the curve-top))))

   (extended-v-min :type 'extended-surface
                   :display-controls (list :color :green :line-thickness 2)
                   :surface (the surface-u)
                   :curve (the curve-left)
                   :direction :v
                   :which-end :start
                   :deformation-param 0.5)
   
   (extended-v-max :type 'extended-surface
                   :display-controls (list :color :green :line-thickness 2)
                   :surface (the extended-v-min)
                   :curve (the curve-right)
                   :direction :v
                   :which-end :end
                   :deformation-param 0.5)))

