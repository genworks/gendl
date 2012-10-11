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

(in-package :surf)

(define-object split-surface (surface)
  
  :documentation (:description 
                  "Given a NURBS and a parameter in U or V direction,
split the surface at the parameter and return one section or the other as the 
toplevel self of this instance. Note that both resulting sections are also 
reflected in (the surfaces) sequence which is a hidden child in this object.

As an alternative to a parameter, a projection-point and projection-vector 
can also be given, and the U or V parameter at the resulting surface point
will be used as the parameter.

"
                                
                  :examples "
<pre>

 (in-package :gdl-user)

 (define-object test-split-surface (base-object)
  
   :computed-slots ((projection-point (make-point 10 0 3))
                    (projection-vector (make-vector 1 0 0))
                    (u-or-v :u :settable)
                    (keep-side :left :settable))
  
   :objects
   ((test-surface :type 'test-b-spline-surface
                  :display-controls nil)

   
    (split :type 'split-surface
           :display-controls (list :color :red :line-thickness 3)
           :surface-in (the test-surface)
           :pass-down (keep-side u-or-v projection-point projection-vector))))


 (generate-sample-drawing :object-roots (make-object 'test-split-surface)
                          :projection-direction (getf *standard-views* :trimetric))


</pre>
"
                                
                  )
  
  :input-slots
  (("Keyword symbol, one of :u or :v. Determines the direction of the split." 
    u-or-v :u)
   
   ("GDL Surface object. The surface to be split." surface-in)
   
   ("Number. The parameter in U or V direction at which to do the split. This must
lie in the domain between (the u-min) [or (the v-min)] and (the u-max) [or (the v-max)] 
of the surface-in. If it is outside this domain, a warning will be thrown and the 
value will be pinned to the nearest value within the domain.

If this input is not specified and you specify a projection-point and projection-vector,
then this projected point will be used to establish the parameter for splitting. "
    
    parameter (ecase (the u-or-v)
                (:u (get-u (get-uv-point-of (the projected-point))))
                (:v (get-v (get-uv-point-of (the projected-point))))))
   
   
   ("3D Point or nil. If given and parameter is not given, 
this point will be projected onto the surface using (the projection-vector) 
to establish the split parameter. Defaults to nil." 
    projection-point nil)
   
   ("3D Vector or nil. If given and parameter is not given, 
this will be used to project (the projection-point) 
onto the surface to establish the split parameter. 
Defaults to nil." projection-vector nil)
   
   ("Keyword symbol, one of :left or :right. Determines which half of the split
surface to reflect in this instance. Both halves will be reflected in (the surfaces)
hidden-object sequence which is a child of this instance." 
    keep-side :left))

  
  :computed-slots
  ((projected-points (when (and (the projection-point)
                                (the projection-vector))
                       (the surface-in (projected-points (the projection-point)
                                                         (the projection-vector)))))
   
   (projected-point (least #'(lambda(point) (3d-distance (get-3d-point-of point) (the projection-point)))
                           (the projected-points)))
   

   (pinned-parameter (if (< (the surface-in (evaluate (ecase (the u-or-v)
                                                        (:u :u-min)
                                                        (:v :v-min))))
                            (the parameter)
                            (the surface-in (evaluate (ecase (the u-or-v)
                                                        (:u :u-max)
                                                        (:v :v-max)))))
                         (the parameter)
                       (error "In split-surface at ~s, parameter must 
be between the u-min and u-max of the surface-in." 
                              (cons 'the (reverse (the root-path))))))
   
   (%native-surfaces% (split-surface-at-param
                       *geometry-kernel* 
                       :native-surface (the surface-in native-surface)
                       :u-or-v (ecase (the u-or-v) (:u :u) (:v :v))
                       :parameter (the pinned-parameter)))
   
   (native-surface (ecase (the keep-side) 
                     (:left (the (surfaces 0) native-surface))
                     (:right (the (surfaces 1) native-surface)))))
  
  :hidden-objects
  ((surfaces :type 'surface
             :sequence (:size 2)
             :native-surface (nth (the-child index)
                                  (the %native-surfaces%)))))
                          
  



   
   
