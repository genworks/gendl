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


(define-object extended-surface (surface)
  
  :documentation (:description "Extends a surface to a curve, so that curve will 
become one of the new boundaries for the surface. Continuity is controlled via
options. Note that in the example, extended and extended-2 do not give a smooth
transition to the extended part of the surface because the original surface
is only degree 1 in the direction of extension."
                  
                  :examples "
<pre>

 (in-package :gdl-user)
                   
 (define-object extended-surface-test (base-object)
  
   :computed-slots
   ((regression-test-data (list (multiple-value-list (the extended b-spline-data))
                                (multiple-value-list (the extended-2 b-spline-data))
                                (multiple-value-list (the extended-3 b-spline-data))
                                (multiple-value-list (the extended-4 b-spline-data))))
   
    (display-list-objects (list (the loft)
                                (the extended)
                                (the extended-2))))
  
   :objects
   ((test3 :type 'linear-curve 
           :start (make-point 0 0 0) 
           :end (make-point 10 0 0))
   
    (test4 :type 'linear-curve
           :start (make-point 0 10 0) 
           :end (make-point 10 10 0))
   
   
    (mid-1 :type 'linear-curve
           :start (make-point 0 5.0 1)
           :end (make-point 10 5.0 1))
   
    (mid-2 :type 'linear-curve
           :start (make-point 0 8.0 1)
           :end (make-point 10 8.0 1))
   
    (bridge-1 :type 'b-spline-curve
              :control-points (list (make-point 0 0 0)
                                    (make-point -2 5.0 3) 
                                    (make-point -2 8.0 3) 
                                    (make-point 0 10 0)))       
    (bridge-2 :type 'b-spline-curve
              :control-points (list (make-point 10 0 0)
                                    (make-point 12 5.0 5) 
                                    (make-point 12 8.0 5) 
                                    (make-point 10 10 0)))      

    (bridge-3 :type 'b-spline-curve
              :control-points (list (make-point 0 -1 0)
                                    (make-point 3 -1 5)
                                    (make-point 7 -1 5)
                                    (make-point 10 -1 0)))

   
    (loft :type 'lofted-surface
          :curves (list (the test3) (the mid-1) 
                        (the mid-2) (the test4)))
   
    (extended :type 'extended-surface
              :display-controls (list :color :red :line-thickness 2)
              :surface (the loft)
              :curve (the bridge-1)
              :direction :v
              :which-end :start)

    (extended-2 :type 'extended-surface
                :display-controls (list :color :green :line-thickness 2)
                :surface (the loft)
                :curve (the bridge-1)
                :direction :v
                :which-end :start
                :deformation-param (+ (* 0.25 (- (the-child surface v-max)
                                                 (the-child surface v-min)))
                                      (the-child surface u-min)))
   
    (extended-3 :type 'extended-surface
                :display-controls (list :color :orange
                                        :isos (list :n-u 25 :n-v 25))
                :surface (the loft)
                :curve (the bridge-3)
                :direction :u
                :continuity :cmax
                :which-end :start)
   
    (extended-4 :type 'extended-surface
                :display-controls (list :color :blue 
                                        :isos (list :n-u 25 :n-v 25))
                :surface (the loft)
                :curve (the bridge-3)
                :direction :u
                :deformation-param (+ (* 0.25 (- (the-child surface u-max)
                                                 (the-child surface u-min)))
                                      (the-child surface u-min))
                :continuity :cmax
                :which-end :start)))              
                  
 (generate-sample-drawing :objects (the-object (make-object 'extended-surface-test)
                                               display-list-objects)
                          :projection-direction (getf *standard-views* :trimetric))

</pre>")                  
  

  :input-slots
  ("GDL surface object. The surface to be extended." surface 
   "GDL curve object. The curve to which the surface should be extended." curve
   
   ("Keyword symbol, one of :start or :end. Default is :start."
    which-end :start)
   
   ("Keyword symbol, if deformation-param is given this can 
be one of :c1, :c2, or :cmax, and if deformation-param is not
given this can be one of :g1 or :cmax. Default is :c1 if
deformation-param is given and  :g1 if deformation-param is 
not given."
    continuity (if (the deformation-param) :c1 :g1))
   
   ("Keyword symbol, one of :u or :v. The direction of extension. Note that if 
deformation-param is given, it will be a U parameter if this input :u, 
and a V parameter if this input is :v. Default is :u."
    direction :u)
   
   
   ("Number, either a U or a V surface parameter. This value, if given, controls how 
far inward from the affected boundary the surface is modified. If (the direction) 
is :u, then this will be a U parameter, and likewise if (the direction) is :v, 
then this will be a V parameter. Default is nil which indicates no specific 
control over any deformation." 
    deformation-param nil))
  
  :computed-slots
  ((native-surface 
    (make-extended-surface 
     *geometry-kernel* 
     :surface (the surface native-surface) 
     :direction (the direction)
     :curve (the curve native-curve)
     :direction (the direction)
     :which-end (the which-end)
     :continuity (if (the deformation-param)
                     (ecase (the continuity)
                       (:c1 1)
                       (:c2 5)
                       (:cmax 4))
                   (ecase (the continuity)
                     (:g1 2)
                     (:cmax 4)))
     :deformation-param (the deformation-param)))))
                                          
  
  
  



