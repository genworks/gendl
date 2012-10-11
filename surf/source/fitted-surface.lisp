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

  
(define-object fitted-surface (surface)
  
  :documentation (:description "Fits a surface through a net of points with given degrees and parameterizations. Currently
only interpolated surfaces are supported, this will be extended to allow smooth fitting without the surface necessarily
interpolating (going through) each of the points."
                  
                  :examples "<pre>

 (in-package :gdl-user)

 (define-object c11-test (surface) 

  :input-slots () 

  :computed-slots ()

  :objects
  ((surf-test :type 'fitted-surface
              :hidden nil
              :c11? t
              :points (list (list (make-point -1 0 0)
                                  (make-point 0 0 0) 
                                  (make-point 0.001 0.0 0)
                                  (make-point 1 1 0)
                                  (make-point 1.001 1 0)
                                  (make-point 2 1 0)
                                  (make-point 2.001 1 0)
                                  (make-point 3 2 0)
                                  (make-point 3.001 2.001 0)
                                  (make-point 4 3 0) 
                                  (make-point 5 4 0))   
                            (list
                             (make-point -1 0 1)
                             (make-point 0 0 1) 
                             (make-point 0.001 0.0 1)
                             (make-point 1 1 1)
                             (make-point 1.001 1 1)
                             (make-point 2 1 1)
                             (make-point 2.001 1 1)
                             (make-point 3 2 1)
                             (make-point 3.001 2.001 1)
                             (make-point 4 3 1)
                             (make-point 5 4 1))))))


 (define-object test-fitted-surface (fitted-surface) 

   :input-slots
   ((display-controls (list :color :green-spring :isos (list :n-v 19 :n-u 19)))
   
    (grid-width 4 :settable) (grid-length 4 :settable) (grid-height 4 :settable))
  
   :computed-slots
   ((points (list (list (make-point 0 0 0)
                        (make-point (/ (the grid-width) 4) 0 0)
                        (make-point (half (the grid-width)) 0 0)
                        (make-point (* 3/4 (the grid-width)) 0 0)
                        (make-point (the grid-width) 0 0))
                         
                  (list (make-point 0 (/ (the grid-length) 4) 0)
                        (make-point (/ (the grid-width) 4) 
                                    (/ (the grid-length) 4) 
                                    (/ (the grid-height) 4))
                        (make-point (half (the grid-width)) 
                                    (/ (the grid-length) 4) 
                                    (* (/ (the grid-height) 4) 1.6))
                        (make-point (* 3/4 (the grid-width)) 
                                    (/ (the grid-length) 4) 
                                    (/ (the grid-height) 4))
                        (make-point (the grid-width) 
                                    (/ (the grid-length) 4) 0))
                         
                  (list (make-point 0 (half (the grid-length)) 0)
                        (make-point (/ (the grid-width) 4) 
                                    (half (the grid-length)) 
                                    (* (/ (the grid-height) 4) 1.8))
                        (make-point (half (the grid-width)) 
                                    (half (the grid-length)) 
                                    (the grid-height))
                        (make-point (* 3/4 (the grid-width)) 
                                    (half (the grid-length)) 
                                    (* 3/4 (the grid-height)))
                        (make-point (the grid-width) (half (the grid-length)) 0))
                         
                  (list (make-point 0 (* 3/4 (the grid-length)) 0)
                        (make-point (/ (the grid-width) 4) 
                                    (* 3/4 (the grid-length)) 
                                    (min (* (/ (the grid-height) 4) 
                                            (* (/ (the grid-height) 4) 1.4)) 
                                         (the grid-height)))
                        (make-point (half (the grid-width)) 
                                    (* 3/4 (the grid-length)) 
                                    (min (* (/ (the grid-height) 4) 
                                            (* (/ (the grid-height) 4) 1.8)) 
                                         (the grid-height)))
                        (make-point (* 3/4 (the grid-width)) 
                                    (* 3/4 (the grid-length)) 
                                    (/ (the grid-height) 4))
                        (make-point (the grid-width) 
                                    (* 3/4 (the grid-length)) 0))
                         
                  (list (make-point 0 (the grid-length) 0)
                        (make-point (/ (the grid-width) 4) (the grid-length) 0)
                        (make-point (half (the grid-width)) (the grid-length) 0)
                        (make-point (* 3/4 (the grid-width)) (the grid-length) 0)
                        (make-point (the grid-width) (the grid-length) 0))))))

 (generate-sample-drawing :objects (make-object 'test-fitted-surface)
                          :projection-direction :trimetric)


 </pre>")
                  
  
  :input-slots
  ("List of lists of 3D Points. The points for fitting, with inner lists representing U direction and outer lists V direction." 
   points 
   
   ("List of 3D vectors of same length as points, or nil. If given, these are the surface normals at each point." 
    normals nil)
   
   
   ("Integer. The starting degree for the fit algorithm in the U direction. Default is 1." 
    u-start 1)
   
   ("Integer. The starting degree for the fit algorithm in the V direction. Default is 1." 
    v-start 1)
   
   ("Integer. The desired degree of the resultant surface in the U direction. Default is 3." 
    u-degree 3)
   
   ("Integer. The desired degree of the resultant surface in the V direction. Default is 3." 
    v-degree 3)
   
   ("Keyword symbol, one of :uniform, :chord-length, :centripetal. The parameterization to use in the resultant surface if
interpolant? is t. Default is :chord-length" 
    parameterization :chord-length)
   
   
   ("Boolean. Indicates whether the surface will interpolate the points. Defaults to t." 
    interpolant? t)
   
   ("Boolean. If interpolated, indicates whether to compute a C11 continuous nonrational bicubic NURBS surface. Defaults to nil."
    c11? nil)
   
   ("Keyword symbol, one of :bessel, :akima.  The method used to compute tangents. Defaults to :akima."
    tangent-method :akima)
   
   ("Number. Tolerance for fit. Defaults to *3d-approximation-tolerance-default*." 
    tolerance *3d-approximation-tolerance-default*))

  
  :computed-slots
  ((native-surface (cond ((the interpolant?)
                          (cond ((the c11?)
                                 (interpolate-c11-surface *geometry-kernel* (the points)
                                                          :tangent-method (the tangent-method)))
                                (t (interpolate-surface *geometry-kernel* (the points) 
                                                        (the u-degree) (the v-degree) 
                                                        (the parameterization)))))
                         ((the normals)
                          (approximate-surface *geometry-kernel* (the points) (the normals)))
                         (t (approximate-surface *geometry-kernel* (the points) (the normals)
                                                 :u-start (the u-start) :u-required (the u-degree)
                                                 :v-start (the v-start) :v-required (the v-degree)
                                                 :tolerance (the tolerance)))))
   
   (bounding-box (bounding-box-from-points (flatten (the points))))))








  
