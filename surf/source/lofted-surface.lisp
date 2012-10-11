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
  
(define-object lofted-surface (surface)
  
  :documentation (:description "Loft of a surface through a list of curves with
various controls. "
                  :examples "<pre>
                  
 (in-package :surf)

  (define-object test-lofted-surface (base-object)
  
  :input-slots
  ((curves (list (the curve-1) (the curve-2) (the curve-3) (the curve-4)  )))
  
  :objects
  ((lofted-surface :type 'lofted-surface
                   :curves (the curves))
   
   (curve-1 :type 'b-spline-curve
            :display-controls (list :color :red :line-thickness 3)
            :control-points (list (make-point 0 0 0)
                                  (make-point 1 1 0)
                                  (make-point 0 1 0)
                                  (make-point 0 0 0) ))
   
   (curve-2 :type 'b-spline-curve
            :display-controls (list :color :red :line-thickness 3)
            :control-points (list (make-point 0 0 1)
                                  (make-point -1 1 1)
                                  (make-point 0 1 1)
                                  (make-point 0 0 1) ))
           
   (curve-3 :type 'b-spline-curve
            :display-controls (list :color :red :line-thickness 3)
            :control-points (list (make-point 0 0 2)
                                  (make-point -1 -1 2)
                                  (make-point 0 -1 2)
                                  (make-point 0 0 2) ))
   
   (curve-4 :type 'b-spline-curve
            :display-controls (list :color :red :line-thickness 3)
            :control-points (list (make-point 0 0 3)
                                  (make-point 1 -1 3)
                                  (make-point 0 -1 3)
                                  (make-point 0 0 3) ))))

 (generate-sample-drawing :object-roots (make-object 'test-lofted-surface)
                          :projection-direction :trimetric)

 </pre>" )

  :input-slots 
  ("List of GDL Curve objects. The curves through which the surface will be lofted."
   curves
                
   ("List of curve parameters. The parameter value on the rail-1 which 
should correspond to each respective profile curve. Defaults to the evenly 
spaced parameters between the u-min and u-max of the rail-1, one for each 
profile curve."
    rail-1-params (when (the rail-1)
                    (the rail-1 (equi-spaced-parameters (length (the curves))))))
                
   ("GDL Curve object. Guide rail corresponding to minimum U parameter of 
resulting surface. Defaults to nil."
    rail-1 nil)
                
   ("Boolean. If specified as non-nil, then the rail-1 will be used 
as a spine curve similar to what is described on page 462 of the NURBS book. 
Default is nil." 
    rail-1-is-spine? nil)
   

                
   ("GDL Curve object. Guide rail corresponding to maximum U parameter 
of resulting surface. Defaults to nil. If both rail-1 and rail2 are given, 
then they must be synchronized."
    rail-2 nil)
                
   ("Integer. The degree of the surface in the lofting direction. Defaults to 3."
    v-degree 3)
                
   ("Number. The fitting tolerance to fit to the loft curves.
0 means to interpolate exactly. Default is 0"
    tolerance 0)
                
   ("Boolean. Set this to t if the curves already have synchronized control points. 
It makes the lofted-surface much lighter-weight in terms of its control mesh. 
Default is NIL (which means the lofted-surface does not assume synchronized 
control points on the profile curves)."
    synchronized? nil)
   
   
   ("Boolean. If non-nil, we use the low-level nlib ascssk directly. If nil, we use
the SMLib make-skinned-surface routine. Default is nil" 
    use-ascssk? nil)
   
   ("GDL Curve object. Curve to use as spine for calling ascssk."
    spine nil)
   
   (spine-parameters (the spine (equi-spaced-parameters (length (the curves)))))
   
   (align-profiles? nil)
   
   (spine-z-axis (the (face-normal-vector :top)))
   
   (subdivision-level 5)
   
   (derivative-selection :all-derivatives)
   
   (skinning-degree 3)
   
   (u-or-v :u)
   )

  
  :computed-slots 
  (
   (native-surface
    (if (the use-ascssk?)
        (skin-with-spine *geometry-kernel*
                         :profile-curves (mapsend (the curves) :copy-new)
                         :synchronized? (the synchronized?)
                         :spine-curve (if (the spine)
                                          (the spine native-curve)
                                        (error "You must give a rail-1 in this case."))
                         :spine-parameters (the spine-parameters)
                         :align-profiles? (the align-profiles?)
                         :spine-z-axis (the spine-z-axis)
                         :subdivision-level (the subdivision-level)
                         :derivative-selection (the derivative-selection)
                         :skinning-degree (the skinning-degree)
                         :u-or-v (the u-or-v))
      (call-next-method)))
   
   
   (native-surface-iw 
    (if (the use-ascssk?)
        (call-next-method)
      (let ((control-points-1 (when (the rail-2) 
                                (length (the rail-1 b-spline-data))))
            (control-points-2 (when (the rail-2) 
                                (length (the rail-2 b-spline-data)))))
        (when (and control-points-1 control-points-2
                   (/= control-points-1 control-points-2))
          (error "For lofted-surface, rail-1 and rail-2, if both specified, must be synchronized."))
        (loft-surface* *geometry-kernel* (the curves) 
                       :synchronized? (the synchronized?)
                       :rail-1 (when (the rail-1) 
                                 (the rail-1 copy-new))
                       :rail-2 (when (the rail-2) 
                                 (the rail-2 copy-new))
                       :v-degree (the v-degree)
                       :tolerance (the tolerance)
                       :rail-1-params (the rail-1-params)
                       :rail-1-is-spine?
                       (cond ((and (the rail-1-is-spine?)
                                   (not (the rail-1)))
                              (warn "You have to give a rail to give rail-1-is-spine?~%"))
                             ((and (the rail-1-is-spine?)
                                   (the rail-1)) t)
                             (t nil))))))))






(define-object test-lofted-surface ()
  
  :input-slots
  ((curves (list (the curve-1) (the curve-2) (the curve-3) (the curve-4)  )))
  
  :objects
  ((lofted-surface :type 'lofted-surface
                   :curves (the curves))
   
   (curve-1 :type 'b-spline-curve
            :display-controls (list :color :red :line-thickness 3)
            :control-points (list (make-point 0 0 0)
                                  (make-point 1 1 0)
                                  (make-point 0 1 0)
                                  (make-point 0 0 0) ))
   
   (curve-2 :type 'b-spline-curve
            :display-controls (list :color :red :line-thickness 3)
            :control-points (list (make-point 0 0 1)
                                  (make-point -1 1 1)
                                  (make-point 0 1 1)
                                  (make-point 0 0 1) ))
           
   
   (curve-3 :type 'b-spline-curve
            :display-controls (list :color :red :line-thickness 3)
            :control-points (list (make-point 0 0 2)
                                  (make-point -1 -1 2)
                                  (make-point 0 -1 2)
                                  (make-point 0 0 2) ))
   
   (curve-4 :type 'b-spline-curve
            :display-controls (list :color :red :line-thickness 3)
            :control-points (list (make-point 0 0 3)
                                  (make-point 1 -1 3)
                                  (make-point 0 -1 3)
                                  (make-point 0 0 3) ))
                                   ))
          
          
