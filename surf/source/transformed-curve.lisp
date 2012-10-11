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



(define-object boxed-curve (outline-specialization-mixin b-spline-curve)
  
  :documentation (:description "This object behaves as a hybrid of a curve 
and a normal box. You pass in a curve-in, and it essentially traps the curve 
in a box, which will respond to normal GDL :center and :orientation. 
You can also pass a scale, or scale-x, or scale-y, or scale-z as with
a transformed-curve."
                  
                   :examples "<pre>

  (define-object boxed-curves-test (base-object)
  
   :computed-slots ((b-spline (the b-splines (curves 2))))
  
   :objects
   ((b-splines :type 'test-b-spline-curves)
    
    (boxed :type 'boxed-curve
           :curve-in (the b-splines (curves 2)))
    
    (translated :type 'boxed-curve
                :curve-in (the b-spline)
                :center (translate (the center) :left 15))
    
    (twisted :type 'boxed-curve
             :curve-in (the boxed)
             :orientation 
             (alignment :left (the (face-normal-vector :top))
                        :rear (rotate-vector-d (the (face-normal-vector :rear))
                                               30
                                               (the (face-normal-vector :top)))))
    (rotated :type 'boxed-curve
             :curve-in (the b-spline)
             :display-controls (list :color :purple)
             :orientation 
             (alignment :left 
                        (rotate-vector-d (the (face-normal-vector :left))
                                         50
                                         (the (face-normal-vector :rear)))))
   
    (rotated-about :type 'boxed-curve
                   :curve-in (the b-spline)
                   :display-controls (list :color :purple)
                   :orientation-center (translate (the center) :right 2.5)
                   ;;:center (translate (the center) :up 5)
                   :orientation 
                   (alignment :left 
                              (rotate-vector-d (the (face-normal-vector :left))
                                               45
                                               (the (face-normal-vector :rear)))))
  
    (moved-up :type 'boxed-curve
              :curve-in (the rotated-about)
              :center (translate (the rotated-about center) 
                                 :up 7
                                 :left 5))

    (straightened :type 'boxed-curve
                  :curve-in (the moved-up)
                  :orientation 
                  (alignment 
                   :left 
                   (rotate-vector-d 
                    (the-child curve-in (face-normal-vector :left))
                    45
                    (the-child curve-in (face-normal-vector :rear)))
                   :rear (the-child curve-in (face-normal-vector :rear))))

    (rotated-straightened :type 'boxed-curve
                          :curve-in (the straightened)
                          :orientation (the moved-up orientation)
                          :orientation-center 
                          (translate (the-child curve-in center) :up 2.5))
 
    (rotated-straightened-moved :type 'boxed-curve
                                :curve-in (the rotated-straightened)
                                :center (translate (the-child curve-in center) 
                                                   :right 5))
    (center-sphere :type 'sphere 
                   :radius 0.3 
                   :center (the moved-up-and-straightened orientation-center))
   
   
    (moved-up-and-straightened 
     :type 'boxed-curve
     :curve-in (the straightened)
     :center (translate (the-child orientation-center) :right 7)
     :orientation 
     (alignment :left 
                (the-child curve-in  (face-normal-vector :rear)) 
                :front
                (rotate-vector-d (the-child curve-in (face-normal-vector :left))
                                 45
                                 (the-child curve-in (face-normal-vector :rear))))
     :orientation-center (translate (the straightened center) :up 2.5))
    
    (moved-up-and-straightened-1 
     :type 'boxed-curve
     :curve-in (the straightened)
     :center (translate (the-child curve-in center) :right 14)
     :orientation (the rotated-straightened orientation)
     :orientation-center (translate (the straightened center) :up 2.5))
    
    (moved-up-and-straightened-2 
     :type 'boxed-curve
     :curve-in (the straightened)
     :center (translate (the-child curve-in center) :right 21)
     :orientation (the rotated-straightened orientation)
     :orientation-center (translate (the straightened center) :up 2.5))
   
    (transformed 
     :type 'boxed-curve
     :curve-in (the b-spline)
     :center (translate (the center) :left 50)
     :orientation 
     (alignment :rear 
                (rotate-vector-d (the (face-normal-vector :rear))
                                 30
                                 (the (face-normal-vector :right)))))))
  
 </pre>
 ")
                  
   :input-slots
   ("GDL Curve object. This can be any type of curve, e.g. b-spline-curve, fitted-curve, or an edge from a solid brep.
 Note that the reference-box of this curve (i.e. its center and orientation) will have an effect on the resulting
 boxed-curve. If you want to consider the incoming curve-in as being in global space, then make sure its center 
 is (0 0 0) and its orientation is nil or equivalent to geom-base::+identity-3x3+"
    curve-in
   
    ("Boolean. This determines whether the reference box is displayed along with the curve. Default is t (will be changed to nil)." 
     show-box? nil)

    ("Boolean. This determines whether the tight box is displayed along with the curve. Default is nil." 
     show-tight-box? nil)
   
    ("Boolean. This determines whether the control polygon is displayed along with the curve. Default is nil." 
     show-control-polygon? nil)
   
    ("3x3 Transformation Matrix. This will be the new orientation for the box and the contained curve. Default is 
 (the curve-in orientation) -- i.e. identical orientation with the provided curve-in."
     orientation (the curve-in% orientation))
   
    ("3D Point in global space. If you provide this, the curve's reference box will be moved to have its center
at this point, before any orientation is applied. This will become the new center of the resulting boxed-curve,
unless you explicitly pass in a different center. Default is nil."
     orientation-center nil)
   
    ("3D Point in global space. You can pass in a new center for the curve's reference box, 
which will move the whole box including the curve. This defaults to the 
orientation-center (if given), otherwise to the (the curve-in center)."
     center (or (the orientation-center) (the curve-in% center)))
   
    ("GDL object which mixes in base-object. The current boxed-curve will be positioned and oriented with respect 
to the center and orientation of this object. The default is (the curve-in)."
     from-object (the curve-in%))
   
    ("3x3 Transformation Matrix. The orientation with respect to which this object should be oriented. Normally
this should not be specified by user code, unless you know what you are doing [e.g. to override the orientation
of a curve-in which is meaningless and force it to be interpreted as a curve in the parent's coordinate system, 
you could specify this as (the orientation) when passing it in from the parent]. 
Default is (the curve-in orientation)."
     from-orientation (the from-object orientation)
     
     )
   
    ("3D Point in global space. The center with respect to which this object should be positioned. Normally
this should not be specified by user code, unless you know what you are doing [e.g. to override the center
of a curve-in which is meaningless and force it to be interpreted as a curve in global space, you could
specify this as (the center) when passing it in from the parent]. 
Default is (the curve-in center)."
     from-center (the from-object center))
   
    ("Number. The overall scale factor for X, Y, and Z, if no individual scales are specified. Defaults to 1."
     scale 1)
   
    ("Number. The scale factor for X. Defaults to 1."
     scale-x (the scale))
   
    ("Number. The scale factor for Y. Defaults to 1."
     scale-y (the scale))
   
    ("Number. The scale factor for Z. Defaults to 1."
     scale-z (the scale))
   
    ("Number. Tolerance to determine whether the boxed-curve has
moved with respect to the original. Default is *zero-epsilon*"
     translation-threshold *zero-epsilon*)
    )

  
   :computed-slots
   ((curve-in% (if (null (the curve-in))
                   (error "You must pass a non-nil curve-in into boxed-curve at ~s~%"
                               (cons 'the (reverse (the root-path))))
                 (the curve-in)))
    
    
    (translated? (the orientation-center))
   
    (moved? (not (coincident-point? 
                  (the from-center) (the center)
                  :tolerance (the translation-threshold))))

   
   
    (oriented? (not (equalp (the orientation) (the from-orientation))))
    (width (the curve-in% box-width))
    (length (the curve-in% box-length))
    (height (the curve-in% box-height))
    (degree (the curve-in% degree))
    (knot-vector (the curve-in% knot-vector))
    (weights (the curve-in% weights))
    (translation (when (the translated?) 
                   (subtract-vectors (the from-center) 
                                     (the orientation-center))))

    
    #+nil
    (unweighted 
     (if (the curve-in% rational?)
         (mapcar #'(lambda(point weight) 
                     (scalar*vector (/ weight) point))
                 (the curve-in% control-points) (the curve-in% weights))
       (the curve-in% control-points)))
   

    (unweighted 
     (the curve-in% control-points))
   
   
    (scaled (if (every #'(lambda(value) (= value 1))
                       (list (the scale-x) (the scale-y) (the scale-z)))
                (the unweighted)
              (mapcar #'(lambda(point)
                          (let ((local (the from-orientation-object (global-to-local point))))
                            (the from-orientation-object 
                              (local-to-global
                               (make-point (* (get-x local) (the scale-x))
                                           (* (get-y local) (the scale-y))
                                           (* (get-z local) (the scale-z)))))))
                      (the unweighted))))
   
    (translated (if (the translated?)
                    (mapcar #'(lambda(point) (add-vectors point (the translation))) 
                            (the scaled))
                  (the scaled)))
   
   
    (oriented (if (the oriented?)
                  (mapcar #'(lambda(point) (the to-orientation-object 
                                             (local-to-global 
                                              (the from-orientation-object (global-to-local point)))))
                          (the translated))
                (the translated)))

   
    (restored (if (the translated?)
                  (mapcar #'(lambda(point)
                              (add-vectors point (subtract-vectors (the orientation-center)
                                                                   (the from-center))))
                          (the oriented))
                (the oriented)))
   
    
    (control-points (the %control-points))

    #+nil
    (control-points (if (the curve-in% rational?)
                        (mapcar #'(lambda(point weight) (scalar*vector weight point))
                                (the %control-points) (the weights))
                      (the %control-points)))

   
    (%control-points (if (the moved?)
                         (mapcar #'(lambda(point)
                                     (add-vectors point (subtract-vectors (the center) 
                                                                          (or (the orientation-center)
                                                                              (the from-center)))))
                                 (the restored))
                       (the restored)))

    (outline-objects (cons (the curve)
                           (append (when (the show-box?) (list (the box)))
                                   (when (the show-tight-box?) (list (the tight-box)))
                                   (when (the show-control-polygon?) (list (the control-polygon)))))))

  
   :hidden-objects 
   ((curve :type 'curve :native-curve (the native-curve)
           :display-controls (the display-controls))

   
    (box :type 'box)
   
   
    (control-polygon :type 'global-polyline
                     :display-controls (list :color :green)
                     :vertex-list (the b-spline-data))
   
    (from-orientation-object :type 'base-object
                             :center (the from-center)
                             :orientation (the from-orientation))
    (to-orientation-object :type 'base-object
                           :center (the from-center))))



(define-object boxed-surfaces-test (base-object)
  
 ;;bounding-box-from-points gives errors  
  
  :objects
  ((b-spline :type 'test-b-spline-surface)
   
   (boxed :type 'boxed-surface
          :surface-in (the b-spline))
   
   (translated :type 'boxed-surface
               :surface-in (the b-spline)
               :center (translate (the center) :left 15))
   

   
   
   (twisted :type 'boxed-surface
            :surface-in (the boxed)
            :orientation (alignment :left (the (face-normal-vector :top))
                                    :rear (rotate-vector-d (the (face-normal-vector :rear))
                                                           30
                                                           (the (face-normal-vector :top)))))
   
   
   (rotated :type 'boxed-surface
            :surface-in (the b-spline)
            :display-controls (list :color :purple)
            :orientation (alignment :left 
                                    (rotate-vector-d (the (face-normal-vector :left))
                                                     50
                                                     (the (face-normal-vector :rear)))))
   
   
   (rotated-about :type 'boxed-surface
                  :surface-in (the b-spline)
                  :display-controls (list :color :purple)
                  :orientation-center (translate (the center) :right 2.5)
                  ;;:center (translate (the center) :up 5)
                  :orientation (alignment :left 
                                          (rotate-vector-d (the (face-normal-vector :left))
                                                           45
                                                           (the (face-normal-vector :rear)))))
   
   
   (moved-up :type 'boxed-surface
             :surface-in (the rotated-about)
             :center (translate (the rotated-about center) 
                                :up 7
                                :left 5))

   
   (straightened :type 'boxed-surface
                 :surface-in (the moved-up)
                 :orientation 
                 (alignment :left 
                            (rotate-vector-d (the-child surface-in (face-normal-vector :left))
                                             45
                                             (the-child surface-in (face-normal-vector :rear)))
                            :rear (the-child surface-in (face-normal-vector :rear))))
                                         
   
   (rotated-straightened :type 'boxed-surface
                         :surface-in (the straightened)
                         :orientation (the moved-up orientation)
                         :orientation-center (translate (the-child surface-in center) :up 2.5)
                         )
   
   
   
   (rotated-straightened-moved :type 'boxed-surface
                               :surface-in (the rotated-straightened)
                               :center (translate (the-child surface-in center) :right 5))
                               
   
   (center-sphere :type 'sphere :radius 0.3 :center (the moved-up-and-straightened orientation-center))
   
   
   (moved-up-and-straightened :type 'boxed-surface
                              :surface-in (the straightened)
                              
                              :center (translate (the-child orientation-center) :right 7)
                              
                              :orientation (alignment :left (the-child surface-in (face-normal-vector :rear))
                                                      
                                                      :front
                                                      (rotate-vector-d (the-child surface-in (face-normal-vector :left))
                                                                       45
                                                                       (the-child surface-in (face-normal-vector :rear))))
                              
                              
                              :orientation-center (translate (the straightened center) :up 2.5)
                              )
   
   
   
   
   (moved-up-and-straightened-1 :type 'boxed-surface
                                :surface-in (the straightened)
                              
                                :center (translate (the-child surface-in center) :right 14)
                              
                                :orientation (the rotated-straightened orientation)
                              
                                :orientation-center (translate (the straightened center) :up 2.5)
                                )
   
   (moved-up-and-straightened-2 :type 'boxed-surface
                                :surface-in (the straightened)
                              
                                :center (translate (the-child surface-in center) :right 21)
                              
                                :orientation (the rotated-straightened orientation)
                              
                                :orientation-center (translate (the straightened center) :up 2.5)
                                )
   
   
   (transformed :type 'boxed-surface
                :surface-in (the b-spline)
                :center (translate (the center) :left 50)
                :orientation (alignment :rear 
                                        (rotate-vector-d (the (face-normal-vector :rear))
                                                         30
                                                         (the (face-normal-vector :right)))))))


(define-object boxed-surface (b-spline-surface)
  
  :documentation (:description "This object behaves as a hybrid of a surface and a normal box. You 
pass in a surface-in, and it essentially traps the surface in a box, which will respond to normal
GDL :center and :orientation. You can also pass a scale, or scale-x, or scale-y, or scale-z as with
a transformed-surface."
                  
                  :examples "<pre>

 (define-object boxed-surfaces-test (base-object)

   ;;bounding-box-from-points gives errors  
  
   :objects
   ((b-spline :type 'test-b-spline-surface)
   
    (boxed :type 'boxed-surface
           :surface-in (the b-spline))
   
    (translated :type 'boxed-surface
                :surface-in (the b-spline)
                :center (translate (the center) :left 15))
   
    (twisted :type 'boxed-surface
             :surface-in (the boxed)
             :orientation 
             (alignment :left (the (face-normal-vector :top))
                        :rear (rotate-vector-d (the (face-normal-vector :rear))
                                               30
                                               (the (face-normal-vector :top)))))
    (rotated :type 'boxed-surface
             :surface-in (the b-spline)
             :display-controls (list :color :purple)
             :orientation 
             (alignment :left 
                        (rotate-vector-d (the (face-normal-vector :left))
                                         50
                                         (the (face-normal-vector :rear)))))
    
    (rotated-about :type 'boxed-surface
                   :surface-in (the b-spline)
                   :display-controls (list :color :purple)
                   :orientation-center (translate (the center) :right 2.5)
                   ;;:center (translate (the center) :up 5)
                   :orientation 
                   (alignment :left 
                              (rotate-vector-d (the (face-normal-vector :left))
                                               45
                                               (the (face-normal-vector :rear)))))
    
    (moved-up :type 'boxed-surface
              :surface-in (the rotated-about)
              :center (translate (the rotated-about center) 
                                 :up 7
                                 :left 5))
   
    (straightened :type 'boxed-surface
                  :surface-in (the moved-up)
                  :orientation 
                  (alignment :left 
                             (rotate-vector-d 
                              (the-child surface-in (face-normal-vector :left))
                              45
                              (the-child surface-in (face-normal-vector :rear)))
                             :rear (the-child surface-in (face-normal-vector 
                                                          :rear))))

    (rotated-straightened :type 'boxed-surface
                          :surface-in (the straightened)
                          :orientation (the moved-up orientation)
                          :orientation-center 
                          (translate (the-child surface-in center)  :up 2.5))
  
    (rotated-straightened-moved :type 'boxed-surface
                                :surface-in (the rotated-straightened)
                                :center (translate (the-child surface-in center)
                                                   :right 5))

    (center-sphere :type 'sphere 
                   :radius 0.3 
                   :center (the moved-up-and-straightened orientation-center))
      
    (moved-up-and-straightened 
     :type 'boxed-surface
     :surface-in (the straightened)
     :center (translate (the-child orientation-center) :right 7)
     :orientation 
     (alignment :left (the-child surface-in (face-normal-vector :rear))
                :front
                (rotate-vector-d 
                 (the-child surface-in (face-normal-vector :left))
                 45
                 (the-child surface-in (face-normal-vector :rear))))
     :orientation-center (translate (the straightened center) :up 2.5))
 
    (moved-up-and-straightened-1
     :type 'boxed-surface
     :surface-in (the straightened)
     :center (translate (the-child surface-in center) :right 14)
     :orientation (the rotated-straightened orientation)
     :orientation-center (translate (the straightened center) :up 2.5))
    
    (moved-up-and-straightened-2 
     :type 'boxed-surface
     :surface-in (the straightened)
     :center (translate (the-child surface-in center) :right 21)
     :orientation (the rotated-straightened orientation)
     :orientation-center (translate (the straightened center) :up 2.5))
    
    (transformed :type 'boxed-surface
                 :surface-in (the b-spline)
                 :center (translate (the center) :left 50)
                 :orientation 
                 (alignment :rear 
                            (rotate-vector-d (the (face-normal-vector :rear))
                                             30
                                             (the (face-normal-vector :right)))))))
 </pre>")

  
  
  :input-slots
  ("GDL Surface object. This can be any type of surface, e.g. b-spline-surface, fitted-surface, or an edge from a solid brep.
 Note that the reference-box of this surface (i.e. its center and orientation) will have an effect on the resulting
 boxed-surface. If you want to consider the incoming surface-in as being in global space, then make sure its center 
 is (0 0 0) and its orientation is nil or equivalent to geom-base::+identity-3x3+"
   surface-in
   
   ("Boolean. This determines whether the reference box is displayed along with the surface. Default is nil." 
    show-box? nil)

   ("Boolean. This determines whether the tight box is displayed along with the surface. Default is nil." 
    show-tight-box? nil)
   
   ("Boolean. This determines whether the control polygon is displayed along with the surface. Default is t (will be changed to nil)." 
    show-control-polygon? nil)
   

   ("3x3 Transformation Matrix. This will be the new orientation for the box and the contained surface. Default is 
 (the surface-in orientation) -- i.e. identical orientation with the provided surface-in."
    orientation (the surface-in% orientation))
   
   ("3D Point in global space. If you provide this, the surface's reference box will be moved to have its center
at this point, before any orientation is applied. This will become the new center of the resulting boxed-surface,
unless you explicitly pass in a different center. Default is nil."
    orientation-center nil)
   
   ("3D Point in global space. You can pass in a new center for the surface's reference box, 
which will move the whole box including the surface. This defaults to the 
orientation-center (if given), otherwise to the (the surface-in center)."
    center (or (the orientation-center) (the surface-in% center)))
   
   
   ("GDL object which mixes in base-object. The current boxed-surface will be positioned and oriented with respect 
to the center and orientation of this object. The default is (the surface-in)."
    from-object (the surface-in%))
   
   ("3x3 Transformation Matrix. The orientation with respect to which this object should be oriented. Normally
this should not be specified by user code, unless you know what you are doing [e.g. to override the orientation
of a surface-in which is meaningless and force it to be interpreted as a surface in the parent's coordinate system, 
you could specify this as (the orientation) when passing it in from the parent]. Default is (the surface-in orientation)."
    from-orientation (the from-object orientation))
   
   ("3D Point in global space. The center with respect to which this object should be positioned. Normally
this should not be specified by user code, unless you know what you are doing [e.g. to override the center
of a surface-in which is meaningless and force it to be interpreted as a surface in global space, you could
specify this as (the center) when passing it in from the parent]. Default is (the surface-in orientation)."
    from-center (the from-object center))

   
   ("Number. The overall scale factor for X, Y, and Z, if no individual scales are specified. Defaults to 1."
    scale 1)
   
   ("Number. The scale factor for X. Defaults to 1."
    scale-x (the scale))
   
   ("Number. The scale factor for Y. Defaults to 1."
    scale-y (the scale))
   
   ("Number. The scale factor for Z. Defaults to 1."
    scale-z (the scale)))
  
  
  :computed-slots
  (
   (surface-in% (or (the surface-in)
                    (error "You must pass a non-nil surface-in into boxed-surface at ~s~%"
                              (cons 'the (reverse (the root-path))))))
   (translated? (the orientation-center))
   (moved? (not (coincident-point? (the from-center) (the center))))
   (oriented? (not (equalp (the orientation) (the from-orientation))))
   (width (the surface-in% width))
   (length (the surface-in% length))
   (height (the surface-in% height))
   (u-degree (the surface-in% u-degree))
   (v-degree (the surface-in% v-degree))
   (u-knot-vector (the surface-in% u-knot-vector))
   (v-knot-vector (the surface-in% v-knot-vector))
   (weights (the surface-in% weights))
   (translation (when (the translated?) (subtract-vectors (the from-center) (the orientation-center))))
   
   #+nil
   (unweighted (if (the surface-in% rational?)
                   (mapcar #'(lambda(point-row weight-row)
                               (mapcar #'(lambda(point weight) (scalar*vector (/ weight) point))
                                       point-row weight-row))
                           (the surface-in% control-points) (the surface-in% weights))
                 (the surface-in% control-points)))

   (unweighted (the surface-in% control-points))
   
   (scaled (if (every #'(lambda(value) (= value 1))
                      (list (the scale-x) (the scale-y) (the scale-z)))
               (the unweighted)
             (mapcar #'(lambda(row)
                         (mapcar #'(lambda(point)
                                     (let ((local (the from-orientation-object (global-to-local point))))
                                       (the from-orientation-object 
                                         (local-to-global
                                          (make-point (* (get-x local) (the scale-x))
                                                      (* (get-y local) (the scale-y))
                                                      (* (get-z local) (the scale-z)))))))
                                 row))
                     (the unweighted))))
   
   
   (translated (if (the translated?)
                   (mapcar #'(lambda(row)
                               (mapcar #'(lambda(point) (add-vectors point (the translation)))
                                       row))
                           (the scaled))
                 (the scaled)))
   
   (oriented (if (the oriented?)
                 (mapcar #'(lambda(row)
                             (mapcar #'(lambda(point) (the to-orientation-object 
                                                        (local-to-global 
                                                         (the from-orientation-object (global-to-local point)))))
                                     row))
                         (the translated))
               (the translated)))

   
   (restored (if (the translated?)
                 (mapcar #'(lambda(row)
                             (mapcar #'(lambda(point)
                                         (add-vectors point (subtract-vectors (the orientation-center)
                                                                              (the from-center))))
                                     row))
                         (the oriented))
               (the oriented)))
   
   
   #+nil
   (control-points (if (the surface-in% rational?)
                       (mapcar 
                        #'(lambda(point-row weight-row)
                            (mapcar #'(lambda(point weight)
                                        (scalar*vector weight point))
                                    point-row weight-row)) (the %control-points) (the weights))
                     (the %control-points)))
   
   (control-points (the %control-points))

   (%control-points (if (the moved?)
                        (mapcar #'(lambda(row)
                                    (mapcar #'(lambda(point)
                                                (add-vectors point (subtract-vectors (the center) 
                                                                                     (or (the orientation-center)
                                                                                         (the from-center)))))
                                            row))
                                (the restored))
                      (the restored)))

   (outline-objects (cons (the surface)
                          (append (when (the show-box?) (list (the box)))
                                  (when (the show-tight-box?) (list (the tight-box)))
                                  (when (the show-control-polygon?) (list (the control-polygon)))))))

  
  :hidden-objects 
  ((surface :type 'surface :native-surface (the native-surface))
   
   ;;(box :type 'box)
   
   
   (control-polygon :type 'global-polyline
                    :display-controls (list :color :green)
                    :vertex-list (the b-spline-data))
   
   (from-orientation-object :type 'base-object
                            :center (the from-center)
                            :orientation (the from-orientation))
   (to-orientation-object :type 'base-object
                          :center (the from-center))))





(define-object transformed-bspline-mixin ()

  :input-slots
  ((from-location (the center))
   (to-location (the from-location))
   (to-orientation (the from-orientation))
   
   (center (the to-location))
   
   (scale 1)
   (scale-x (the scale))
   (scale-y (the scale))
   (scale-z (the scale)))

  
  :computed-slots
  ((control-points (the transformed-control-points))
   (weights (getf (the b-spline-list) :weights))
   (scaling-needed? (some #'(lambda(value) (/= value 1)) (list (the scale-x) (the scale-y) (the scale-z)))))
   
  
  :hidden-objects
  ((from-coord-space :type 'base-object
                     :orientation (or (the from-orientation) geom-base::+identity-3x3+)
                     :center (the from-location))
   
   (to-coord-space :type 'base-object
                   :orientation (or (the to-orientation) geom-base::+identity-3x3+)
                   :center (the to-location))))
  

(define-object transformed-curve (curve)
  
  :input-slots (curve-in
                (from-location (the curve-in center))
                (to-location (the from-location))
                (from-orientation (the curve-in orientation))
                (to-orientation (the from-orientation))
                (scale 1)
                (scale-x (the scale))
                (scale-y (the scale))
                (scale-z (the scale))
                (center (the curve-in center)))
                
  
  :computed-slots
  (
    
   (orientation (the to-orientation))

   
   (composed? nil ;;(> (the decomposed curves number-of-elements) 1)
              )
   
   (built-from (if (the composed?) (the composed-curve) (the transformed first))))
  
  
  :hidden-objects
  ((decomposed :type 'decomposed-curves
               :curve-in (the curve-in))
   
   (composed-curve :type (if (the composed?) 'composed-curve 'null-part)
                   :curves (list-elements (the transformed)))
   
   (transformed :type 'transformed-curve*
                :sequence (:size 1) ;;(:size (the decomposed curves number-of-elements))
                :curve-in (the curve-in) ;;(the decomposed (curves (the-child index)))
                :pass-down (curve-in from-location
                                     to-location from-orientation to-orientation
                                     scale scale-x scale-y scale-z))))


(define-object transformed-curve* (transformed-bspline-mixin b-spline-curve)

  :input-slots
  (curve-in
   (from-orientation (the curve-in orientation)))

  
  :computed-slots
  (
   (b-spline-list (multiple-value-bind (control-points weights knots  degree)
                      (the curve-in b-spline-data)
                    (list :control-points control-points  
                          :weights weights :knots knots :degree degree)))

   (knot-vector (getf (the b-spline-list) :knots))
   
   (degree (getf (the b-spline-list) :degree))
   
   (transformed-control-points (if (the curve-in rational?)
                                   (mapcar #'(lambda(point weight)
                                               (scalar*vector weight point))
                                           (the %transformed-control-points)
                                           (the weights))
                                 (the %transformed-control-points)))
   
   (unweighted-points (if (the curve-in rational?)
                          (mapcar #'(lambda(point weight)
                                      (scalar*vector (/ weight) point))
                                  (getf (the b-spline-list) :control-points)
                                  (the weights))
                        (getf (the b-spline-list) :control-points)))
   
   (%transformed-control-points 
    (mapcar #'(lambda(point)
                (let ((transformed
                       (the to-coord-space 
                         (local-to-global 
                          (the from-coord-space (global-to-local point))))))
                  (if (the scaling-needed?)
                      (let ((local (the to-coord-space (global-to-local transformed))))
                        (the to-coord-space (local-to-global 
                                             (make-point (* (get-x local) (the scale-x))
                                                         (* (get-y local) (the scale-y))
                                                         (* (get-z local) (the scale-z))))))
                    transformed)))
            (the unweighted-points)))))



  
  
  
  
    
