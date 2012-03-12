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


(defun pin-value-to-range (value min max &key (lt-test #'<) (gt-test #'>))
  "Number. Returns a value pinned to the range described by the min and max. 
If the input value is out of range, a warning is issued.

:args (value \"Input value\"
       min \"minimum end of allowed range\"
       max \"maximum end of allowed range\")

:&key ((lt-test #'&lt;) \"The test applied for less-than comparison\"
       (gt-test #'&gt;) \"The test applied for greater-than comparison\")
"
  (cond ((funcall lt-test value min)
         (warn "Value ~a is less than minimum allowed ~a, pinning to ~a.~%" 
               value min min)
         min)
        ((funcall gt-test value max)
         (warn "Value ~a is greater than maximum allowed ~a, pinning to ~a.~%" 
               value max max)
         max)
        (t value)))

;;
;; FLAG - move into gdl-base module.
;;
(defmacro with-pinned-values ((&rest bindings) &body body)
  "Macro. Takes a list of bindings and body similar to let, but each binding
contains a variable, which must already exist in this context, followed by a 
min and a max value to which its inner binding will be pinned. If the 
value of the outer binding falls outside the range, a warning will be 
issued."
  `(let (,@(mapcar #'(lambda(binding)
                       `(,(first binding) (pin-value-to-range ,(first binding) 
                                                              ,(second binding) 
                                                              ,(third binding))))
                   bindings))
     ,@body))

(define-object surface (geometry-kernel-object-mixin 
                        outline-specialization-mixin 
                        base-object)
  
  :documentation (:description "A generalized NURBS surface. 
Usually used as a mixin in more specific surfaces.")
  
  :input-slots
  (("GDL Surface. Specify this if you want this surface to be a 
clone of an existing surface (note - this uses a shared underlying 
surface object, it does not make a copy)"
    built-from nil)
   
   
   (%schedule-finalization?% t)
   
   (native-surface (get-nurbs-from-surface *geometry-kernel* (the native-surface-iw)))
   
   (native-pointer nil)
   
   (native-surface-iw 
    (or (the native-pointer)
        (cond ((the built-from) (the built-from native-surface-iw))
              ((null (the native-surface)) 
               (error "No native-surface instantiated"))
              (t (make-b-spline-surface* 
                  *geometry-kernel*
                  (the native-surface)
                  :schedule-finalization? (the %schedule-finalization?%))))))
   
   ("Number. Approximation tolerance for display purposes."
    tolerance *display-tolerance*)
   
   ("Number. Overall tolerance for the internal brep representation of this surface.
Defaults to nil. Note that a value of nil indicates for SMLib a value of 1.0e-05 of
the longest diagonal length of the brep."
    brep-tolerance nil)
   
   (parameterization 4)
   
   (nonrational-degree 3)
   
   ("Plist with keys :n-u and :n-v. The number of isoparametric 
curves to be displayed in each direction. This value comes from the 
value of :isos on the display-controls if that exists, and defaults 
to *isos-default* otherwise."
    isos (getf (the display-controls) :isos *isos-default*) :defaulting)
   
   
   ("Boolean. Indicates whether to attempt automatic endcaps on conversion 
of this surface to a brep. 
Note that this might change in future to a keyword value 
for :min, :max, or :both to provide more control." 
    end-caps-on-brep? nil)

   ("Boolean. Indicates whether brep representation should undergo a 
sew-and-orient operation. Defaults to nil."
    sew-and-orient-brep? nil)
   

   ("Plist of keyword symbols and numbers. This controls tessellation for this brep.
The keys are as follows:
<tt><ul>
          <li>:min-number-of-segments</li>
          <li>:max-3d-edge-factor</li>
          <li>:min-parametric-ratio</li>
          <li>:max-chord-height</li>
          <li>:max-angle-degrees</li>
          <li>:min-3d-edge</li>
          <li>:min-edge-ratio-uv</li>
          <li>:max-aspect-ratio</li>
</ul></tt>

and the defaults come from the following parameters:

 <tt>

 (list :min-number-of-segments *tess-min-number-of-segments*
       :max-3d-edge-factor *tess-max-3d-edge-factor*
       :min-parametric-ratio *tess-min-parametric-ratio*
       :max-chord-height *tess-max-chord-height*
       :max-angle-degrees *tess-max-angle-degrees*
       :min-3d-edge *tess-min-3d-edge*
       :min-edge-ratio-uv *tess-min-edge-ratio-uv*
       :max-aspect-ratio *tess-max-aspect-ratio*)

 </tt>

"
    tessellation-parameters
    
    (list :min-number-of-segments *tess-min-number-of-segments*
          :max-3d-edge-factor *tess-max-3d-edge-factor*
          :min-parametric-ratio *tess-min-parametric-ratio*
          :max-chord-height *tess-max-chord-height*
          :max-angle-degrees *tess-max-angle-degrees*
          :min-3d-edge *tess-min-3d-edge*
          :min-edge-ratio-uv *tess-min-edge-ratio-uv*
          :max-aspect-ratio *tess-max-aspect-ratio*))

   
   
   ("Boolean. Returns non-nil iff this surface is rational, i.e. all weights are not 1." 
    rational? (not (every #'(lambda(weight) (near-to? weight 1.0)) 
                          (apply #'append (the weights))))))

   
  
  :computed-slots
  (
   ;;
   ;; FLAG -- make sure this doesn't step on b-spline-surface inputs
   ;;
   (b-spline-data-list (multiple-value-list (the b-spline-data)))
   
   (control-points (first (the b-spline-data-list)))
   (weights (second (the b-spline-data-list)))
   (u-knot-vector (third (the b-spline-data-list)))
   (v-knot-vector (fourth (the b-spline-data-list)))
   (u-degree (fifth (the b-spline-data-list)))
   (v-degree (sixth (the b-spline-data-list)))

   ;;
   ;;
      
   (basis-surface self)
   
   (iso-curves (list :u (get-iso-curves *geometry-kernel* self 
                                        :u (getf (the isos) :n-u))
                     :v (get-iso-curves *geometry-kernel* self 
                                        :v (or (getf (the isos) :n-v)))))
   

   (bounding-box (bounding-box-from-points 
                  (apply #'append 
                         (apply #'append (mapsend (the outline-objects) 
                                                  :%curves-to-draw%)))))

   
   (control-points-local (mapcar #'(lambda(list) 
                                     (mapcar #'(lambda(point) 
                                                 (the (global-to-local point))) list)) 
                                 (the b-spline-data)))
   
   (width (the bounding-bbox width))
   (height (the bounding-bbox height))
   (length (the bounding-bbox length))
   
   (%renderer-info% (list :vrml? t :view-default :trimetric))
   
   
   (%control-points% (the b-spline-data))
   (%control-points-transposed% (apply #'mapcar #'list (the b-spline-data)))
   
   (outline-objects (append (when (or (eql (getf (the display-controls) :isos :no) :no)
                                      (getf (getf (the display-controls) :isos) :n-u))
                              (list-elements (the u-iso-curves)))
                            (when (or (eql (getf (the display-controls) :isos :no) :no)
                                      (getf (getf (the display-controls) :isos) :n-v))
                              (list-elements (the v-iso-curves)))
                            (when (getf (the display-controls) :bezier-points)
                              (append (list-elements (the control-grid))))))
   
   (min-max-x-y-z (the %min-max-x-y-z%)))
  
  
  :hidden-objects
  ((control-grid :type 'points-display 
                 :sequence (:size (length (the %control-points%)))
                 :points (nth (the-child index)(the %control-points%)))
   
   (control-grid-u :type 'global-polyline
                   :sequence (:size (length (the %control-points%)))
                   :vertex-list (the (control-grid (the-child index)) points))
   
   (control-grid-v :type 'global-polyline
                   :sequence (:size (length (the %control-points-transposed%)))
                   :vertex-list (nth (the-child index) (the %control-points-transposed%)))
   
   
   ("Sequence of curve objects. The isoparametric curves in the U direction."
    u-iso-curves :type 'curve
                 :sequence (:size (length (getf (the iso-curves) :u)))
                 :curve-list (getf (the iso-curves) :u)
                 :pseudo-inputs (curve-list)
                 :native-curve (nth (the-child index) (the-child curve-list)))
   
   ("Sequence of curve objects. The isoparametric curves in the V direction."
    v-iso-curves :type 'curve
                 :sequence (:size (length (getf (the iso-curves) :v)))
                 :curve-list (getf (the iso-curves) :v)
                 :pseudo-inputs (curve-list)
                 :native-curve (nth (the-child index) (the-child curve-list)))
   
   
   (test-copy :type 'b-spline-surface 
              :parameters (multiple-value-bind (control-points weights u-knots 
                                                v-knots u-degree v-degree)
                              (the b-spline-data)
                            (list :control-points control-points
                                  :weights weights
                                  :u-knot-vector u-knots
                                  :v-knot-vector v-knots
                                  :u-degree u-degree
                                  :v-degree v-degree)))
               
   

   ("GDL Brep object. This is the brep representation of this surface."
    brep :type 'brep-from-surface 
    :surface (multiple-value-bind (control-points weights u-knots v-knots 
                                   u-degree v-degree)
                 (the b-spline-data)
               (make-object 'b-spline-surface :control-points control-points 
                            :weights weights 
                                 :u-knot-vector u-knots :v-knot-vector v-knots 
                                 :u-degree u-degree :v-degree v-degree 
                                 :%schedule-finalization?% t))
         :tessellation-parameters (the tessellation-parameters)
                                 
         :pass-down (brep-tolerance sew-and-orient-brep?)
         :end-caps? (the end-caps-on-brep?))
   
   ("GDL surface object. This surface object swaps the role of u and v- directional parameters, i.e. old-surface(u,v) = new-surface(v,u)."    
    swapped-uv-surface :type 'surface
                       :native-surface (let ((copy (the fresh-copy)))
                                         (funcall (read-from-string "smlib::surface-swap-uv")
                                                  (the-object copy native-surface)))))
  :functions
  ((%min-max-x-y-z%
    (&optional (tolerance *zero-epsilon*))
    (flet ((min-max (accessor comparison)
             (let (current-u)
               (let ((v 
                      (min-max-search 
                       #'(lambda(v-param)
                           (let ((u-param 
                                  (min-max-search 
                                   #'(lambda(u-param)
                                       (funcall accessor (the (point u-param v-param))))
                                   comparison
                                   (the u-min) (the u-max) tolerance)))
                             (setq current-u u-param)
                             (funcall accessor (the (point u-param v-param)))))
                                      comparison (the v-min) (the v-max) tolerance)))
                 (list current-u v)))))
      (let ((min-x (min-max #'get-x #'<))
            (max-x (min-max #'get-x #'>))
            (min-y (min-max #'get-y #'<))
            (max-y (min-max #'get-y #'>))
            (min-z (min-max #'get-z #'<))
            (max-z (min-max #'get-z #'>)))
        (list :min-x (the (point (first min-x) (second min-x))) :min-x-params min-x
              :max-x (the (point (first max-x) (second max-x))) :max-x-params max-x
              :min-y (the (point (first min-y) (second min-y))) :min-y-params min-y
              :max-y (the (point (first max-y) (second max-y))) :max-y-params max-y
              :min-z (the (point (first min-z) (second min-z))) :min-z-params min-z
              :max-z (the (point (first max-z) (second max-z))) :max-z-params max-z))))
   
   
   (fresh-copy ()
               (multiple-value-bind 
                   (control-points weights u-knots 
                    v-knots u-degree v-degree) (the b-spline-data)
                 (make-object 'b-spline-surface
                              :control-points control-points
                              :weights weights
                              :u-knot-vector u-knots
                              :v-knot-vector v-knots
                              :u-degree u-degree
                              :v-degree v-degree)))
   
   (write-stl-file
    (file-name &key (format :ascii))
    (the brep (write-stl-file file-name :format format)))

   
   ("Number. Returns the area of the surface.

:&key ((desired-accuracy 0.000001) \"Number. Desired accuracy of result. Should not be smaller than 0.00000001 (10e-08).\")"
    area 
    (&key (desired-accuracy 0.000001))
    (surface-area *geometry-kernel* (the native-surface-iw) desired-accuracy))
   
   #+allegro
   ("List of Surface points. Returns the given 3D point dropped normally to this surface.

:arguments (point \"3D Point. The point to be dropped.\")
:&key  ((3d-tolerance *3d-tolerance-default*) \"The tolerance used for dropping.\")"
    dropped-points
    (point &key (3d-tolerance *3d-tolerance-default*))
    (let ((uv-pairs (surface-drop-point *geometry-kernel* self point 
                                        :3d-tolerance 3d-tolerance)))
      (mapcar #'(lambda(uv-pair) 
                  (make-surface-point :uv-point (make-point (first uv-pair) 
                                                            (second uv-pair))
                                      :3d-point (the (point (first uv-pair) 
                                                            (second uv-pair))))) 
              uv-pairs)))
   
   
   ("Returns a bbox object, answering xmin, ymin, zmin, xmax, ymax, and zmax, 
for a box containing the convex hull (i.e. the control points) of this 
surface and oriented according to the given <tt>center</tt> and <tt>orientation</tt>."
    local-bounding-box
    (center orientation)
    (let* ((object (make-object 'base-object :center center :orientation orientation))
           (points (mapcar #'(lambda(point) (the-object object (global-to-local point))) 
                           (apply #'append (the b-spline-data)))))
      (let ((xmax (apply #'max (mapcar #'get-x points)))
            (ymax (apply #'max (mapcar #'get-y points)))
            (zmax (apply #'max (mapcar #'get-z points)))
            (xmin (apply #'min (mapcar #'get-x points)))
            (ymin (apply #'min (mapcar #'get-y points)))
            (zmin (apply #'min (mapcar #'get-z points))))
        (make-object 'bbox :xmax xmax :ymax ymax :zmax zmax 
                     :xmin xmin :ymin ymin :zmin zmin))))
   
   #+allegro
   ("Surface point. Returns the given 3D point dropped normally to this 
surface, as close as possible to the given 3D point.

:arguments (point \"3D Point. The point to be dropped.\")
:&key  ((3d-tolerance *3d-tolerance-default*) \"The tolerance used for dropping.\")"
    dropped-point
    (point &key (3d-tolerance *3d-tolerance-default*))
    (let ((uv-pairs (surface-drop-point *geometry-kernel* self point 
                                        :3d-tolerance 3d-tolerance)))
      (first (sort
              (mapcar #'(lambda(uv-pair) 
                          (make-surface-point 
                           :uv-point (make-point (first uv-pair) (second uv-pair))
                           :3d-point (the (point (first uv-pair) (second uv-pair))))) 
                      uv-pairs)
              #'(lambda(p1 p2)
                  (< (3d-distance (get-3d-point-of p1) point) 
                     (3d-distance (get-3d-point-of p2) point)))))))
   
   

   ("Plist. The returned plist contains information about the minimum 
distance from this surface to the curve given as the argument.

:arguments (curve \"GDL Curve object\")"
    minimum-distance-to-curve
    (curve)
    (the (curve-solve curve :minimize)))


   #+allegro   
   ("Plist. The returned plist contains information about the maximum 
distance from this surface to the curve given as the argument.

:arguments (curve \"GDL Curve object\")"
    maximum-distance-to-curve
    (curve)
    (the (curve-solve curve :maximize)))
   
   (curve-solve
    (curve operation)
    (let ((result 
           (first 
            (global-surface-curve-solve *geometry-kernel* self 
                                        :curve curve :operation-type operation))))
      (when result
        (destructuring-bind (parameter surface-u surface-v)  result
          (let ((point-on-curve (when parameter (make-surface-point 
                                                 :parameter parameter
                                                 :3d-point (the-object curve 
                                                                       (point parameter)))))
                (point-on-surface (when (and surface-u surface-v) 
                                    (make-surface-point 
                                     :uv-point (make-point surface-u surface-v)
                                     :3d-point (the (point surface-u surface-v))))))
            
            (let ((distance (when (and point-on-curve point-on-surface)
                              (3d-distance (get-3d-point-of point-on-curve) 
                                           (get-3d-point-of point-on-surface)))))
              (list :point-on-curve point-on-curve 
                    :point-on-surface point-on-surface :distance distance)))))))

   
   ("Plist. The returned plist contains information about the minimum 
distance from this surface to the surface given as the argument.

:arguments (surface \"GDL Surface object\")"
    minimum-distance-to-surface
    (surface)
    (the (surface-solve surface :minimize)))
   

   #+allegro
   ("Plist. The returned plist contains information about the maximum 
distance from this surface to the surface given as the argument.

:arguments (surface \"GDL Surface object\")"
    maximum-distance-to-surface
    (surface)
    (the (surface-solve surface :maximize)))
   
   (surface-solve
    (surface operation)
    (destructuring-bind (surface-u surface-v other-surface-u other-surface-v) 
        (first (global-surface-surface-solve *geometry-kernel* self 
                                             :other-surface surface 
                                             :operation-type operation))
      (let ((point-on-surface 
             (when (and surface-u surface-v) 
               (make-surface-point :uv-point (make-point surface-u surface-v)
                                   :3d-point (the (point surface-u surface-v)))))
            (point-on-other-surface 
             (when (and other-surface-u other-surface-v) 
               (make-surface-point 
                :uv-point (make-point other-surface-u other-surface-v)
                :3d-point (the-object surface (point other-surface-u other-surface-v))))))
        (let ((distance (when (and point-on-surface point-on-other-surface)
                          (3d-distance (get-3d-point-of point-on-surface) 
                                       (get-3d-point-of point-on-other-surface)))))
          (list :point-on-surface point-on-surface 
                :point-on-other-surface point-on-other-surface :distance distance)))))
   
   
   ("3D Point. Returns the surface point at the given parameters offset 
along the surface normal at that point by the given distance.
:arguments (u \"Number. The U parameter for the surface point.\"
            v \"Number. The V parameter for the surface point.\"
            distance \"Number. The distance for offsetting.\")"
    offset-point
    (u v distance)
    (let ((point (the (point u v))) (normal (the (normal u v))))
      (translate-along-vector point normal distance)))
   
   ("Number. Returns the Gaussian curvature on the surface at the given 
parameter values. Three additional values are returned, which are the Normal 
Curvature at the point, the first Fundamental Principle Curvature, and the 
second Fundamental Principle Curvature.

:note This function might be updated to provide a clearer idea of 
actual radius of curvature; at the time of this writing it is 
      not clear what the relationship is between the four returned 
values and actual radius of curvature.

:arguments (u \"Number. The U parameter for the surface point.\"
            v \"Number. The V parameter for the surface point.\")"
    radius-of-curvature
    (u v)
    (surface-evaluate-geometric *geometry-kernel* self u v))
   
   
   ("List of Surface points. Returns the given point projected along 
the given vector intersected with the surface.

:arguments (point \"3D Point. The point to be projected.\"
            vector \"3D Vector. The vector along which to project.\")"
    
    projected-points
    (point vector)
    (let ((parameter-pairs (surface-global-line-intersect *geometry-kernel* 
                                                          self point vector)))
      (mapcar 
       #'(lambda(parameter-pair)
           (make-surface-point 
            :uv-point (make-point (first parameter-pair) (second parameter-pair))
            :3d-point (the (point (first parameter-pair) (second parameter-pair))))) 
       parameter-pairs)))
   
   ("Surface point. Returns the first result of the given point projected 
along the given vector intersected with the surface.

:arguments (point \"3D Point. The point to be projected.\"
            vector \"3D Vector. The vector along which to project.\")"
    
    projected-point
    (point vector)
    (let ((parameter-pair (first (surface-global-line-intersect *geometry-kernel* 
                                                                self point vector))))
      (make-surface-point :uv-point (make-point (first parameter-pair) 
                                                (second parameter-pair))
                          :3d-point (the (point (first parameter-pair) 
                                                (second parameter-pair))))))
   
   
   ("Plist. Returns plist containing :min and :max indicating min and max UV points 
for parameter space for this surface."
    domain
    ()
    (surface-get-domain-components *geometry-kernel* self))
   
   
   ("Number. Returns minimum U component of the surface parameter space."
    u-min 
    ()
    (get-x (getf (the domain) :min)))
   
   ("Number. Returns minimum V component of the surface parameter space."
    v-min 
    ()
    (get-y (getf (the domain) :min)))
   
   ("Number. Returns maximum U component of the surface parameter space."
    u-max
    ()
    (get-x (getf (the domain) :max)))
   
   ("Number. Returns maximum V component of the surface parameter space."
    v-max
    ()
    (get-y (getf (the domain) :max)))
   
   ("Boolean. Returns non-nil if the given UV (2D) point lies within the 
parameter space of this surface. Currently
this function works only on the basis surface ; it does not observe 
trimming island or holes."
    on?
    (uv-point)
    (and (> (the u-max) (get-x uv-point) (the u-min))
         (> (the v-max) (get-y uv-point) (the v-min))))
   
   
   ("List of lists 3D points, List of lists numbers, List of numbers, 
List of numbers, Integer, and Integer. 
Returns six values which are the control points, the weights, the u-knots, 
the v-knots, the u-degree, and the v-degree of the surface."
    b-spline-data
    ()
    (get-surface-b-spline-data *geometry-kernel* (the native-surface)))
   
   
   ("3D Point. The point on the surface corresponding to the given u and v 
parameter values."
    point
    (u v)
    (%get-point-on-surface *geometry-kernel* (the native-surface) u v))
   
      
   ("3D Vector. The surface normal vector at the given u and v values. 
Three other values are also returned:
The 3D point, the U tangent, and the V tangent at the given parameter value."
    normal
    (u v)
    (with-pinned-values ((u (the u-min) (the u-max))
                         (v (the v-min) (the v-max)))
      (get-surface-normal *geometry-kernel* (the native-surface) u v)))
   
   #+allegro
   ("Surface point. Returns the first point of intersection between 
this surface and the curve given as an argument.

:arguments (curve \"GDL Curve object. The curve to intersect with this surface.\")
:&key ((3d-tolerance *3d-tolerance-default*) \"Number. The tolerance to 
use for intersecting.\")"
    curve-intersection-point
    (curve &key (3d-tolerance *3d-tolerance-default*)) 
    (let ((triplet (first (get-surface-curve-intersection-points 
                           *geometry-kernel* self curve :3d-tolerance 3d-tolerance))))
      (when triplet (make-surface-point 
                     :parameter (first triplet)
                     :uv-point (make-point (second triplet) (third triplet))
                     :3d-point (the (point (second triplet) (third triplet)))))))

   
   #+allegro
   ("List of Surface points. Returns the point(s) of intersection between 
this surface and the curve given as an argument.

:arguments (curve \"GDL Curve object. The curve to intersect with this surface.\")
:&key ((3d-tolerance *3d-tolerance-default*) \"Number. The tolerance 
to use for intersecting.\")"
    curve-intersection-points
    (curve &key (3d-tolerance *3d-tolerance-default*)) 
    (let ((triplets (get-surface-curve-intersection-points 
                     *geometry-kernel* self curve :3d-tolerance 3d-tolerance)))
      (mapcar #'(lambda(triplet)
                  (make-surface-point 
                   :parameter (first triplet)
                   :uv-point (make-point (second triplet) (third triplet))
                   :3d-point (the (point (second triplet) (third triplet))))) 
              triplets)))))


(define-object brep-from-surface (brep)
  
  :input-slots (surface (end-caps? nil)
                sew-and-orient-brep?)
  
  :computed-slots 
  ((%native-brep% 
    (let ((brep (make-brep *geometry-kernel* :tolerance (the brep-tolerance))))
      (make-faces-from-surface *geometry-kernel* brep (the surface) 
                               :continuity-type :c0
                               :end-caps? (the end-caps?))


      (when (the sew-and-orient-brep?)
        (let ((status (iwbrep-sew-and-orient brep)))
          (format t "sew-and-orient status is: ~a.~%" status)))

      (unless (the brep-tolerance)
        (brep-reset-tolerance *geometry-kernel* brep))
                    
      brep))))

                       
  
  
  

