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


(defstruct surface-point 
  (parameter 0.0 :type double-float)
  (other-parameter 0.0 :type double-float)
  (uv-point nil #+allegro :type #+allegro (vector double-float))
  (3d-point nil #+allegro :type #+allegro (vector double-float)))

(defmethod get-parameter-of ((surface-point surface-point))
  "Number. Returns the parameter component of a surface point."
  (surface-point-parameter surface-point))

(defmethod get-parameter-of ((surface-point t))
  "Number. Returns the parameter component of a surface point." nil)

(defmethod get-other-parameter-of ((surface-point surface-point))
  "Number. Returns the other parameter component of a surface point."
  (surface-point-other-parameter surface-point))

(defmethod get-other-parameter-of ((surface-point t))
  "Number. Returns the other parameter component of a surface point." nil)

(defmethod get-3d-point-of ((surface-point surface-point))
  "3D Point. Returns the 3D point component of a surface point."
  (surface-point-3d-point surface-point))

(defmethod get-3d-point-of ((surface-point t))
  "3D Point. Returns the 3D point component of a surface point." nil)

(defmethod get-uv-point-of ((surface-point surface-point))
  "UV Point. Returns the UV (2D) point component of a surface point."
  (surface-point-uv-point surface-point))

(defmethod get-uv-point-of ((surface-point t))
  "UV Point. Returns the UV (2D) point component of a surface point." nil)


(defun get-point-on-surface (plist)
  "3D point. Returns the point found from a minimum or maximum distance computation involving a surface."
  (getf plist :point-on-surface))

(defun get-point-on-curve (plist)
  "3D point. Returns the point found from a minimum or maximum distance computation involving a curve."
  (getf plist :point-on-curve))

(defun get-point-on-other-curve (plist)
  "3D point. Returns the other point found from a minimum or maximum distance computation involving a curve."
  (getf plist :point-on-other-curve))
  



(define-object curve (geometry-kernel-object-mixin base-object)
  
  :documentation (:description "A generalized NURBS curve. Usually used as a mixin in more specific curves."
                  
                  :examples "<pre>
 
 (in-package :surf)

 (define-object test-curve (curve)
  
  :input-slots
  ((built-from (the curv-in)))
  
  :computed-slots
   ((control-pts (list (make-point 3 5 1)
                       (make-point 5 8.0 1) 
                       (make-point 7 10.0 1) 
                       (make-point 8 5.0 1) 
                       (make-point 7 0.0 1) 
                       (make-point 5 0.0 1) 
                       (make-point 3 5 1))))
   
   :hidden-objects
    ((curv-in  :type 'b-spline-curve
              :control-points (the control-pts))))

  (generate-sample-drawing :object-roots (make-object 'test-curve)
                         :projection-direction :top)
  </pre>")
  
  :input-slots
  ((%decomposed?% nil)
   (%copy?% nil)
   (%reversed?% nil)

   (native-curve (when (the native-curve-iw)
                   (get-nurbs-from-curve *geometry-kernel* (the native-curve-iw))))
   
   
   ("GDL Curve object. The corresponding UV curve for the primary surface on which this curve lies, if any. If
this is not a surface-curve, this will return an error."
    uv-curve (error "This curve is not on any surface"))   
   
   (native-curve-iw (cond ((the built-from) (the built-from copy-new native-curve-iw))
                          ((null (the native-curve)) (error "No native-curve instantiated"))
                          (t (make-b-spline-curve* *geometry-kernel* (the native-curve) 
                                                   :schedule-finalization? t))))
   
   ("GDL Curve. Specify this if you want this curve to be a clone of
an existing curve. (note - this uses a shared underlying curve object,
it does not make a copy)" built-from nil)

   ;;
   ;; FLAG -- made this not be :defaulting anymore because tolerance
   ;; is not declared as trickle-down anywhere.
   ;;
   #+nil
   ("Number. Approximation tolerance for display purposes. Defaults to the tolerance of the 
built-from curve, if one exists, otherwise defaults to the *display-tolerance*."
    tolerance (if (the built-from) (the built-from tolerance) *display-tolerance*) :settable :defaulting)
   
   ("Number. Approximation tolerance for display purposes. Defaults to the tolerance of the 
built-from curve, if one exists, otherwise defaults to the *display-tolerance*."
    tolerance (if (the built-from) (the built-from tolerance) *display-tolerance*) :settable)
   
   (surf nil)
   
   (parameterization :inherited :settable)
   
   (nonrational-degree 3)
   
   
   (box-width (abs (- (get-x (first (the  bounding-box))) (get-x (second (the  bounding-box))))))
   (box-length (abs (- (get-y (first (the bounding-box))) (get-y (second (the  bounding-box))))))
   (box-height (abs (- (get-z (first (the bounding-box))) (get-z (second (the  bounding-box))))))
   
   (tolerance-for-native-beziers (let ((tolerance (or (the tolerance)
                                                      *display-tolerance*))
                                       (adaptive1 (/ (the measure) 10.0))
                                       (adaptive2 (* (the measure) 10.0))
                                       (linear? (equal 2 (length (the weights)))))
                                   (cond (linear? tolerance)
                                         ((> (the measure) 100) adaptive1)
                                         ((< (the measure) .01) adaptive2)
                                         (t tolerance)))))

  
  :computed-slots
  ((measure (let ((tolerance (or (the tolerance)
                                 *display-tolerance*)))
              (* tolerance (curve-extent *geometry-kernel* (the native-curve)))))

   
   (b-spline-data-list (multiple-value-list (the b-spline-data)))
   
   (control-points (first (the b-spline-data-list)))
   (weights (second (the b-spline-data-list)))
   (knot-vector (third (the b-spline-data-list)))
   (degree (fourth (the b-spline-data-list)))

   
   (rational? (when (the weights)
                (not (every #'(lambda(weight) (near-to? weight 1.0)) (the weights)))))

   
   ("List of GDL surfaces. The surfaces on which this curve lies." on-surfaces nil)
   
   (uv-curves nil)

   
   ("Number. Equal to the natural <tt>u-min</tt> of the curve."
    u1 (the u-min))
   
   ("Number. Equal to the natural <tt>u-max</tt> of the curve."
    u2 (the u-max))
   

   (native-beziers 
    (let ((beziers (with-error-handling (:error? t :timeout *nurbs-to-beziers-timeout*)
                     (let ((native-curve (the native-curve))
                           (tolerance (when (or (the rational?) (/= (the degree) 3))
                                        (the tolerance-for-native-beziers)))
                           (nonrational-degree (the nonrational-degree))
                           (parameterization (the parameterization))
                           (maintain-end-tangents? nil))
                       (nurbs-to-beziers *geometry-kernel* native-curve tolerance nonrational-degree 
                                         parameterization maintain-end-tangents? self (the rational?)
                                         :control-points-only? t))))) 
      (make-array (length beziers) :initial-contents beziers)))
   

   (%lines-to-draw% (cond ((and (= (the degree) 1) (not (the rational?)))
			   (let ((control-points (the b-spline-data)))
			     (mapcar #'(lambda(start end)
					 (list start end))
				     control-points
				     (rest control-points))))
                          (*curve-tessellation?*
                           (let ((points (the (equi-spaced-points 
                                               (max 2 (floor (* (the total-length) )))))))
                             (mapcar #'list points (rest points))))
                          (t nil)))

   
   (%curves-to-draw%
    (unless (or *curve-tessellation?*
                (near-to? (the total-length) 0))
      (if (typep self 'arc-curve)
	  (call-next-method)
	  (if (= (the %decomposed% curves number-of-elements) 1)
	      (unless (and (= (the degree) 1) (not (the rational?)))
		(when (list-elements (the beziers))
		  (let (result)
		    (dolist (bezier 
                              (if t ;;*chain-beziers-for-display?*
                                  (chain-nurbs-curves (list-elements (the beziers)))
				  (list-elements (the beziers)))
			     result)
		      (if (null result)
			  (setq result (the-object bezier %curves-to-draw%))
			  (nconc result (the-object bezier %curves-to-draw%)))))))
	      (append-elements (the %decomposed% curves) 
			       (the-element %curves-to-draw%))))))
   
   


   ;;(closed? (coincident-point? (the start) (the end) :tolerance (the closed-tolerance)))
   (closed? (coincident-point? (the start) (the end)))
   
   ("Boolean. This will be t if the curve is generated successfully, nil otherwise."
    success? (when (the native-curve) t))
   
   ("Number. The lowest parameter value of the underlying mathematical definition for this parametric curve"
    u-min (multiple-value-bind (min max) (the parameter-bounds)
            (declare (ignore max)) min))
   
   ("Number. The highest parameter value of the underlying mathematical definition for this parametric curve"
    u-max (multiple-value-bind (min max) (the parameter-bounds)
            (declare (ignore min)) max))

   ("3D Point. The point returned by evaluating the curve function at <tt>u1</tt>."
    start (the (point (the u1))))
   
   ("3D Point. The point returned by evaluating the curve function at <tt>u2</tt>."
    end (the (point (the u2))))
   
   ("3D Vector. The direction pointing from the start to the end."
    direction-vector (subtract-vectors (the end) (the start)))
   
   (first-derivative-native-curve (get-curve-first-derivative *geometry-kernel* (the native-curve)))
   
   (second-derivative-native-curve (get-curve-second-derivative *geometry-kernel* (the native-curve)))
   
   ;;
   ;; FLAG -- use the smlib method for this.
   ;;
   (bounding-box (bounding-box-from-points (the control-points)))

   (control-points-local (mapcar #'(lambda(point) (the (global-to-local point))) (the b-spline-data)))
   
   (%renderer-info% (list :vrml? t :view-default :trimetric))
   
   
   (min-max-x-y-z (the %min-max-x-y-z%))
   
   
   
   (face-curves (list :top (make-object 'linear-curve
                                        :start (the tight-box (vertex :top :front :left))
                                        :end (the tight-box (vertex :top :rear :right)))
                      :bottom (make-object 'linear-curve
                                        :start (the tight-box (vertex :bottom :front :left)) 
                                        :end (the tight-box (vertex :bottom :rear :right)))

                      :left (make-object 'linear-curve
                                        :start (the tight-box (vertex :left :bottom :front))
                                        :end (the tight-box (vertex :left :top :rear)))

                      :right (make-object 'linear-curve
                                          :start (the tight-box (vertex :right :bottom :front))
                                          :end (the tight-box (vertex :right :top :rear)))

                      :rear (make-object 'linear-curve
                                         :start (the tight-box (vertex :rear :left :bottom))  
                                         :end (the tight-box (vertex :rear :right :top)))

                      :front (make-object 'linear-curve
                                          :start (the tight-box (vertex :front :left :bottom))  
                                          :end (the tight-box (vertex :front :right :top)))))
   
   ;;
   ;; FLAG -- get this from smlib.
   ;;
   (tight-box (the bounding-bbox))
   
   )

  
  :hidden-objects
  ((%decomposed% :type (if (the %decomposed?%) 'null-object 'decomposed-curves)
		 :curve-in self 
		 :tolerance (the tolerance)
		 :tolerance-for-native-beziers (the tolerance-for-native-beziers))

   
   ("GDL Curve. The first derivative of this curve. The degree will be one less than the degree of this curve."
    first-derivative :type (if (>= (the degree) 1) 'curve 'null-object) :native-curve (the first-derivative-native-curve))
   
   ("GDL Curve. The second derivative of this curve. The degree will be two less than the degree of this curve."
    second-derivative :type (if (>= (the degree) 2) 'curve 'null-object) :native-curve (the second-derivative-native-curve))

  
   (beziers :type 'bezier-curve
            :sequence (:size (length (the native-beziers)))
            :weights nil
            :knots nil
            :degree 3
            :control-points (let ((result (aref (the native-beziers) (the-child index))))
                              (ecase (length result)
                                (4 result)
                                (3 (list (first result) (second result) (third result) (third result)))
                                (2 (list (first result) (first result) (second result) (second result))))))

   (reverse :type (if (the %reversed?%) 'null-object 'curve)
            :native-curve (curve-reverse *geometry-kernel* (the native-curve))
	    :%reversed?% t)
   
   (copy :type (if (the %copy?%) 'null-object 'b-spline-curve)
	 :%copy?% t
         :data (multiple-value-list (the b-spline-data))
         :pseudo-inputs (data)
         :control-points (first (the-child data))
         :weights (second (the-child data))
         :knot-vector (third (the-child data))
         :degree (fourth (the-child data))
         
         ))
  
  :functions
  (
   (%min-max-x-y-z%
     (&optional (tolerance *zero-epsilon*))
    (flet ((min-max (accessor comparison)
             (min-max-search #'(lambda(param)
                                 (funcall accessor (the (point param))))
                             comparison (the u1) (the u2) tolerance)))
      (let ((min-x (min-max #'get-x #'<))
            (max-x (min-max #'get-x #'>))
            (min-y (min-max #'get-y #'<))
            (max-y (min-max #'get-y #'>))
            (min-z (min-max #'get-z #'<))
            (max-z (min-max #'get-z #'>)))
        (list :min-x (the (point min-x)) :min-x-param min-x
              :max-x (the (point max-x)) :max-x-param max-x
              :min-y (the (point min-y)) :min-y-param min-y
              :max-y (the (point max-y)) :max-y-param max-y
              :min-z (the (point min-z)) :min-z-param min-z
              :max-z (the (point max-z)) :max-z-param max-z))))
   
   
   ("Number. Returns the parameter of the given point on the curve.
     :arguments (point \"Point. The point of which the parameter should be calculated.\")"
     parameter-at-point 
     (3d-point)
     (get-parameter-of (getf (the (minimum-distance-to-point 3d-point)) :point-on-curve)))
   
   
   (copy-new
    (&key (finalize? t))
    (make-object 'curve :native-curve-iw (iw-copy-curve *geometry-kernel* (the native-curve-iw) 
                                                        :finalize? finalize?)))
   

   
   ("List of 3D points, List of numbers, List of numbers, and integer. Returns four values which are
the control points, the weights, the knots, and the degree of the curve."
    b-spline-data
    ()
    (get-curve-b-spline-data *geometry-kernel* (the native-curve)))
   
   
   
   ("3D Point. The point on the curve corresponding to the given parameter value.

:arguments (parameter \"Number. The desired parameter to be evaluated on the curve.\")"
    point
    (parameter)
    (when (< parameter (the u-min)) 
      (warn "~&Asked for point at parameter value ~a, but u-min of the curve is ~a, 
pinning to the u-min value.~%" parameter (the u-min)) (setq parameter (the u-min)))
    (when (> parameter (the u-max)) 
      (warn "~&Asked for point at parameter value ~a, but u-max of the curve is ~a, 
pinning to the u-max value.~%" parameter (the u-max)) (setq parameter (the u-max)))
    (%get-point-on-curve *geometry-kernel* (the native-curve) parameter))
   
   
   ("3D Vector. The curve tangent at the given parameter value. Supplementary values returned
are: the 3D point at the parameter value. If keyword argument <tt>:include-curvature?</tt> is given as non-NIL, 
the radius of the osculating circle, the center for the osculating circle, the normal for the osculating circle, 
and the curve normal are also returned. Note: If <tt>:include-curvature?</tt> is given as non-NIL and the curve 
has no curvature at the specified parameter, NIL is returned for each of these four values.

:arguments (parameter \"Number. The desired parameter to be evaluated on the curve.\")
:&key ((include-curvature? nil) \"Boolean (T or NIL). Indicates whether to compute curvature information.\")"
    tangent
    (parameter &key include-curvature?)
    (if include-curvature?
        (get-curve-curvature *geometry-kernel* (the native-curve) parameter)
      (get-curve-tangent *geometry-kernel* (the native-curve) parameter)))
   
   ("Number. The radius of curvature (i.e. radius of the osculating circle) at the given parameter.

:arguments (parameter \"Number. The parameter at which to compute radius of curvature\")"
    radius-of-curvature
    (parameter)
    (multiple-value-bind (tangent point radius) (the (tangent parameter :include-curvature? t))
      (declare (ignore tangent point)) radius))
   
   ("3D Vector. The normal of the curve at the given parameter value, i.e. the vector pointing from the point 
on the curve at this parameter to the center of the osculating circle at that point. if the curve has no 
curvature at the given parameter, NIL is returned.

:arguments (parameter \"Number. The desired parameter to be evaluated on the curve.\")"
    normal
    (parameter)
    (multiple-value-bind (tangent point radius center circle-normal curve-normal)
        (the (tangent parameter :include-curvature? t))
      (declare (ignore tangent point radius center circle-normal)) curve-normal))
   

   
   ("3D Vector. The given parameter evaluated on the second derivative curve of this curve. Note that this is
only valid if this curve has degree of at least two (2), and will throw an error otherwise.

:arguments (parameter \"Number. The desired parameter to be evaluated on the second derivative curve.\")"
    acceleration
    (parameter)
    (the second-derivative (point parameter)))

   
   ("Numbers (multiple return values). The minimum and maximum parameter values for this parametric curve."
    parameter-bounds
    ()
    (get-curve-parameter-bounds *geometry-kernel* (the native-curve)))
   
   
   
   
   ("Number. The total length of the curve from given start-parameter to given end-parameter.

:&key ((u1 (the u1)) \"Number. The start parameter for computing the length\"
       (u2 (the u2)) \"Number. The end parameter for computing the length\"
       (tolerance 0.01) \"Number. The tolerance (absolute or relative to curve extent) for computing the length\"
       (tolerance-type :relative) \"Keyword Symbol, :relative or :absolute. The type of the specified tolerance\")"
    total-length
    (&key (u1 (the u1)) (u2 (the u2)) (tolerance 0.01) (tolerance-type :relative))
    (get-curve-total-length *geometry-kernel* self u1 u2 tolerance tolerance-type))
   
   ("List of 3D Points. Returns the specified number of points equally spaced along the curve.

:arguments (number \"Number. How many points to return.\")

:&key ((spacing :arc-length) \"Keyword Symbol, :parametric or :arc-length. Defaults to :arc-length.\"
       (arc-length-approximation-tolerance *zero-epsilon*) \"Number. A smaller number gives tighter arc 
length approximation. Scaled to curve exent.\")"
    equi-spaced-points
    (number &key (spacing :arc-length) (arc-length-approximation-tolerance *zero-epsilon*)
            (u1 (the u1)) (u2 (the u2)) 
            (parameter-list (when (eql spacing :parametric) (list-of-numbers u1 u2 (/ (- u2 u1) (1- number)))))
            (percentage-list (when (eql spacing :arc-length) (list-of-numbers 0 1 (/ (1- number))))))

    (unless (> number 1) (error "Equi-spaced-points must be called with a number greater than 1."))
    (case spacing 
      (:parametric (mapcar #'(lambda(parameter) (the (point parameter))) parameter-list))
      (:arc-length (mapcar #'(lambda(percentage) 
                               (the (point-at-arc-length percentage
                                                         :tolerance arc-length-approximation-tolerance))) 
                             percentage-list))
      (otherwise 
       (error "For equi-spaced-points, only :parametric and :arc-length spacing are currently implemented."))))
   
   ("Number. Returns the parameter representing a point offset from the start of the curve by the given length.

:arguments (distance \"Number. The arc length from the start.\")"
    parameter-at-length
    (distance)
    (get-offset-point-along *geometry-kernel* self (the u-min) distance))
   
   ("Surface point. Returns point at given parameter offset by given distance.

:arguments (curve-parameter \"Number. The curve parameter to start from\"
            distance \"Number. The distance to offset\")"
    offset-point-along 
    (curve-parameter distance)
    (let ((parameter (get-offset-point-along *geometry-kernel* self curve-parameter distance)))
      (make-surface-point :parameter parameter :3d-point (the (point parameter)))))
   
   (point-at-arc-length 
    (percentage &key (tolerance *zero-epsilon*))
    (get-point-at-arc-length *geometry-kernel* (the native-curve) percentage :tolerance tolerance))
   
   
   ("List of Numbers. Returns the specified number of parameters equally spaced along the curve.

:arguments (number \"Number. How many parameters to return.\")

:&key ((spacing :arc-length) \"Keyword Symbol, :parametric or :arc-length. :arc-length is the default\")"

    equi-spaced-parameters
    (number &key (spacing :arc-length))

    (unless (member spacing '(:parametric :arc-length)) (error "For equi-spaced-parameters, only :parametric and :arc-length spacing are currently implemented."))
    (unless (> number 1) (error "Equi-spaced-parameters must be called with a number greater than 1."))
    (ecase spacing
      (:parametric
       (let ((u1 (the u1)) (u2 (the u2)))
         (list-of-numbers u1 u2 (/ (- u2 u1) (1- number)))))
      (:arc-length
       (let ((percentage-list (list-of-numbers 0 1 (/ (1- number)))))
         (mapcar #'(lambda(percentage) (second (multiple-value-list (the (point-at-arc-length percentage)))))
                 percentage-list)))))

   
   ("Returns a bbox object, answering xmin, ymin, zmin, xmax, ymax, and zmax, for a box containing the convex hull
 (i.e. the control points) of this curve and oriented according to the given <tt>center</tt> and <tt>orientation</tt>."
    local-bounding-box
    (center orientation)
    (let* ((object (make-object 'base-object :center center :orientation orientation))
           (points (mapcar #'(lambda(point) (the-object object (global-to-local point))) (the b-spline-data))))
      (let ((xmax (apply #'max (mapcar #'get-x points)))
            (ymax (apply #'max (mapcar #'get-y points)))
            (zmax (apply #'max (mapcar #'get-z points)))
            (xmin (apply #'min (mapcar #'get-x points)))
            (ymin (apply #'min (mapcar #'get-y points)))
            (zmin (apply #'min (mapcar #'get-z points))))
        (make-object 'bbox :xmax xmax :ymax ymax :zmax zmax :xmin xmin :ymin ymin :zmin zmin))))
   
   
   ("Boolean. Given a point and a vector defining a plane, returns T or NIL depending whether this curve lies in the plane.
     Also returns a second value which is the maximum distance of the curve from the plane.

    :arguments (plane-point \"3d-point. Point on the plane\"
                plane-normal \"3d-vector. Normal vector of the plane\")
    :&key ((distance-tolerance *3d-tolerance-default*) \"Number. Allowed distance of any point on the curve from the plane.\")"
    in-plane?
    (plane-point plane-normal &key (distance-tolerance *3d-tolerance-default*))
    (curve-on-plane? *geometry-kernel* self plane-point plane-normal :distance-tolerance distance-tolerance))

   
   ("Boolean. Returns non-NIL if the given parameter lies within the parameter range of this curve.

:arguments (parameter \"Number. The parameter to be tested.\")"
    on? (parameter) (<= (the u-min) parameter (the u-max)))
   
   ("GDL Curve object. Returns a curve which is trimmed from parameter-1 to parameter-2.

:arguments (parameter-1 \"Number. The start parameter\"
            parameter-2 \"Number. The end parameter\")"
    trim
    (parameter-1 parameter-2)
    (make-object 'trimmed-curve :built-from self :u1 parameter-1 :u2 parameter-2))
   
   ("Surface Point. Given a 3D point, returns the point(s) projected normally onto the curve.

:&key ((distance-tolerance [covers entire curve]) \"Number. The 3D point must be within this distance of the curve for a successful drop.\")"
    dropped-point
    (3d-point &key (distance-tolerance (let ((control-points (the b-spline-data)))
                                         (apply #'max (mapcar #'(lambda(point) (3d-distance 3d-point point)) control-points)))))
    (let ((parameter (get-curve-dropped-point *geometry-kernel* self 3d-point :distance-tolerance distance-tolerance)))
      (when parameter (make-surface-point :parameter parameter :3d-point (the (point parameter))))))
   

   ("Surface Points.  Points of intersection between this curve and another curve.

This function also returns a second value, which is a list of surface points representing the ends of 
contiguous segments, if any, associated with the surface point in the same position in the primary returned list. 
NIL values in this second returned list indicate that there was no contiguous segment, only an intersecting point 
as indicated by the surface point in the primary returned list.

:arguments (other-curve \"GDL Curve. The curve with which to intersect.\")
:&key ((distance-tolerance *3d-tolerance-default*) \"Number. Distance for two points to be considered coincident.\")
:note use <tt>get-parameter-of</tt>, <tt>get-3d-point-of</tt>, and <tt>get-uv-point-of</tt> to extract 
components of a surface point."
    curve-intersection-points
    (other-curve &key (distance-tolerance *3d-tolerance-default*))
    (multiple-value-bind (starts ends) 
        (get-curve-curve-intersection-point *geometry-kernel* self other-curve 
                                            :distance-tolerance distance-tolerance)
      (let ((pairs (sort (mapcar #'list starts ends) #'< :key #'(lambda(list) (first (first list))))))
        (values (mapcar #'(lambda(pair) (let ((parameter (first pair))) 
                                          (make-surface-point :parameter (first parameter)
                                                              :other-parameter (second parameter)
                                                              :3d-point (the (point (first parameter)))))) pairs)
                (mapcar #'(lambda(pair) (let ((parameter (when (> (first (second pair)) 
                                                                  (first (first pair))) (second pair))))
                                          (when parameter (make-surface-point 
                                                           :parameter (first parameter) 
                                                           :other-parameter (second parameter)
                                                           :3d-point (the (point (first parameter))))))) pairs)))))
   
   
   ("Surface point. Returns the first point of intersection between this curve and the surface given as an argument.

:arguments (surface \"GDL Surface object. The surface to intersect with this curve.\")
:&key ((3d-tolerance *3d-tolerance-default*) \"Number. The tolerance to use for intersecting.\")"
    surface-intersection-point
    (surface &key (3d-tolerance *3d-tolerance-default*)) 
    (let ((triplet (first (get-surface-curve-intersection-points *geometry-kernel* surface self :3d-tolerance 3d-tolerance))))
      (when triplet (make-surface-point :parameter (first triplet)
                                        :uv-point (make-point (second triplet) (third triplet))
                                        :3d-point (the (point (first triplet)))))))
   
   
   ("List of Surface points. Returns the point(s) of intersection between this curve and the surface given as an argument.

:arguments (surface \"GDL Surface object. The surface to intersect with this curve.\")
:&key ((3d-tolerance *3d-tolerance-default*) \"Number. The tolerance to use for intersecting.\")"
    surface-intersection-points
    (surface &key (3d-tolerance *3d-tolerance-default*)) 
    (let ((triplets (get-surface-curve-intersection-points *geometry-kernel* surface self :3d-tolerance 3d-tolerance)))
      (mapcar #'(lambda(triplet)
                  (make-surface-point :parameter (first triplet)
                                      :uv-point (make-point (second triplet) (third triplet))
                                      :3d-point (the (point (first triplet))))) triplets)))
   
   
   ("Surface Point.  First point of intersection between this curve and the other curve given as the argument.

This function also returns a second value, which is a surface point representing the end of 
a contiguous segment, if any, associated with the surface point given as the primary return value.
A NIL value as this second return value indicates that there was no contiguous segment, only an 
intersecting point as indicated by the surface point given as the primary return value.

:arguments (other-curve \"GDL Curve. The curve with which to intersect.\")
:&key ((distance-tolerance *3d-tolerance-default*) \"Number. Distance for two points to be considered coincident.\")
:note use <tt>get-parameter-of</tt> and <tt>get-3d-point-of</tt> to extract components of a surface point."

    curve-intersection-point
    (other-curve &key (distance-tolerance *3d-tolerance-default*))
    (multiple-value-bind (starts ends) (the (curve-intersection-points other-curve :distance-tolerance distance-tolerance))
      (values (first starts) (first ends))))
   
   ("Number. The minimum radius of curvature for the curve. A second value is also returned, which is a surface point indicating
the point on the curve where this minimum radius occurs. A third value is also returned, which is a list of additional curve
parameters where similar minimum radii occur."
    minimum-radius
    ()
    (let ((parameters (get-curve-minimum-radius *geometry-kernel* self)))
      (values (the (radius-of-curvature (first parameters)))
              (make-surface-point :parameter (first parameters)
                                  :3d-point (the (point (first parameters)))) (rest parameters))))
   
   
   ("Surface Points.  Points of intersection between this curve and the plane denoted by plane-point and plane-normal.

:note use <tt>get-parameter-of</tt> and <tt>get-3d-point-of</tt> to extract components of a surface point."
    plane-intersection-points
    (plane-point plane-normal)
    (let ((parameters (sort (get-curve-plane-intersection-point *geometry-kernel* self plane-point (unitize-vector plane-normal)) #'<)))
      (mapcar #'(lambda(parameter) (make-surface-point :parameter parameter :3d-point (the (point parameter)))) parameters)))
   
   ("Surface Point.  First point of intersection between this curve and the plane denoted by plane-point and plane-normal

:note use <tt>get-parameter-of</tt> and <tt>get-3d-point-of</tt> to extract components of a surface point."

    plane-intersection-point
    (plane-point plane-normal)
    (first (the (plane-intersection-points plane-point plane-normal))))

   
   ("Either T or a plist of numbers with keys <tt>:distance</tt>, <tt>angle</tt>, <tt>:length</tt>. 

:&key ((3d-tolerance *3d-tolerance-default*) \"Allowed maximum distance between curve segments.\"
       (angle-tolerance *angle-tolerance-default) \"Allowed maximum angle in radians between end/start tangents of curve segments.\"
       (minimum-segment-length *minimum-segment-length-default*) \"Allowed minimum curve segment length.\")"
    check-continuity
    (&key (3d-tolerance *3d-tolerance-default*)
          (angle-tolerance *angle-tolerance-radians-default*)
          (minimum-segment-length *minimum-segment-length-default*))
    (let ((decomposed (list-elements (the %decomposed%))))
      (let ((result (list :distance nil :angle nil :length nil)))
        (mapc #'(lambda(c1 c2)
                  (let ((tan1 (the-object c1 (tangent (the-object c1 u-max))))
                        (tan2 (the-object c2 (tangent (the-object c2 u-min)))))
                    (let ((angle (angle-between-vectors tan1 tan2))
                          (current (getf result :angle)))
                      (when (and (> angle angle-tolerance) (or (null current) (> angle current)))
                        (setf (getf result :angle) angle))))
                  (let ((distance (3d-distance (the-object c1 end) (the-object c2 start)))
                        (current (getf result :distance)))
                    (when (and (> distance 3d-tolerance) (or (null current) (> distance current)))
                      (setf (getf result :distance) distance)))
                  (let ((length (the-object c1 total-length))
                        (current (getf result :length)))
                    (when (and (< length minimum-segment-length) (or (null current) (< length current)))
                      (setf (getf result :length) length))))
              (if (the closed?) (append decomposed (list (first decomposed)))
                decomposed)
              (if (the closed?) (rest (append decomposed (list (first decomposed))))
                (rest decomposed)))
        (if (and (null (getf result :distance)) (null (getf result :angle)) (null (getf result :length)))
            t result))))
   
   
   ("Keyword symbol, :closed, :open, or :continuous. :continuous if ends are within <tt>*3d-tolerance-default*</tt> and tangents 
are within <tt>*angle-tolerance-default*</tt>, :closed if ends are within <tt>*3d-tolerance-default*</tt> but tangents are
not within <tt>*angle-tolerance-default*</tt>, and :open if ends are not within <tt>*3d-tolerance-default*</tt>."
    closure
    ()
    (cond ((and (coincident-point? (the start) (the end) :tolerance *3d-tolerance-default*)
                (let ((tan1 (the (tangent (the u-max))))
                      (tan2 (the (tangent (the u-min)))))
                  (< (angle-between-vectors tan1 tan2) *angle-tolerance-radians-default*))) :continuous)
          ((coincident-point? (the start) (the end) :tolerance *3d-tolerance-default*) :closed)
          (t :open)))

   
   ("Number. The reciprocal of the radius of curvature at the given parameter." curvature (parameter) (/ (the (radius-of-curvature parameter))))
   
   
   ("Plist. The returned plist contains information about the minimum distance from this curve to the point given as the argument.

:arguments (point \"3D Point\")"
    minimum-distance-to-point
    (point &key (distance-tolerance 10000))
    (the (point-solve point :minimize :distance-tolerance distance-tolerance)))
   
   ("Plist. The returned plist contains information about the maximum distance from this curve to the point given as the argument.

:arguments (point \"3D Point\")"
    maximum-distance-to-point
    (point &key (distance-tolerance 10000))
    (the (point-solve point :maximize :distance-tolerance distance-tolerance)))
   
   
   (point-solve
    (point operation &key (distance-tolerance 10000))
    (let ((parameter (first (global-curve-point-solve *geometry-kernel* 
                                                      self 
                                                      :point point 
                                                      :operation-type operation
                                                      :distance-tolerance distance-tolerance))))
      (let* ((point-on-curve (when parameter (make-surface-point :parameter parameter
                                                                 :3d-point (the (point parameter)))))
             (distance (when point-on-curve
                         (3d-distance (get-3d-point-of point-on-curve) point))))
          (list :point-on-curve point-on-curve :distance distance))))
   
   
   ("List of Plists. The returned list of plists contains information about the points where the tangents of this curve and those of the curve given as the argument
are equal.

:arguments (curve \"GDL Curve object\")"
    dropped-curve
    (curve)
    (the (curve-solves curve :normalize)))
   
   
   ("List of Plists. The returned list of plists contains information about the points where the tangents of this curve and the vector given as the argument
are equal.

:arguments (vector \"GDL Vector\")"
    
    tangent-points 
    (vector)
    (the (curve-solves (make-object 'linear-curve :start (make-point 0 0 0) :end vector) :normalize)))
   
   
   ("Plist. The returned plist contains information about the minimum distance from this curve to the curve given as the argument.

:arguments (curve \"GDL Curve object\")"
    minimum-distance-to-curve
    (curve)
    (the (curve-solve curve :minimize)))
   
   ("Plist. The returned plist contains information about the maximum distance from this curve to the curve given as the argument.

:arguments (curve \"GDL Curve object\")"
    maximum-distance-to-curve
    (curve)
    (the (curve-solve curve :maximize)))
   
   (curve-solves
    (curve operation)
    (let (result)
      (dolist (solution (global-curve-solve *geometry-kernel* self 
                                            :other-curve curve :operation-type operation) (nreverse result))
        (destructuring-bind (parameter other-parameter) solution
          (let ((point-on-curve (when parameter (make-surface-point :parameter parameter
                                                                    :3d-point (the (point parameter)))))
                (point-on-other-curve (when other-parameter 
                                        (make-surface-point :parameter other-parameter
                                                            :3d-point (the-object curve 
                                                                                  (point other-parameter))))))
            (let ((distance (when (and point-on-curve point-on-other-curve)
                              (3d-distance (get-3d-point-of point-on-curve) 
                                           (get-3d-point-of point-on-other-curve)))))
              (push (list :point-on-curve point-on-curve 
                          :point-on-other-curve point-on-other-curve :distance distance) result)))))))
   
   
   (curve-solve
    (curve operation)
    (destructuring-bind (parameter other-parameter) 
        (first (global-curve-solve *geometry-kernel* self :other-curve curve :operation-type operation))
      
      (let ((point-on-curve (when parameter (make-surface-point :parameter parameter
                                                                :3d-point (the (point parameter)))))
            (point-on-other-curve (when other-parameter 
                                    (make-surface-point :parameter other-parameter
                                                        :3d-point (the-object curve (point other-parameter))))))
        (let ((distance (when (and point-on-curve point-on-other-curve)
                          (3d-distance (get-3d-point-of point-on-curve) (get-3d-point-of point-on-other-curve)))))
          (list :point-on-curve point-on-curve :point-on-other-curve point-on-other-curve :distance distance)))))))



