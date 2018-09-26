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


(in-package :geom-base)

;;
;; FLAG -- vet the use of each slot in this object.
;;


(define-object renderer-mixin ()

  :documentation (:description "Object mixed into the base-view to compute required values to provide
a rendered perspective view, as in VRML.")
  
  :input-slots
  (
   (camera-distance-discount 0.8)
   
   "List of GDL Objects. Roots of the leaf objects to be displayed in this renderer view."
   object-roots
   
   "List of GDL Objects. Leaves of the objects to be displayed in this renderer view."
   objects
   
   (background-color (lookup-color (getf *colors-default* :background) :format :decimal))
   
   (zoom-factor-renderer 1)
   
   (view :trimetric)
   
   ("Number in angular degrees. The maximum angle of the view frustrum for perspective views. 
Defaults to 0.1 (which results in a near parallel projection with virtually no perspective effect)."
    field-of-view-default 45)
   
   

   ("List of Plists. Each plist contains, based on each entry in the <tt>view-vectors</tt>, keys: 
<ul>
<li><tt>:point</tt> (camera location, defaults to the <tt>3d-box-center</tt> translated 
     along the corresponding element of <tt>view-vectors</tt>) by the local camera distance. 
     The camera distance is computed based on the field-of-view angle and 
     the <tt>bounding-sphere</tt></li>
<li><tt>:orientation</tt> (3d matrix indicating camera orientation)</li>
<li><tt>field-of-view</tt> Angle in degrees of the view frustrum (i.e. lens angle of the virtual camera)."
    viewpoints (mapcan #'(lambda(key vector)
                           ;;
                           ;; FLAG -- compute this distance to do a fit
                           ;;
                           (let ((transform 
                                  (alignment :top vector
                                             :rear (if (or (same-direction-vectors? vector *nominal-z-vector*)
                                                           (same-direction-vectors? vector *nominal-z-vector-r*))
                                                       *nominal-y-vector* *nominal-z-vector*))))
                             (let* ((2d-box (project-3d-box (the 3d-box) vector 
                                                            (matrix:transpose-matrix transform)))
                                    (max-extent (max (- (get-x (second 2d-box)) (get-x (first 2d-box)))
                                                     (- (get-y (second 2d-box)) (get-y (first 2d-box)))))
                                    (camera-distance
				     (* (the camera-distance-discount)
                                      (* (+ (/ max-extent (tan (degree (the field-of-view-default))))
                                            (getf (the bounding-sphere) :radius)) (/ (the zoom-factor-renderer)))))
				    )
                               
                               (list key (list :point (translate-along-vector 
                                                       (the 3d-box-center) vector camera-distance)
                                               :orientation transform
                                               :speed (div camera-distance 5)
                                               :field-of-view (the field-of-view-default))))))
                       (plist-keys (the view-vectors))
                       (plist-values (the view-vectors))))
   
   
   (flight-paths (list (list :points (list (make-point 0 -100 10)
                                           (make-point 0 -90 10)
                                           (make-point 0 -80 10)
                                           (make-point 0 -70 10)
                                           (make-point 0 -60 10)
                                           (make-point 0 -50 10)
                                           (make-point 0 -40 10)
                                           (make-point 0 -30 10)
                                           (make-point 0 -20 10)
                                           (make-point 0  -10 10)
                                           (make-point 0    0 10))
                             
                             :vectors (mapcar 
                                       #'(lambda(vector)
                                           (alignment 
                                            :top vector
                                            :rear (if (or (same-direction-vectors? vector *nominal-z-vector*)
                                                          (same-direction-vectors? vector *nominal-z-vector-r*))
                                                      *nominal-y-vector* *nominal-z-vector*)))
                                              (list (make-vector 0 -1 0)
                                                    (make-vector 0 -1 0)
                                                    (make-vector 0 -1 0)
                                                    (make-vector 0 -1 0)
                                                    (make-vector 0 -1 0)
                                                    (make-vector 0 -1 0)
                                                    (make-vector 0 -1 0)
                                                    (make-vector 0 -1 0)
                                                    (make-vector 0 -1 0)
                                                    (make-vector 0 -1 0)
                                                    (make-vector 0 -1 0)))
                             
                             :fields-of-view (make-list 11 :initial-element 0.5))))

   
   
   ("3D Point. The effective view center for the scene contained in this view object. Defaults to the center of the bounding sphere of all
the objects in the scene, consisting of the <tt>object-roots</tt> and the <tt>objects</tt>."
    3d-box-center (getf (the bounding-sphere) :center))
   
   
   ("Plist. Keys indicate view vector names (e.g. <tt>:trimetric</tt>), and values contain the 3D vectors. Defaults to the 
parameter <tt>*standard-views*</tt>, but with the key corresponding to current <tt>(the view)</tt> ordered 
first in the plist. This list of view-vectors is used to construct the default <tt>viewpoints</tt>."
    view-vectors (append (list (the view) (getf *standard-views* (the view)))
                         (mapcan #'(lambda(key val)
                                     (when (not (eql key (the view))) (list key val))) 
                                 (plist-keys *standard-views*) (plist-values *standard-views*))))
   
   ("List of two 3D points. The left-front-lower and right-rear-upper corners of the axis-aligned bounding 
box of the <tt>object-roots</tt> and <tt>objects</tt>."
   3d-box (bounding-box-from-list (the object-roots) :local-objects (the objects)))
   
   
   ("Plist containing keys: <tt>:center</tt> and <tt>:radius</tt>. This plist represents the tightest-fitting sphere
around all the objects listed in the <tt>object-roots</tt> and the <tt>objects</tt>"
    bounding-sphere
    (let ((ll (first (the 3d-box))) (ur (second (the 3d-box))))
      (let ((x1 (get-x ll)) (y1 (get-y ll)) (z1 (get-z ll))
            (x2 (get-x ur)) (y2 (get-y ur)) (z2 (get-z ur)))
        (let ((center (make-point (+ x1 (half (- x2 x1))) (+ y1 (half (- y2 y1))) (+ z1 (half (- z2 z1))))))
          (list :center center :radius (3d-distance center ll))))))))



;;
;; FLAG -- to utilities 
;; FLAG -- see if this is used anywhere but in
;; renderer-mixin, if so, make into member function.
;;

(defun project-3d-box (bbox vector transform)
  (let ((p1 (first bbox)) (p2 (second bbox)))
    (let ((2d-points (mapcar #'(lambda(point)
                                 (transform-3d-vector (inter-line-plane point vector p1 vector) transform))
                             (list p1 
                                   (make-point (get-x p1) (get-y p1) (get-z p2))
                                   (make-point (get-x p1) (get-y p2) (get-z p2))
                                   (make-point (get-x p2) (get-y p1) (get-z p1))
                                   (make-point (get-x p2) (get-y p2) (get-z p2))
                                   (make-point (get-x p1) (get-y p2) (get-z p1))
                                   (make-point (get-x p2) (get-y p1) (get-z p2))
                                   p2))))
      (list (make-point (apply #'min (mapcar #'get-x 2d-points))
                        (apply #'min (mapcar #'get-y 2d-points)))
            (make-point (apply #'max (mapcar #'get-x 2d-points))
                        (apply #'max (mapcar #'get-y 2d-points)))))))
