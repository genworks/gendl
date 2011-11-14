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

(in-package :geom-base)

(define-format vrml (base-format)
  
  :slots ((minimum-number-of-segments 0) (maximum-3d-distance-between-points 0)
          (minimum-parametric-ratio 0.00001) (chord-height 5)
          (angle-tolerance-degrees 45) (max-edge-length 0)
          (min-edge-length 0) (min-edge-length-ratio-uv 0.001) (max-aspect-ratio 5)
          (use-bsplines? nil)))
  
(define-lens (vrml vanilla-mixin)()
  :output-functions
  ((cad-output-tree (&rest args) (declare (ignore args)))
   (cad-output (&rest args) (declare (ignore args)))
   (shape (&rest args) (declare (ignore args)))))



(define-lens (vrml point)()
  :output-functions
  ((shape
    ()
    (let ((point (the (global-to-local (the center)))))
      (format *stream* "
Shape
  {geometry PointSet {coord Coordinate {point [~a ~a ~a]}}
   appearance Appearance {
     material Material {~a}}}
"
              (get-x point) (get-y point) (get-z point)
              (write-the material-properties-string))))))


(define-lens (vrml box)()
  :output-functions
  (("Generic VMRL shape output for a box"
    shape
    ()
    (format *stream* "



Shape
  {geometry Box {size ~3,7f ~3,7f ~3,7f}
   appearance Appearance {
     material Material {~a}}}
"

            (the width) (the length) (the height)
              
              (write-the material-properties-string)))
   
))
        
        
(define-lens (vrml global-filleted-polyline)()
  :output-functions
  ((shape
    ()
    (let ((points (the interpolated-points)))
      (format *stream* "

Shape 
 {geometry IndexedLineSet {coord Coordinate {point [~{~a~^ ~}]}
                           coordIndex [~{~a~^ ~}]}
  appearance Appearance {
     material Material {~a}}}
"
              (mapcar #'(lambda(point) (let ((point (the (global-to-local point))))
                                         (format nil "~a ~a ~a" (get-x point) (get-y point) (get-z point)))) points)
              (list-of-numbers 0 (1- (length points)))
              (write-the material-properties-string))))))


(define-lens (vrml spherical-cap)()
  :output-functions
  ((shape
    ()
    (let ((cross-section (let ((section (append (when (typep (the inner-cap) 'spherical-cap)
                                                  (reverse (mapcar #'(lambda(point) 
                                                                       (the (global-to-local (translate point :up (half (the axis-length))
                                                                                                        :left (the base-radius)))))
                                                                   (the inner-cap (meridians 0) interpolated-points))))
                                                (mapcar #'(lambda(point) 
                                                            (the (global-to-local (translate point :up (half (the axis-length))
                                                                                             :left (the base-radius)))))
                                                        (the (meridians 0) interpolated-points)))))
                           (if (typep (the inner-cap) 'spherical-cap) (append section (list (first section))) section)))
          
          (spine (reverse (mapcar #'(lambda(point) 
                                      (the (global-to-local point))) (the (parallels :first) interpolated-points)))))
           
      (format *stream* "


Shape 
 {geometry Extrusion {
                      solid FALSE
                      beginCap FALSE
                      endCap FALSE
                      creaseAngle 0.1
                      crossSection [~{~3,7f ~3,7f~^, ~}]
                      spine [~{~3,7f ~3,7f ~3,7f~^, ~}]}
   appearance Appearance {
     material Material {~a}}}
"
              (mapcan #'(lambda(point) (list (get-x point) (get-z point))) cross-section)
              (mapcan #'(lambda(point) (list (get-x point) (get-y point) (get-z point))) spine)
              (write-the material-properties-string))
      
      ;;
      ;; FLAG -- simply extend the cross-section in this case
      ;;
      ))))



(define-lens (vrml cylinder)()
  :output-functions
  (
   (shape
    ()
    (format *stream* "

 Shape 
  {geometry Cylinder {height ~3,7f radius ~3,7f
                      bottom ~a top ~a}
   appearance Appearance {
     material Material {~a}}}
"
            (the length) (the radius)
            (if (the bottom-cap?) "TRUE" "FALSE") (if (the top-cap?) "TRUE" "FALSE")
            (write-the material-properties-string)))))


(define-lens (vrml global-polyline)()
  :output-functions
  ((shape
    ()
    (format *stream* "

Shape 
 {geometry IndexedLineSet {coord Coordinate {point [~{~a~^ ~}]}
                           coordIndex [~{~a~^ ~}]}
  appearance Appearance {
     material Material {~a}}}
"
            
            
            (mapcar #'(lambda(point) (format nil "~a ~a ~a" (get-x point) (get-y point) (get-z point))) 
                    (the vertex-list-local))
            (list-of-numbers 0 (1- (length (the vertex-list-local))))
            (write-the material-properties-string)))))





;;
;; FLAG -- identical to surf:curve and arc.
;;
(define-lens (vrml ellipse)()
  :output-functions
  ((shape
    ()
    (dolist (bezier (list-elements (the beziers)))
      (let* ((points (mapcar #'(lambda(point) (the (global-to-local point))) (the-object bezier (equi-spaced-points *curve-chords*))))
             (indices (list-of-numbers 0 (1- (length points)))))
        (format *stream* "



Shape
  {geometry IndexedLineSet {coord Coordinate {point [~{~a~^ ~}]}
                            coordIndex [~{~a~^ ~}]}
   appearance Appearance {
     material Material {~a}}}
"
                (mapcar #'(lambda(point) (format nil "~a ~a ~a" (get-x point) (get-y point) (get-z point))) points)
                indices
                (write-the material-properties-string)))))))


;;
;; FLAG -- this all breaks if you have non-specific base-objects as
;; children, since multiple headers will be written.
;;


(define-lens (vrml base-object)()
  :output-functions
  ((header
    ()
    (format *stream* "#VRML V2.0 utf8~%~%")
    
    (format *stream* "
WorldInfo {
  title \"VRML data generated by Genworks GDL\"
  info \"Please visit www.genworks.com for more info\"}")
    
    

    (let ((background-color (lookup-color (the background-color))))
      
      ;;
      ;; FLAG -- start using background-color again.
      ;;
      (declare (ignore background-color))
      
      (format *stream* "
Background {skyColor [~a ~a ~a]}" 
              1.0 ;;(to-double-float (get-x background-color)) 
              1.0 ;;(to-double-float (get-y background-color))
              1.0 ;;(to-double-float (get-z background-color))
              
              )))

    
   (get-diffuse-color 
    ()
    (let* ((display-controls (find-in-hash self *display-controls*))
           (color-decimal (getf display-controls :color-decimal)))
      
      (or color-decimal (coerce (or (the color-decimal) 
                                    (format-slot foreground-color)
                                    (lookup-color (the color-decimal))) 'list))))
   
   (material-properties-string
    ()
    (let ((display-controls (the display-controls)))
      (let ((diffuse-color (format nil "diffuseColor ~{~3,7f~^ ~}" 
                                   (coerce (write-the get-diffuse-color) 'list)))
            
            (emissive-color (let ((color (getf (the display-controls) :emissive-color)))
                               (if color (format nil " emissiveColor ~{~3,7f~^ ~}" (coerce (lookup-color color) 'list)) "")))
            
            
            (specular-color (let ((color (getf (the display-controls) :specular-color)))
                               (if color (format nil " specularColor ~{~3,7f~^ ~}" (coerce (lookup-color color) 'list)) "")))
            
            (transparency (let ((transparency (getf display-controls :transparency)))
                            (if transparency (format nil " transparency ~a" transparency) "")))
            
            
            (shininess (let ((shininess (getf display-controls :shininess)))
                         (if shininess (format nil " shininess ~a" shininess) "")))
            
            
            (ambient-intensity (let ((ambient-intensity  (getf display-controls :ambient-intensity)))
                                 (if ambient-intensity (format nil " ambientIntensity ~a" ambient-intensity) "")))
            
            ;;
            ;; FLAG -- fill in
            ;;
            )
        (string-append diffuse-color emissive-color specular-color transparency shininess  ambient-intensity ))))
        
        

   
   
   (cad-output
    (&key (header? t))
    
    (when header? (write-the header))
    (let ((orientation (if (the local-left-handed?) 
                           (multiply-matrices +lh-identity-matrix+ (the orientation))
                         (the orientation))))
      (let ((rotation (quaternion-to-rotation (matrix-to-quaternion orientation)))
            (x (if (the local-left-handed?) (- (get-x (the center))) (get-x (the center))))
            (y (get-y (the center))) (z (get-z (the center))))
        (format *stream* "
Transform
{~a~a

children
 [
"
                  
                (if rotation (format nil "rotation ~3,7f ~3,7f ~3,7f ~3,7f~%" 
                                     (get-x rotation) (get-y rotation) (get-z rotation) (get-w rotation)) "")
                (if (not (every #'zerop (list x y z))) (format nil "translation ~3,7f ~3,7f ~3,7f" x y z) "")
                
                ))

      
      (when gwl::*onclick-function* 
        (format *stream* "

Anchor 
{
url \"javascript:event=null;~a\"
children [

"  (funcall gwl::*onclick-function* self)))

      
      (write-the shape)
      
      (when gwl::*onclick-function* (format *stream* "]}~%"))
      
      )
    
    (when (and (typep self 'outline-specialization-mixin)
               (not (ignore-errors (typep self (read-from-string "surf:surface")))))
      
      (let* ((center (reverse-vector (the center)))
             (x (get-x center)) (y (get-y center)) (z (get-z center))
             (inverse (when (the orientation)
                        (quaternion-to-rotation (matrix-to-quaternion (matrix:transpose-matrix (the orientation)))))))
        
        (format *stream* "
Transform
{~a
  children
 [
 
 Transform
 {~a
  children
  [
"
                (if inverse (format nil "rotation ~3,7f ~3,7f ~3,7f ~3,7f~%" 
                                    (get-x inverse) (get-y inverse) (get-z inverse) (get-w inverse)) "")
                
                (if (not (every #'zerop (list x y z)))
                    (format nil "translation ~3,7f, ~3,7f, ~3,7f" x y z)
                  "")))
      
      (mapc #'(lambda(outline-object)
                (write-the-object outline-object (cad-output :header? nil)))
            (the outline-leaves))
      
      (format *stream* "]}]}~%" ))

    
    (format *stream* "]}~%" ))
    

   (cad-output-tree
    (&key (header? t) (from-root? t))
    
    (when header? (write-the header))
    
    (let ((center (if from-root? (the center) (the vrml-center)))
          (orientation (let ((base (if from-root? (the orientation) (the local-orientation))))
                         (if (the local-left-handed?) 
                             (multiply-matrices +lh-identity-matrix+ base)
                           base))))
      (let ((rotation (quaternion-to-rotation (matrix-to-quaternion orientation)))
            ;;(x (if (the left-handed?) (- (get-x center)) (get-x center))) 
            (x (get-x center))
            (y (get-y center)) (z (get-z center)))
        
        (format *stream* "
Transform
{~a~a
 children
 ["

                (if rotation (format nil "rotation ~3,7f ~3,7f ~3,7f ~3,7f~%" 
                                     (get-x rotation) (get-y rotation) (get-z rotation) (get-w rotation)) "")
                
                (if (not (every #'zerop (list x y z))) (format nil "translation ~3,7f ~3,7f ~3,7f" x y z) ""))))
      
    (if (null (the children))
        (progn
          (when gwl::*onclick-function* 
            (format *stream* "

Anchor 
{
url \"javascript:event=null;~a\"
children [

"  
                (funcall gwl::*onclick-function* self)))
          
          (write-the shape)
          
          (when gwl::*onclick-function* (format *stream* "]}~%")))
          
      (mapc #'(lambda(child) (write-the-object child (cad-output-tree :header? nil :from-root? nil))) (the children)))
    
    (when (and (typep self 'outline-specialization-mixin)
               (not (ignore-errors (typep self (read-from-string "surf:surface")))))
      
      (let* ((center (reverse-vector (the center)))
             (x (get-x center)) (y (get-y center)) (z (get-z center))
             (inverse (when (the orientation)
                        (quaternion-to-rotation (matrix-to-quaternion (matrix:transpose-matrix (the orientation)))))))

        (format *stream* "
Transform
{~a
  children
 [

 Transform
  {~a
   children
  [
"
                (if inverse (format nil "rotation ~3,7f ~3,7f ~3,7f ~3,7f~%" 
                                    (get-x inverse) (get-y inverse) (get-z inverse) (get-w inverse)) "")

                
                (if (not (every #'zerop (list x y z)))
                    (format nil "translation ~3,7f, ~3,7f, ~3,7f" x y z)
                  "")))
      
      (mapc #'(lambda(outline-object)
                (write-the-object outline-object (cad-output :header? nil)))
            (the outline-leaves))
      
      (format *stream* "]}]}~%"))
    
      
    (format *stream* "]}~%"))
   

   
   (shape () )
   
   (transform ())
   
   (transform-tree () (maptree self #'(lambda(object) (write-the-object object (transform)))))
   
   
   (ui-display-list
    ()
    
    (write-the header)
    
    (dolist (object (the ui-display-list-objects))
      (maptree object #'(lambda(node) (write-the-object node (transform)))))
    (format *stream* "]}"))))

(define-lens (vrml global-polygon-projection)()
  :output-functions
  (
   
   (shape
    ()
    
    (if (null (the polygon-original profile-origin))
        (warn "~&~%
*******************************************************

The profile ~s (root-path: ~a) 
does not lie in a major plane of its local coordinate system. This 
is not currently supported in GDL VRML output. Please let us know 
if you require this and we will investigate the feasibility.

*******************************************************

" (the polygon-original) (the polygon-original root-path))

      (let ((start-point (the polygon-original profile-origin))
            (projection-vector (if (the orientation) (matrix*vector (the orientation) (the projection-vector)) (the projection-vector))))
        (let ((cross-section (the polygon-original (interpolated-points-2d)))
              (spine-first (ecase (the offset)
                             ((:up :down) start-point)
                             (:center (translate-along-vector start-point (reverse-vector projection-vector)
                                                              (half (the projection-depth)))))))
          (let ((spine (list spine-first
                             (translate-along-vector spine-first
                                                     (ecase (the offset)
                                                       ((:up :center) projection-vector)
                                                       (:down (reverse-vector projection-vector)))
                                                     (the projection-depth)))))
            (format *stream* "
Shape 
 {geometry Extrusion {
                      beginCap TRUE
                      endCap TRUE
                      convex FALSE
                      creaseAngle 1.5
                      solid FALSE
                      crossSection [~{~3,7f ~3,7f~^, ~}]
                      spine [~{~3,7f ~3,7f ~3,7f~^, ~}]}
   appearance Appearance {
     material Material {~a}}}
"
                    (mapcan #'(lambda(point) (list (get-x point) (get-y point))) cross-section)
                    (mapcan #'(lambda(point) (list (get-x point) (get-y point) (get-z point))) spine)
                    (write-the material-properties-string)))))))))



;;
;; FLAG -- finish this and other lights later.
;;




(define-lens (vrml line)()
  :output-functions
  ((shape
    ()
    (let* ((points (mapcar #'(lambda(point) (the (global-to-local point))) (list (the start) (the end))))
           (indices (list-of-numbers 0 (1- (length points)))))
        (format *stream* "
Shape
  {geometry IndexedLineSet {coord Coordinate {point [~{~a~^ ~}]}
                            coordIndex [~{~a~^ ~}]}
   appearance Appearance {
     material Material {~a}}}
"
                (mapcar #'(lambda(point) (format nil "~a ~a ~a" (get-x point) (get-y point) (get-z point))) points)
                indices
                (write-the material-properties-string))))))


(define-lens (vrml bezier-curve)()
  :output-functions
  ((shape
    ()
    (let* ((points (mapcar #'(lambda(point) (the (global-to-local point))) (the (equi-spaced-points *curve-chords*))))
           (indices (list-of-numbers 0 (1- (length points)))))
      (format *stream* "
Shape
  {geometry IndexedLineSet {coord Coordinate {point [~{~a~^ ~}]}
                            coordIndex [~{~a~^ ~}]}
   appearance Appearance {
     material Material {~a}}}
"
                (mapcar #'(lambda(point) (format nil "~a ~a ~a" (get-x point) (get-y point) (get-z point))) points)
                indices
                (write-the material-properties-string))))))



(define-lens (vrml cone)()
  :output-functions
  ((shape
    ()
    (let ((spine (let ((spine (mapcar #'(lambda(point) (the (global-to-local point)))
                                      (the (end-arcs 1) interpolated-points))))
                   (if (= (the arc) 2pi) spine (reverse spine))))
          (cross-section (mapcar #'(lambda(point radius)
                                     (let ((point (the (global-to-local (translate point :left radius :front (half (the length)))))))
                                       (make-point (get-x point) (get-y point))))
                                 (append (the start-line)
                                         (if (typep (the inner-cylinder) 'cylinder)
                                             (reverse (the inner-cylinder start-line))
                                           (list (the end) (the start))))
                                 (list (the radius-1) (the radius-2) (the radius-2) (the radius-1)))))
      (setq cross-section (append cross-section (list (first cross-section))))
      (format *stream* "
Shape 
 {geometry Extrusion {
                      creaseAngle ~a
                      solid FALSE
                      crossSection [~{~3,7f ~3,7f~^, ~}]
                      spine [~{~3,7f ~3,7f ~3,7f~^, ~}]}
   appearance Appearance {
     material Material {~a}}}
"
              
              1
              (mapcan #'(lambda(point) (list (get-x point) (get-y point))) cross-section)
              (mapcan #'(lambda(point) (list (get-x point) (get-y point) (get-z point))) spine)
              (write-the material-properties-string))))))


(define-lens (vrml sphere)()
  :output-functions
  (
   

     (shape 
      ()
      (let ((center (the (global-to-local (make-point (if (the left-handed?) 
                                                          (- (get-x (the center))) 
                                                        (get-x (the center))) (get-y (the center)) (get-z (the center)))))))
        (format *stream* "
Transform
{translation ~3,7f ~3,7f ~3,7f
 children
 [
 Shape 
  {geometry Sphere {radius ~3,7f}
   appearance Appearance {
   material Material {~a}}}]}
"
                (get-x center) (get-y center) (get-z center)
                (the radius)
                (write-the material-properties-string))))))


(define-lens (vrml torus)()
  :output-functions
  ((shape
    ()
    (let ((cross-section (mapcar #'(lambda(point) 
                                   (the (global-to-local point))) (the start-circle interpolated-points)))
          (spine (mapcar #'(lambda(point) 
                             (the (global-to-local point))) (the centerline-arc interpolated-points))))
           
      (format *stream* "
Shape 
 {geometry Extrusion {
                      beginCap ~a
                      endCap ~a
                      solid FALSE
                      creaseAngle 0.1
                      crossSection [~{~3,7f ~3,7f~^, ~}]
                      spine [~{~3,7f ~3,7f ~3,7f~^, ~}]}
   appearance Appearance {
     material Material {~a}}}
"
              (if (the end-caps?) "TRUE" "FALSE")
              (if (the end-caps?) "TRUE" "FALSE")
              ;;(if (getf (the display-controls) :solid?) "TRUE" "FALSE")
              (mapcan #'(lambda(point) (list (get-x point) (get-y point))) cross-section)
              (mapcan #'(lambda(point) (list (get-x point) (get-y point) (get-z point))) spine)
              (write-the material-properties-string))
      
      (when (the inner-minor-radius) (write-the inner-torus shape))))))


;;
;; FLAG -- identical to surf:curve and ellipse.
;;


(define-lens (vrml arc)()
  :output-functions
  ((shape
    ()
    (dolist (bezier (list-elements (the beziers)))
      (let* ((points (mapcar #'(lambda(point) (the (global-to-local point))) (the-object bezier (equi-spaced-points *curve-chords*))))
             (indices (list-of-numbers 0 (1- (length points)))))
        (format *stream* "
Shape
  {geometry IndexedLineSet {coord Coordinate {point [~{~a~^ ~}]}
                            coordIndex [~{~a~^ ~}]}
   appearance Appearance {
     material Material {~a}}}
"
                (mapcar #'(lambda(point) (format nil "~a ~a ~a" (get-x point) (get-y point) (get-z point))) points)
                indices
                (write-the material-properties-string)))))))


(define-lens (vrml base-drawing)()
  :output-functions
  ((cad-output
    ()
    (let ((req (when (find-package :gwl) (symbol-value (read-from-string "gwl:*req*")))))
      (let ((keep-alive? (and req (slot-value req (read-from-string "net.aserve::request-header-connection"))))
            (protocol (and req (slot-value req (read-from-string "net.aserve::protocol")))))
          
        (when *debug?* (describe req) (print-variables self keep-alive? protocol))
          
        (write-the header)
          
        (mapc #'(lambda(view)
                  (let ((object-roots (ensure-list (the-object view object-roots)))
                        (objects (ensure-list (the-object view  objects))))
                      
                    (when *debug?* (print-variables object-roots objects))
                      
                    (mapc #'(lambda(root) (write-the-object root (cad-output-tree :header? nil))) object-roots)
                    (mapc #'(lambda(leaf) (write-the-object leaf (cad-output :header? nil))) objects)
                    )) 
                
              (the views))
          
        )))))


(in-package :gdl-user)


(in-package :surf)

(defparameter *include-vrml-normals?* t "Boolean. 
Indicates whether surface-normals are included in vrml output. 
Defaults to t.")

(defparameter *separate-brep-faces-for-vrml?* t "Boolean. 
Indicates whether vrml output for breps includes one IndexedFaceSet per face, or one IndexedFaceSet for the whole brep. 
Default is t (one IndexedFaceSet for each brep).")


(eval-when (compile load eval) 
  (export '*include-vrml-normals?* :surf)
  (export '*separate-brep-faces-for-vrml?* :surf))


(define-lens (vrml brep)()
   :output-functions
   ((shape
     ()
     (with-error-handling (:timeout *brep-vrml-timeout*)
       (dolist (data  (if *separate-brep-faces-for-vrml?*
                          (the triangle-data) 
                        (list (the one-face-data))))
         (let ((vertex-counts (coerce (getf data :vertex-counts) 'list))
               (vertex-indices (coerce (getf data :vertex-indices) 'list))
               (normals (coerce (getf data :surface-normals) 'list))
               (3d-points (coerce (getf data :3d-points) 'list)))
           (let ((3d-points  
                  (mapcar 
                   #'(lambda(point) 
                       (the (global-to-local 
                             (make-point (if (the left-handed?) 
                                             (- (get-x point)) (get-x point))
                                         (get-y point) (get-z point)))))
                   3d-points))
                
                
                 (normals
                  (when *include-vrml-normals?*
                    (mapcar 
                     #'(lambda(point) 
                         (the (global-to-local 
                               (make-point (if (the left-handed?) 
                                               (- (get-x point)) (get-x point))
                                           (get-y point) (get-z point)))))
                     normals))))
              
             (format *stream* "
Shape
  {geometry IndexedFaceSet {
           solid FALSE
           creaseAngle ~a
           coord Coordinate {
                 point [~{~a~^, ~}]}
~a         

          coordIndex [~a]}
   appearance Appearance {
     material Material {~a}}}
"
                      
                      
                     (or (getf (the display-controls) :crease-angle)
                         *crease-angle-default*)
                     
                     (let ((*read-default-float-format* 'single-float))
                       (mapcar #'(lambda(point) 
                                   (format nil "~a ~a ~a" 
                                           (to-single-float (get-x point)) 
                                           (to-single-float (get-y point)) 
                                           (to-single-float (get-z point)))) 
                               3d-points))

                     (if normals
                         (format nil
                                 "                 normal Normal {
                 vector [~{~a~^, ~}]}" 
                                 (let ((*read-default-float-format* 'single-float))
                                   (mapcar #'(lambda(point) 
                                               (format nil "~a ~a ~a" 
                                                       (to-single-float (get-x point)) 
                                                       (to-single-float (get-y point)) 
                                                       (to-single-float (get-z point)))) 
                                           normals))) "")
                    
                     (make-grouped-strings vertex-counts vertex-indices)

                     (write-the material-properties-string)))))))))


(defun make-grouped-strings (counts indices)
  (let ((index-array (make-array (list (length indices)) :initial-contents indices))
        result (total 0))
    (declare (type (simple-array fixnum) index-array))
    (dolist (count counts result)
      (let ((indices (subseq index-array total (+ total count))))
        (let ((new-ones (append (coerce indices 'list) (list -1))))
          (if (null result) (setq result new-ones)
            (nconc result new-ones))))
      (setq total (+ total count)))
    (format nil "~{~a ~}" result)))



(defun make-triplets (list)
  (let ((count 0) triplet triplet-list)
    (setq triplet-list 
      (dolist (item list (nreverse triplet-list))
        (if (< count 3)
            (progn (push item triplet) (incf count))
          (progn (push (nreverse triplet) triplet-list)
                 (setq count 1) (setq triplet (list item))))))
    (setq triplet-list (append triplet-list (list (nreverse triplet))))))

(defun make-triplet-strings (list)
  (let ((count 0) triplet strings-list)
    (setq strings-list 
      (dolist (item list (nreverse strings-list))
        (if (< count 3)
            (progn (push item triplet) (incf count))
          (progn (push (format nil "~{~a ~}" (nreverse triplet)) strings-list)
                 (setq count 1) (setq triplet (list item))))))
    (setq strings-list (append strings-list (list (format nil "~{~a ~}" 
                                                          (nreverse triplet)))))))



(define-lens (vrml trimmed-surface)()
  :output-functions
  ((shape
    ()
    (write-the brep shape))))


(define-lens (vrml trimmed-curve)()
  :output-functions
  ((shape 
    ()
    (write-the tess-shape))))      
    
;;
;; FLAG -- this fails for degree 1 non-rationals (polylines).
;;
(define-lens (vrml curve)()
  :output-functions
  ((shape ()
          (with-format-slots (use-bsplines?)
            (if use-bsplines?
                (write-the b-spline-shape)
              (write-the tess-shape))))
   (tess-shape
    ()
    
    (let ((points (if (the %curves-to-draw%)
                      (the (equi-spaced-points *curve-chords*))
                      (remove-duplicates (flatten-lines (the %lines-to-draw%))
                                         :test #'coincident-point?))))

                      
      (when (the closed?) (nconc points (list (first points))))

      (let* ((display-controls (geom-base::find-in-hash self *display-controls*))
             (thickness (getf display-controls :line-thickness))
             (thickness (or thickness (getf (the display-controls) :line-thickness) 1))
             (*read-default-float-format* 'single-float)
             (color-decimal (getf display-controls :color-decimal))
             (color-decimal (or (when color-decimal (coerce color-decimal 'array))
                                (the color-decimal) 
                                (format-slot foreground-color)
                                (lookup-color (the color-decimal)))))

        (if (/= thickness 1)
            (mapc #'(lambda(p1 p2)
                      (unless (coincident-point? p1 p2)
                        (let ((center (midpoint p1 p2)))
                          (let ((p1-local (the (global-to-local p1)))
                                (p2-local (the (global-to-local p2))))


                            
                            (format *stream* "




Transform
{

translation ~a ~a ~a
~a

children
[


 Shape
 {appearance Appearance {material Material{~a}}


  geometry 

   Cylinder {radius ~a
                     height ~a}
   }] 
  }


"
                                    (to-single-float (get-x center))
                                    (to-single-float (get-y center))
                                    (to-single-float (get-z center))

                                    (let ((rotation
                                           (quaternion-to-rotation
                                            (matrix-to-quaternion
                                             (progn
                                               (alignment :front (subtract-vectors p2 p1)
                                                          :top (the (face-normal-vector :top))))))))
                                      (if rotation
                                          (format nil "~%rotation ~a ~a ~a ~a~%"
                                                  (to-single-float (get-x rotation))
                                                  (to-single-float (get-y rotation))
                                                  (to-single-float (get-z rotation))
                                                  (to-single-float (get-w rotation)))
                                        ""))
                                    
                                  
                                    (write-the material-properties-string)
                                    (to-single-float (/ thickness (* 10 (the total-length))))
                                    (to-single-float (3d-distance p1-local p2-local)))))))

                  points (rest points))
        
          (let ((indices (list-of-numbers 0 (1- (length points)))))
            (format *stream* "
Shape
  {geometry IndexedLineSet {coord Coordinate {point [~{~a~^ ~}]}
                            coordIndex [~{~a~^ ~}]
                            color Color {color [~a]}
                            colorPerVertex FALSE
                            colorIndex [0]
                           }
   appearance Appearance {
     material Material {~a}}}
"
                    (let ((*read-default-float-format* 'single-float))
                      (mapcar #'(lambda(point) (let ((point (the (global-to-local point))))
                                                 (format nil "~a ~a ~a" (to-single-float (get-x point)) 
                                                         (to-single-float (get-y point)) 
                                                         (to-single-float (get-z point))))) 
                              points))
                
                    indices

                    (format nil "~a ~a ~a" (get-x color-decimal) (get-y color-decimal)
                            (get-z color-decimal))
              
                    (write-the material-properties-string)))))))
   
   
   (b-spline-shape    ()
                      (format *stream* "

 Shape 
 {geometry
 NurbsCurve { ~&")
                      (format *stream* " knot [")              
                      (dolist (knot (the knot-vector))
                        (format *stream* " ~,6f," knot))
                      (format *stream* " ] ~&")
                      (format *stream* " order ~a ~&" (1+ (the degree)))
                      (format *stream* " controlPoint [ ")
                      (dolist (point (the control-points))
                        (format *stream* " ~,6f ~,6f ~,6f," (get-x point) (get-y point) (get-z point)))
                      (format *stream* " ]~&")
                      (format *stream* " weight [")            
                      (dolist (weight (the  weights))
                        (format *stream* " ~,6f," weight))
                      (format *stream* " ]~&")
                      (format *stream* " tessellation  50 ")
                      (format *stream* "} 
appearance Appearance {
material Material {~a}}" (write-the material-properties-string))
                      
                      (format *stream* "}~%"))))



(define-lens (vrml surface)() :output-functions  
             
             ((shape ()
                     (with-format-slots (use-bsplines?)
                       (if use-bsplines?
                           (write-the b-spline-shape)
                         (write-the tess-shape))))
              
              (tess-shape () (write-the brep shape))
              
              (b-spline-shape 
               ()
               
               (let ((vDimension (length (the control-points)))       
                     (uDimension (length (first (the control-points))))     
                     (uKnot (mapcar 'float (the u-knot-vector)))     
                     (vKnot (mapcar 'float (the v-knot-vector)))        
                     (uOrder (+ 1 (the u-degree)))       
                     (vOrder (+ 1 (the v-degree))) 
                     (controlPoint (flatten (the control-points))) 
                     (weight (flatten (the weights))))
                 (format *stream* "

Shape 
 {geometry
 NurbsSurface { ~&")
                   (format *stream* " uDimension ~a ~&" uDimension) 
                   (format *stream* " vDimension ~a ~&" vDimension) 
                   (format *stream* " uKnot [")        
                   (dolist (point uKnot)
                     (format *stream* " ~,6f," point))
                   (format *stream* " ] ~&")
                   (format *stream* " vKnot [")        
                   (dolist (point vKnot)
                     (format *stream* " ~,6f," point))
                   (format *stream* " ] ~&")
                   (format *stream* " uOrder ~a ~&" uOrder)
                   (format *stream* " vOrder ~a ~&" vOrder)
                   (format *stream* " controlPoint [ ")
                   (dolist (point controlPoint)
                     (format *stream* " ~,6f ~,6f ~,6f," (get-x point) 
                             (get-y point) (get-z point)))      
                   (format *stream* " ]~&")
                   (format *stream* " weight [")               
                   (dolist (point  weight)
                     (format *stream* " ~,6f," point))
                   (format *stream* " ]~&")
                   (format *stream* " uTessellation    100 ~& vTessellation    100 ~& ") 
                   (format *stream* "solid FALSE}~%")
                   
                   (format *stream* "appearance Appearance {
     material Material {~a}}~%" (write-the material-properties-string))

                   (format *stream* "}~%")))))










