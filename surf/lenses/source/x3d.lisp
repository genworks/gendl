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

(define-format x3d (vrml))

(define-lens (x3d brep)()
  :output-functions
  (
   (shape
    ()
    (write-the tri-shape))
   
   (tri-shape
    ()
    (mapcar #'(lambda (face)
                (write-the (shape-face face)))
            (the triangle-data)))
   
   (shape-face
    (face)
    (let* ((vertex-list (coerce (getf face :vertex-indices) 'list))
           (3d-point-list (coerce (getf face :3d-points) 'list))
           (formatted-vertices (format nil "~{~a -1 ~}" (surf::make-triplet-strings vertex-list)))
           (3d-points (mapcar #'(lambda (coord)
                                  (the (global-to-local coord)))
                              3d-point-list)))
      (cl-who:with-html-output (*stream* nil :indent t)
        (:Shape
         (:Appearance (progn (if (getf (the display-controls) :pixel-texture)
                                 (write-the pixel-texture)
                               (write-the material-properties))
                             (when (getf (the display-controls) :linetype)
                               (write-the line-properties))))
         (:IndexedFaceSet :creaseAngle "1.0"
                          :solid "false"
                          :coordIndex formatted-vertices
                          (:Coordinate :point (format nil "~{~a~^, ~}" 
                                                      (let ((*read-default-float-format* 'single-float))
                                                        (mapcar #'(lambda(point) (format nil "~a ~a ~a"
                                                                                         (coerce (get-x point) 'single-float)
                                                                                         (coerce (get-y point) 'single-float) 
                                                                                         (coerce (get-z point) 'single-float)
                                                                                         ))
                                                                3d-points)))))))))))



(define-lens (x3d base-object)()
  :output-functions
  (
   (header
    ()
    (format *stream* "~a~%~a~%"
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            "<!DOCTYPE X3D PUBLIC \"ISO//Web3D//DTD X3D 3.1//EN\" \"http://www.web3d.org/specifications/x3d-3.1.dtd\">"))
   
   (rgb-color
    (color)
    (when color
      (let ((fcolor (coerce (lookup-color color) 'list)))
        (format nil "~,3f ~,3f ~,3f" (nth 0 fcolor) (nth 1 fcolor) (nth 2 fcolor)))))
   
   (pixel-texture
    ()
    (cl-who:with-html-output (*stream* nil :indent nil)
      (:PixelTexture :image (getf (the display-controls) :pixel-texture))))
   
   (line-properties
    ()
    (cl-who:with-html-output (*stream* nil :indent nil)
      (:LineProperties :linetype (getf (the display-controls) :linetype))))
   
   (material-properties
    ()
    (cl-who:with-html-output (*stream* nil :indent nil)
      (:Material :diffuseColor (write-the (rgb-color (if (getf (the display-controls) :color)
                                                         (getf (the display-controls) :color)
                                                       :black)))
                 :ambientIntensity (getf (the display-controls) :ambient-intensity)
                 :emissiveColor (write-the (rgb-color (getf (the display-controls) :emissive-color)))
                 :shininess (getf (the display-controls) :shininess)
                 :specularColor (write-the (rgb-color (getf (the display-controls) :specular-color)))
                 :transparency (getf (the display-controls) :transparency)
                 )))
   
   (appearance
    ()
    (cl-who:with-html-output (*stream* nil :indent nil)
      (write-the pixel-texture)
      (write-the material-properties)))
   
   (transform
    ()
    (let* ((rotation (quaternion-to-rotation (matrix-to-quaternion (the orientation))))
           (x (get-x (the center))) 
           (y (get-y (the center))) 
           (z (get-z (the center))))
      
      (cl-who:with-html-output (*stream* nil :indent nil)
        (:Transform :translation (when (not (every #'zerop (list x y z)))
                                   (format nil "~3,7f ~3,7f ~3,7f" x y z))
                    :rotation (when rotation
                                (format nil "~3,7f ~3,7f ~3,7f ~3,7f" (get-x rotation) (get-y rotation) (get-z rotation) (get-w rotation)))
                    (:Group (write-the shape))))))
   
   (scene 
    ()
    (cl-who:with-html-output (*stream* nil :indent nil)
      (:X3D :profile "Immersive"
            :version "3.1" 
            :xmlns\:xsd "http://www.w3.org/2001/XMLSchema-instance"
            :xsd\:noNamespaceSchemaLocation "http://www.web3d.org/specifications/x3d-3.1.xsd"
            (:Scene (:Transform (:MetadataString :name "fieldID" 
                                                 ;;:value (the x3d-node-id)
                                                 :value (the root-path-string)
                                                 :containerField "metadata")
                                (:Group (write-the x3d-output-tree)))))))

   (cad-output 
    (&key (header? t))
    (write-the (x3d-output :header? header?)))
   
   (cad-output-tree
    (&key (header? t))
    (write-the (x3d-output-tree :header? header?)))
   
   (x3d-output
    (&key (header? t))
    
    (when header? (write-the header))
    (write-the scene))
   
   (x3d-output-tree
    (&key (header? t))
    (when header? (write-the header))
    (cond ((typep self 'outline-specialization-mixin)
           (cl-who:with-html-output (*stream* nil :indent t)
             (:Transform (:Group (mapcar #'(lambda (outline-object)
                                             (write-the-object outline-object x3d-output-tree))
                                         (the outline-objects))))))
          ((the leaf?)
           (write-the transform))
          (t (mapcar #'(lambda(child)
                         (write-the-object child x3d-output-tree))
                     (the children)))))
   ))


(define-lens (x3d base-drawing)()
  :output-functions
  ((cad-output
    ()
    (let ((req (when (find-package :gwl) (symbol-value (read-from-string "gwl:*req*")))))
      (let ((keep-alive? (and req (slot-value req (read-from-string "net.aserve::request-header-connection"))))
            (protocol (and req (slot-value req (read-from-string "net.aserve::protocol")))))
          
        ;;(when *debug?* (describe req) (print-variables self keep-alive? protocol))
          
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



(define-lens (x3d cone)()
  :output-functions
  (
   (shape
    ()
    (cl-who:with-html-output (*stream* nil :indent nil)
      (:Shape
       (:Appearance (write-the material-properties))
       (:Cone :bottomRadius (the radius-1)
              :height (the height)
              :bottom (the closed?)))))
   )
  )

(define-lens (x3d sphere)()
  :output-functions
  (
   (shape
    ()
    (cl-who:with-html-output (*stream* nil :indent nil)
      (:Shape
       (:Appearance (if (getf (the display-controls) :pixel-texture)
                        (write-the pixel-texture)
                      (write-the material-properties)))
       (:Sphere :radius (the radius)))))
   )
  )

(define-lens (x3d cylinder)()
  :output-functions
  (
   (shape
    ()
    (cl-who:with-html-output (*stream* nil :indent nil)
      (:Shape
       (:Appearance (write-the material-properties))
       (:Cylinder :radius (the radius) 
                  :height (the length)))))
   )
  )

(define-lens (x3d box)()
  :output-functions
  (
   (shape
    ()
    (cl-who:with-html-output (*stream* nil :indent nil)
      (:Shape
       (:Appearance (write-the material-properties))
       (:Box :size (format nil "~a ~a ~a" (the width) (the length) (the height))))))
   )
  )

(define-lens (x3d global-polyline)()
  :output-functions
  (
   (shape
    ()
    (cl-who:with-html-output (*stream* nil :indent nil)
      (:Shape
       (:Appearance (progn (when (getf (the display-controls) :linetype)
                             (write-the line-properties))
                           (write-the material-properties)))
       (:IndexedLineSet :coordIndex (format nil "~{~a ~}-1" (list-of-numbers 0 (1- (length (the vertex-list)))))
                        (:Coordinate :point (format nil "~{~a~^, ~}" 
                                                    (let ((*read-default-float-format* 'single-float))
                                                      (mapcar #'(lambda(point) (format nil "~a ~a ~a"
                                                                                       (coerce (get-x point) 'single-float)
                                                                                       (coerce (get-y point) 'single-float) 
                                                                                       (coerce (get-z point) 'single-float)
                                                                                       ))
                                                              (the vertex-list)))))))))
   )
  )



(define-lens (x3d curve)()
  :output-functions
  ((shape
    ()
    (let* ((total-points (floor (the total-length)))
           (points (the (equi-spaced-points total-points))))
      (let ((indices (list-of-numbers 0 (1- (length points)))))
        (cl-who:with-html-output (*stream* nil :indent nil)
          (:Shape
           (:Appearance (write-the material-properties))
           (:IndexedLineSet :coordIndex (format nil "~{~a ~}-1" indices)
                            (:Coordinate :point (format nil "~{~a~^, ~}"
                                                        (let ((*read-default-float-format* 'single-float))
                                                          (mapcar #'(lambda(point) 
                                                                      (let ((point (the (global-to-local point))))
                                                                        (format nil "~a ~a ~a" 
                                                                                (coerce (get-x point) 'single-float)
                                                                                (coerce (get-y point) 'single-float) 
                                                                                (coerce (get-z point) 'single-float)
                                                                                ))) points))))))))))))

