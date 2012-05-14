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
     (with-error-handling (;;:timeout *brep-vrml-timeout*
				    )
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
                
		 (normals normals)
		 
		 #+nil
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
    (declare (type simple-array index-array))
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

        (if nil ;;(/= thickness 1)
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










