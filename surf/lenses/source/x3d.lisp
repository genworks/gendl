;;
;; Copyright 2002-2011, 2012 Genworks International
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


(define-lens (x3d surface) ()
  :output-functions
  ((shape 
    ()
    (write-the brep shape))))


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
           (formatted-vertices (format nil "~{~a -1 ~}" 
				       (surf::make-triplet-strings vertex-list)))
           (3d-points (mapcar #'(lambda (coord)
                                  (the (global-to-local* coord)))
                              3d-point-list)))
      (cl-who:with-html-output (*stream* nil :indent nil)
        (:Shape
         (:Appearance (progn (if (getf (the display-controls) :pixel-texture)
                                 (write-the pixel-texture)
                               (write-the material-properties))
                             (when (getf (the display-controls) :linetype)
                               (write-the line-properties))))
         (:IndexedFaceSet :creaseAngle "1.571"
                          :solid "false"
                          :coordIndex formatted-vertices
                          (:Coordinate :point (format nil "~{~a~^, ~}" 
                                                      (let ((*read-default-float-format* 'single-float))
                                                        (mapcar #'(lambda(point) 
                                                                    (format nil "~a ~a ~a"
                                                                            (coerce (get-x point) 'single-float)
                                                                            (coerce (get-y point) 'single-float) 
                                                                            (coerce (get-z point) 'single-float)
                                                                            ))
                                                                3d-points)))))))))))


(define-lens (x3d curve)()
  :output-functions
  ((shape
    ()
    (let* ((total-points (floor (the total-length)))
           (points (the (equi-spaced-points total-points)))
	   (3d-points (mapcar #'(lambda(point) (the (global-to-local* point))) points)))
      (let ((indices (list-of-numbers 0 (1- (length points)))))
        (cl-who:with-html-output (*stream* nil :indent nil)
          (:|Shape|
           (:|Appearance| (write-the material-properties))
           (:|IndexedLineSet| :|coordIndex| (format nil "~{~a ~}-1" indices)
                            (:|Coordinate| :|point| (format nil "~{~a~^, ~}"
                                                        (let ((*read-default-float-format* 'single-float))
                                                          (mapcar #'(lambda(point) 
								      (format nil "~a ~a ~a" 
									      (coerce (get-x point) 'single-float)
									      (coerce (get-y point) 'single-float) 
									      (coerce (get-z point) 'single-float)))
								  3d-points))))))))))))


