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

(define-object transformed-solid (brep)
  
  :documentation (:description "This primitive Translates, Orients, and optionally Scales a brep solid into another brep solid.")
  
  :input-slots ("GDL Brep Object. Source Brep to be copied and transformed."
                brep 
                
                ("GDL Object. Reference Object for from-location and from-orientation. Defaults to the given brep."
                 from-object (the brep)) 
                
                ("3D Point. Reference location from which to translate. Defaults to the from-object center."
                 from-location (the from-object center))
                
                ("3D Point. Reference location to which to translate. Defaults to the from-object center."
                 to-location (the from-object center))
                
                ("3x3 Orientation Matrix or nil. Defaults to the from-object's orientation."
                 from-orientation (the from-object orientation))
                
                ("3x3 Orientation Matrix or nil. Target orientation relative to the from-orientation. Defaults to nil."
                 to-orientation nil)
                
                ("3D Vector or nil. Scale to be applied before transform in each axis, or nil if no scale to be applied."
                 scale nil)
                
                ("Boolean. Controls whether to do the shrink cleanup step after the tranform. Defaults to nil." sew-and-orient? nil)
                
                ("Boolean. Controls whether to do the shrink cleanup step after the tranform. Defaults to nil." shrink? nil)
                
                )
  
  :computed-slots
  ((transform-vector (subtract-vectors (the to-location) (the from-location)))
   
   (transform-matrix (let ((no-transform? (and (null (the to-orientation)) (null (the from-orientation)))))
                       (if no-transform? geom-base::+identity-3x3+
                         (matrix:multiply-matrix (or (the to-orientation) geom-base::+identity-3x3+)
                                                 (matrix:transpose-matrix (or (the from-orientation)
                                                                              geom-base::+identity-3x3+))))))
   
   (%native-brep% (make-transformed-brep *geometry-kernel* (the brep)
                                         :translation (the transform-vector)
                                         :x-vector (the coordinate-base (face-normal-vector :right))
                                         :y-vector (the coordinate-base (face-normal-vector :rear))
                                         :sew-and-orient? (the sew-and-orient?)
                                         :shrink? (the shrink?)
                                         :scale-vector (the scale))))
  
  :hidden-objects
  ((coordinate-base :type 'base-object
                    :orientation (the transform-matrix))))
   

  
  
  
  
