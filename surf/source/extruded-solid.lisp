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

(define-object extruded-solid (brep)
  :input-slots ("GDL Curve object. The profile to be extruded into a solid." profile
                
                ("3D Vector. The direction of extrusion. Defaults 
to (the (face-normal-vector :top))" 
                 axis-vector (the (face-normal-vector :top)))
                
                ("Number. The distance to extrude the profile along the axis-vector.
Defaults to (the height)." 
                 distance (the height))
                
                ("Number. Overall tolerance for the created brep solid. Defaults to nil.
Note that a value of nil indicates for SMLib a value of 1.0e-05 of the longest diagonal 
length of the brep."
                 brep-tolerance nil))
  
  
  :computed-slots ((%native-brep%
                    (let ((brep (make-brep *geometry-kernel* 
                                           :tolerance (the brep-tolerance))))            
                      (add-linear-sweep-primitive
                       *geometry-kernel* brep 
                       (if (listp (the profile))
                           (mapcar #'(lambda(curve) 
                                       (the-object curve (copy-new :finalize? nil))) 
                                   (the profile))
                         
                         
                         (list (the profile (copy-new :finalize? nil))))
                       
                       (the axis-vector) (the distance)) 
                      
                      ;;(iwbrep-sew-faces *geometry-kernel* brep)
                                    
                      (tag-edges *geometry-kernel* brep (get-long *geometry-kernel* brep))

                      (unless (the brep-tolerance) 
                        (brep-reset-tolerance *geometry-kernel* brep))
                      brep)))
  
  :hidden-objects
  ((decomposed-profile :type 'decomposed-curves
                       :curve-in (the profile))))

  
