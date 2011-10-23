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

(define-object edge (curve)
  
  :input-slots (%native-edge%
                %native-brep% 
                brep 
                )
  
  
  :computed-slots ((native-curve-iw 
                    
                    (get-bspline-curve-from-edge *geometry-kernel* (the %native-edge%)))
                   
                   
                   ("List of GDL Face objects. The faces connected to this edge."
                    faces (let ((faces-ht (the brep faces-ht)))
                            (mapcar #'(lambda(native-face) (gethash native-face faces-ht))
                                    (the %native-faces%))))
                   
                   
                   (%native-faces% (get-faces-from-edge *geometry-kernel* (the %native-edge%)))
                            
                            
                   
                   
                   (unique-id (get-long *geometry-kernel* self)))
  
  :functions (("GDL Curve object. This represents the UV curve for this edge on the given surface. 
Note that you  have to pass in the surface, which should be the basis-surface of a face connected
to this edge. The GDL edge object will be supplemented with a sequence of faces which are connected
with this edge."
               uv-curve 
               (surface)
               (make-object 'curve 
                            :native-curve-iw 
                            (get-uv-curve-from-edge *geometry-kernel* (the %native-edge%) (the-object surface native-surface-iw))))))

  
  

