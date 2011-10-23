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


(define-object poly-brep (geometry-kernel-object-mixin base-object)
  
  :documentation (:description "A polygonal representation for a boundary representation geometric entity.")
  
  
  :input-slots (brep max-chord-height max-angle-degrees
                max-3d-edge max-aspect-ratio smooth-results?
                (native-pointer nil)
                )

  
  :computed-slots
  ((%lines-to-draw% (let* ((poly-brep-data (the mesh-data))
                           (vertex-counts (getf poly-brep-data :vertex-counts))
                           (vertex-indices (coerce (getf poly-brep-data :vertex-indices) 'vector))
                           (3d-points (coerce (getf poly-brep-data :3d-points) 'vector))
                           (total 0))
                      (mapcan #'(lambda(count)
                                  (let ((indices (subseq vertex-indices total (+ total count))))
                                    (let ((result
                                           (append 
                                            (map 'list 
                                              #'(lambda(p1 p2)
                                                  (list p1 p2))
                                              (map 'list #'(lambda(index) (svref 3d-points index))
                                                      indices)
                                              (map 'list #'(lambda(index) (svref 3d-points index))
                                                      (subseq indices 1)))
                                            (list (list (svref 3d-points (svref indices (1- count)))
                                                        (svref 3d-points (svref indices 0)))))))
                                      (incf total count)
                                      result)))
                              vertex-counts)))
   
   ("Plist. Contains mesh data from the poly brep."
    mesh-data (the get-brep-mesh))
   
   (%native-poly-brep% (or (the native-pointer)
                           (make-poly-brep *geometry-kernel* (the brep %native-brep%) 
                                           :dCHTol (the max-chord-height)
                                           :dCrvTessAngle (the max-angle-degrees)
                                           :dSrfTessAngle (the max-angle-degrees)
                                           :dMax3DEdge (the max-3d-edge)
                                           :dMaxAspect (the max-aspect-ratio)
                                           :bSmoothResults (if (the smooth-results?) 1 0)))))
   
  

    
  
  :functions
  ((get-brep-mesh 
    ()
    (poly-brep-get-brep-mesh *geometry-kernel* (the %native-poly-brep%)))
   
   (write-stl-file 
    (file-name &key (format :ascii))
    (poly-brep-write-to-stl-file *geometry-kernel* 
                                 (the %native-poly-brep%) file-name format))))
    



