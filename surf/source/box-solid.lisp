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

(define-object box-solid (brep box)
  
  :documentation (:description "A rectangular box represented as a brep solid. Contains the union of messages (e.g. input-slots, computed-slots, etc)
from brep and box."
  
  :examples "<pre>
 (in-package :surf)

 (define-object test-box-solid (box-solid)
    :computed-slots ((length 10) (width 20) (height 30)))

 (generate-sample-drawing :objects (the-object (make-object 'test-box-solid) )
                          :projection-direction (getf *standard-views* :trimetric))

</pre>")


  :computed-slots (
		   ;;
		   ;; FLAG -- should be able to call to base-object's version of this instead of repeating code here. 
		   ;;
		   (local-box (let ((vertices (let (vertices) (dolist (lateral '(:left :right) vertices)
								(dolist (longitudinal '(:front :rear))
								  (dolist (vertical '(:bottom :top))
								    (push (the (vertex lateral longitudinal vertical)) vertices)))))))
				(let (xmin ymin zmin xmax ymax zmax)
				  (mapc #'(lambda(vertex)
					    (let ((x (get-x vertex)) (y (get-y vertex)) (z (get-z vertex)))
					      (when (or (null xmin) (< x xmin)) (setq xmin x))
					      (when (or (null xmax) (> x xmax)) (setq xmax x))
					      (when (or (null ymin) (< y ymin)) (setq ymin y))
					      (when (or (null ymax) (> y ymax)) (setq ymax y))
					      (when (or (null zmin) (< z zmin)) (setq zmin z))
					      (when (or (null zmax) (> z zmax)) (setq zmax z)))) vertices)
				  (list (make-point xmin ymin zmin) (make-point xmax ymax zmax)))))

		    
		   (%native-brep% (let ((brep (make-brep  *geometry-kernel* 
							 :tolerance (the brep-tolerance))))
                                    (let ((x-vector (the (face-normal-vector :right)))
                                          (y-vector (the (face-normal-vector :rear)))
                                          (corner (the (vertex :left :front :bottom))))
                                      
                                      (let ((origin-x (get-x corner))
                                            (origin-y (get-y corner))
                                            (origin-z (get-z corner)))
                                        
                                        
                                        (add-box-primitive  *geometry-kernel* brep 
                                                           (the width)(the length)(the height)
                                                           origin-x origin-y origin-z
                                                           (get-x x-vector) (get-y x-vector) (get-z x-vector)
                                                           (get-x y-vector) (get-y y-vector) (get-z y-vector)))
                                      
                                      (unless (the brep-tolerance)
                                        (brep-reset-tolerance  *geometry-kernel* brep))

                                      brep)))))



;; (define-object test-box-solid (box-solid)
;;  :computed-slots ((length 10) (width 20) (height 30)))
