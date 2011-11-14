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

(define-object cylinder-solid (brep cylinder)
  
  :documentation (:description "A right cylinder represented as a brep solid. Contains the union of messages (e.g. input-slots, computed-slots, etc)
from brep and cylinder"
                  
                :examples "<pre>
 (in-package :surf)

 (define-object test-cone-solid (cone-solid)
   :computed-slots
   ((display-controls (list :isos (list :n-u 8 :n-v 8) :color :green))
    (length 100) (radius-1 10) (radius-2 20)))
 
 (generate-sample-drawing :objects (the-object (make-object 'test-cone-solid))
                          :projection-direction (getf *standard-views* :trimetric))

</pre>")  

  :input-slots
  ()
  
   :computed-slots ((%native-brep% (if (typep (the inner-cylinder) 'null-part)
                                       (the outer-cylinder %native-brep%) (the subtract %native-brep%))))
  
   :hidden-objects
   ((outer-cylinder :type (if (and  (the :radius-1) (the :radius-2)) 'simple-cone-solid 'simple-cylinder-solid)
                    :pass-down (radius radius-1 radius-2 arc))
   
   
    (inner-cylinder :type (cond ((and (the :inner-radius-1) (the :inner-radius-2)) 'simple-cone-solid)
                                ((the inner-radius) 'simple-cylinder-solid)
                                (t 'null-part))
                    :radius (the :inner-radius)
                    :radius-1 (the :inner-radius-1)
                    :radius-2 (the :inner-radius-2)
                    :pass-down (arc))

   
    (subtract :type (if (typep (the inner-cylinder) 'null-part) 'null-part
                        'subtracted-solid)
              :brep (the outer-cylinder)
              :other-brep (the inner-cylinder))))


(define-object simple-cylinder-solid (brep cylinder)
  :computed-slots
  ((%native-brep% (let ((brep (make-brep *geometry-kernel* :tolerance (the brep-tolerance))))
                    (let ((x-vector (the (face-normal-vector :right)))
                          (y-vector (the (face-normal-vector :bottom)))
                          (corner (the (face-center :front))))
                      (let ((origin-x (get-x corner))
                            (origin-y (get-y corner))
                            (origin-z (get-z corner)))
                        (add-cone-primitive *geometry-kernel* brep (the length) (the radius-1) (the radius-2) 0 (the arc)
                                            origin-x origin-y origin-z
                                            (get-x x-vector) (get-y x-vector) (get-z x-vector)
                                            (get-x y-vector) (get-y y-vector) (get-z y-vector))))

                    (unless (the brep-tolerance) (brep-reset-tolerance *geometry-kernel* brep))
                    
                    brep))))
                                    

(define-object cone-solid (cylinder-solid cone)
  :documentation (:description "A right cone represented as a brep solid. Contains the union of messages (e.g. input-slots, computed-slots, etc)
from brep and cone."
                  :examples "<pre>
                  
 (in-package :surf)

 (define-object test-cone-solid-hollow ()
  
  :objects
  ((cone-solid :type 'cone-solid
               :display-controls (list :isos (list :n-u 8 :n-v 8) :color :green)
               :length 100 
               :radius-1 10 
               :radius-2 20
               :inner-radius-1 8 
               :inner-radius-2 16)))
 
 (generate-sample-drawing :object-roots (make-object 'test-cone-solid-hollow)
                          :projection-direction (getf *standard-views* :trimetric))
 
 </pre>"
 ))


(define-object simple-cone-solid (simple-cylinder-solid cone))


;;(define-object test-cone-solid (cone-solid)
;;  :computed-slots
;;  ((display-controls (list :color :green))
;;   (length 100) (radius-1 10) (radius-2 20)))


(define-object test-cone-solid-hollow ()
  
  :objects
  ((cone-solid :type 'cone-solid
               :display-controls (list :color :green)
               :length 100 
               :radius-1 10 
               :radius-2 20
               :inner-radius-1 8 
               :inner-radius-2 16)))



