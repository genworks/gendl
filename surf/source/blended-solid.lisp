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

(define-object blended-solid (brep)
  
  :documentation (:description "This primitive attempts to fillet one or more edges of a brep solid."
                  
                  :examples "<pre>
 (in-package :gdl-surf-user)

 (define-object blend-sample (base-object)
   
   :objects
   ((box :type 'box-solid
         :length 10 :width 20 :height 15)
   
    (blend :type 'blended-solid
           :display-controls (list :color :blue)
           :default-radius 3
           :brep (the box))))

 (generate-sample-drawing :objects (the-object (make-object 'blend-sample) blend)
                          :projection-direction (getf *standard-views* :trimetric))

</pre>")
  
  
  :input-slots
  ("GDL Brep object. This is the original solid, whose edges you want to be filleted."
   brep 
   
   "Number. This will be used as the fillet radius."
   default-radius 
   
   ("Plist with key :edges. This specifies which edges are to be filleted. The default (nil) 
means that all edges should be filleted."
    specs nil))
  
  :computed-slots
  ((%native-brep% (let ((brep (make-blended-brep *geometry-kernel* (the brep) :default-radius (the default-radius)
                                                 :specs (the specs))))
                    (tag-edges *geometry-kernel* brep (get-long *geometry-kernel* brep))
                    brep))))


(define-object test-blended-mixin (blended-solid)
  :computed-slots
  ((default-radius 1)
   (display-controls (list :color :green :transparency 0.5))
   (visible-children (list-elements (the edges)))
   
   ))
  

(define-object test-blended-subtracted (test-blended-mixin)
  :computed-slots ((specs (list :edges 
                                (remove-if #'(lambda(edge) (the-object edge unique-id))
                                           (list-elements (the brep edges))))))
  
  :objects
  ((brep :type 'test-subtracted-solid)))

(define-object test-blended-united (test-blended-mixin)
  
  :objects
  ((brep :type 'test-united-solid)))

(define-object test-blended-intersected (test-blended-mixin)

  :objects
  ((brep :type 'test-intersected-solid)))
