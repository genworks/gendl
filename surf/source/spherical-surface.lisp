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

;;
;; FLAG -- update to do spherical patches.
;;
(define-object spherical-surface (surface sphere)
  
  :documentation (:description "A surface representation of the sphere. Takes the same inputs as native GDL sphere. Partial spheres are not yet implmented.
Note that some VRML browsers, e.g. Cortona v. 4.2, show some spurious artifacts with NURBS 
created as spherical surfaces. BS Contact does not appear to have this problem."

                  :examples "<pre>
                  
 (in-package :surf)

 (define-object test-spherical-surface (spherical-surface)
   :computed-slots ((display-controls (list :color :sky-summer))
                    (radius 10)))

 (generate-sample-drawing :objects (make-object 'test-spherical-surface)
                          :projection-direction :trimetric)

</pre>")
  
  :input-slots
  ((center (make-point 0 0 0) :defaulting))
  
  :computed-slots
  ((native-surface (make-spherical-surface *geometry-kernel* (the center) (the radius) 0 180 
                                           (radians-to-degrees (the end-horizontal-arc)) 2 2))))


(define-object test-spherical-surface (spherical-surface)
   :computed-slots ((radius 10))

   :hidden-objects ((view :type 'base-view
                          :projection-vector (getf *standard-views* :trimetric)
                          :page-width (* 5 72) :page-length (* 5 72)
                          :objects (list self))))


(define-object test-spherical (base-object)
  :objects
  ((spherical :type 'test-spherical-surface)
   
   (view-1 :type 'generic-view 
           :%curves-to-draw% (the spherical %curves-to-draw%))
   
   (view-2 :type 'generic-view 
           :%curves-to-draw% (the spherical brep %curves-to-draw%))))


(define-object generic-view (base-object)
  :input-slots (%curves-to-draw%))


(define-object test-spherical-surface-2 (spherical-surface)
  :computed-slots
  ((radius 10) (end-horizontal-arc (* 3/2 pi))))

