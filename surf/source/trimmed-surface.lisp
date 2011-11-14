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

(define-object trimmed-surface (face)
  
  :input-slots
  (
   
   ("GDL NURBS Surface. The underlying surface to be trimmed."
    basis-surface (the %extracted-basis%))
   
   
   "Single GDL NURBS Curve or list of same. These curves make up the outer trimming loop. Normally should
be in counter-clockwise orientation; if not, please specify <tt>reverse-island?</tt> as non-NIL."
   island 
   
   ("List of GDL NURBS Curves or list of lists of GDL NURBS Curves. These curves make up zero or more holes within the outer trimming loop.
Normally should be in clockwise orientation; if not, please specify <tt>reverse-holes?</tt> as non-NIL."
    holes nil)
   
   (%native-brep%
    (let ((island-3d (mapcar #'(lambda (curve) (make-b-spline-curve* *geometry-kernel*
                                                                     (the-object curve native-curve)
                                                                     :schedule-finalization? nil))
                             (if (listp (the island)) (the island) (list (the island)))))
                         
          (holes-3d (mapcar #'(lambda (hole) 
                                (mapcar #'(lambda (curve) (make-b-spline-curve* *geometry-kernel*
                                                                                (the-object curve native-curve)
                                                                                :schedule-finalization? nil))
                                        (if (listp hole) hole (list hole))))
                            (the holes)))
          (converted-basis 
           (make-b-spline-surface* *geometry-kernel* (the native-surface) :schedule-finalization? nil)
            ;;(the basis-surface native-surface-iw)
            )
                    
          (brep (make-brep *geometry-kernel* :tolerance (the brep-tolerance))))
      (let ((island-uv (when (the uv-inputs) (mapcar #'(lambda (curve) (make-b-spline-curve*-2d *geometry-kernel* curve))
                                                     island-3d)))
            (holes-uv (when (the uv-inputs) 
                        (mapcar #'(lambda (hole) (mapcar #'(lambda (curve) (make-b-spline-curve*-2d *geometry-kernel* curve)) hole)) ;;hack
                                holes-3d))))
        (make-trimmed-face *geometry-kernel* brep 
                           (when (not (the uv-inputs)) (cons island-3d holes-3d))
                           (cons island-uv holes-uv)
                           (cons (if (the reverse-island?) 2 1) 
                                 (make-list (length (the holes)) :initial-element (if (the reverse-holes?) 1 2)))
                           nil converted-basis 1)

        (unless (the brep-tolerance) (brep-reset-tolerance *geometry-kernel* brep))

        brep)))
   
   ("Boolean. NIL if feeding in 3D curves, non-NIL if feeding in UV curves."
    uv-inputs nil)

   ("Boolean. Specify this as non-NIL if the island is given in clockwise orientation. Default is NIL."
    reverse-island? nil)
   
   ("Boolean. Specify this as non-NIL if the holes are given in counter-clockwise orientation. Default is NIL."
    reverse-holes? nil))

  
  :computed-slots
  (("Single GDL 3D NURBS Curve or list of GDL 3D NURBS Curves. These make up the outer trimming loop of the resultant 
trimmed surface. If you specified <tt>island</tt> as an input-slot, normally these should be the same
or very similar. If the trimmed surface was read in from an outside system through a translator such as IGES, 
the <tt>result-island</tt> should return a non-NIL value while the <tt>island</tt> will return NIL."
    result-island (let ((island (list-elements (the %extracted-island%))))
                    (if (consp (rest island)) island (first island))))
   
   ("List of GDL 3D NURBS Curves or list of lists of GDL 3D NURBS Curves. These make up the inner holes of the resultant 
trimmed surface. If you specified <tt>holes</tt> as an input-slot, normally these should be the same
or very similar. If the trimmed surface was read in from an outside system through a translator such as IGES, 
the <tt>result-holes</tt> might return a non-NIL value while the <tt>island</tt> will always return NIL."
    result-holes (let ((holes (list-elements (the %extracted-holes%) (the-element curve-segments))))
                   (if (consp (rest holes)) holes (first holes))))
   
   (%native-face% (first (get-faces-from-brep *geometry-kernel* (the %native-brep%))))
   
   ;;(%curves-to-draw% (the face %curves-to-draw%))
   
   (native-surface-iw (when (the basis-surface) (the basis-surface native-surface-iw)))
   
   
   (%curves-to-draw% (the brep %curves-to-draw%))
   (outline-objects nil )
   
   
   )

  
  :hidden-objects
  (("GDL Brep Object. The Brep containing the face corresponding to this trimmed surface."
    brep :type 'brep
    :pass-down (%native-brep%))
   

  
  )

  
  :documentation (:description "Creates a surface which is trimmed by outer trimming loop curves (the ``island''), and one or more 
hole curves within the outer trimming loop. The curves can be input either as u-v curves or 3D curves.
If 3D curves are given, they must lie on the surface. If not, please use dropped-curve or projected-curve
to ensure that the curves lie on the surface.
<p>

Note that this object mixes in face, which mixes in surface. So this object should answer all the messages
of both face and surface. However, the local surface messages will operate on the basis, not on the trimmed 
representation. The messages for face will operate on the trimmed representation.

<p>
NOTE: the interface for this object is still under development so please stay apprised of any changes."
                  
                  
                  :examples "<pre>

 (in-package :surf)
                  
 (define-object test-trimmed-surface-3 (trimmed-surface)
   :computed-slots
   ((reverse-holes? t)
    (island (the island-container ordered-curves))
    (holes (list (the hole ordered-curves)))
    (display-controls (list :color :periwinkle :line-thickness 2)))
  
   :hidden-objects
   ((basis-surface :type 'test-planar-surface
                   :display-controls (list :color :grey-light-very))
   
    (island-container :type 'global-filleted-polyline-curves
                      :default-radius .05
                      :vertex-list (list (make-point 0 0 0)
                                         (make-point 0.3 0.6 0)
                                         (make-point 0 1 0)
                                         (make-point 1 1 0)
                                         (make-point 1 0 0)
                                         (make-point 0 0 0)))
   
    (island-2 :type 'b-spline-curve 
              :control-points (list (make-point 0 0 0)
                                    (make-point 0 1 0)
                                    (make-point 1 1 0)
                                    (make-point 1 0 0)
                                    (make-point 0 0 0)))
   
    (hole :type 'global-filleted-polyline-curves
          :default-radius .05
          :vertex-list (list (make-point 0.5 0.5 0)
                             (make-point 0.75 0.5 0)
                             (make-point 0.75 0.75 0)
                             (make-point 0.5 0.75 0)
                             (make-point 0.5 0.5 0)))))

 (generate-sample-drawing :objects (make-object 'test-trimmed-surface-3)
                          :projection-direction :trimetric)


</pre>"))













  
