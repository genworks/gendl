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

(define-object face (surface)
  
  :documentation (:description "This object represents a (possibly) trimmed surface contained within a brep. It answers
all the local messages of face, and it has surface mixed in as well, so it will answer all the surface messages. Note 
however that the local surface messages will operate on the basis, not on the trimmed representation. The messages 
for face will operate on the trimmed representation.

This object is not meant for direct instantiation; rather, a brep will contain a quantified set of faces (called \"faces\"),
and trimmed surface also mixes in face, so a trimmed-surface will answer all the face messages.")
  
  :input-slots (%native-face% 
                
                %native-brep% ;; FLAG -- should default to get-brep-from-face of the %native-face%.
                
                "GDL Brep object. This is the brep object which contains this face object."
                brep ;; FLAG -- should default to a new brep constructed from the %native-brep%
                
                ("GDL NURBS Surface. The underlying surface, before any trimming takes place."
                 basis-surface (the %extracted-basis%)))

  
  :computed-slots
  (
   (outline-objects nil)
   (native-surface-iw (the %extracted-basis% native-surface-iw))
   (%native-island-curves% (extract-face-island-curves *geometry-kernel* (the %native-face%)))
   (%native-hole-curves% (extract-face-hole-curves *geometry-kernel* (the %native-face%)))
   

   (%lines-to-draw% (progn (append 
                             (append-elements (the %reconstructed-trimmed-iso-curves%)
                                              (the-element %lines-to-draw%))
                             (append-elements (the %extracted-island%) (the-element %lines-to-draw%))
                             (apply #'append (mapcar #'(lambda(hole) (the-object hole %lines-to-draw%))
                                                     (list-elements (the %extracted-holes%)))))))
   
   (%curves-to-draw% (progn (append 
                             
                             (append-elements (the %reconstructed-trimmed-iso-curves%) 
                                              (the-element %curves-to-draw%))
                             
                             (append-elements (the %extracted-island%) (the-element %curves-to-draw%))
                             
                             (apply #'append (mapcar #'(lambda(hole) (the-object hole %curves-to-draw%))
                                                     (list-elements (the %extracted-holes%)))))))
   
   
   
   
   (bounding-box (the basis-surface bounding-box))
   
   
   (u-iso-curves (the basis-surface u-iso-curves))
   (v-iso-curves (the basis-surface v-iso-curves))
   
   (%reconstructed-trimmed-iso-curves-native%
    (make-trimmed-iso-curves *geometry-kernel* (the %native-face%) *display-tolerance*
                             (mapcar #'(lambda (curve) (make-b-spline-curve* *geometry-kernel* curve 
                                                                             :schedule-finalization? t
                                                                             ))
                                     (append (list-elements (the u-iso-curves) (the-element native-curve))
                                             (list-elements (the v-iso-curves) (the-element native-curve))))
                             nil))
   
   (%native-edges% (get-edges-from-face *geometry-kernel* (the %native-face%)))
   
   (brep-id (the brep unique-id))
   
   )

  
  :hidden-objects
  ((%extracted-basis%
    :type 'surface
    :display-controls (when (the brep isos) (list :isos (the brep isos))) ;;(list :isos (list :n-u 8 :n-v 8))
    :native-surface-iw (get-surface-from-face *geometry-kernel* (the %native-face%)))
   
   (%extracted-island%
    :type 'curve
    :sequence (:size (length (the %native-island-curves%)))
    :native-curve-iw (nth (the-child index) (the %native-island-curves%)))
   
   (%extracted-holes%
    :type 'native-curve-sequence
    :sequence (:size (length (the %native-hole-curves%)))
    :native-curves (nth (the-child index) (the %native-hole-curves%)))
   
   (%reconstructed-trimmed-iso-curves%
    :type 'curve
    :sequence (:size (length (the %reconstructed-trimmed-iso-curves-native%)))
    :native-curve-iw (nth (the-child index) (the %reconstructed-trimmed-iso-curves-native%)))
   
   ("Sequence of GDL Edge Objects. The Edges contained within this brep."
    edges :type 'edge
    :sequence (:size (length (the %native-edges%)))
    :brep (the brep)
    :%native-brep% (the %native-brep%)
    :%native-edge% (nth (the-child index) (the %native-edges%)))
   
   )
  
  :functions
  ((trim-curves-to-face
    (curves) (make-trimmed-iso-curves *geometry-kernel* (the %native-face%) 0 curves nil))
   
   
   ("Number. Returns the area of the face. 

:&key ((desired-accuracy 0.000001) \"Number. Desired accuracy of result. Should not be smaller than 0.00000001 (10e-08).\"
       (face-thickness 0.0) \"Number. Optional thickness.\")"
    area 
    (&key (desired-accuracy 0.000001) (face-thickness 0.0))
    (multiple-value-bind (area volume moments)
        (the (precise-properties :desired-accuracy desired-accuracy :face-thickness face-thickness))
      (declare (ignore moments volume)) area))
   
   
   ("Number. Returns the volume of the face. 

:&key ((desired-accuracy 0.000001) \"Number. Desired accuracy of result. Should not be smaller than 0.00000001 (10e-08).\"
       (face-thickness 0.0) \"Number. Optional thickness.\")"
    volume 
    (&key (desired-accuracy 0.000001) (face-thickness 0.0))
    (multiple-value-bind (area volume moments)
        (the (precise-properties :desired-accuracy desired-accuracy :face-thickness face-thickness))
      (declare (ignore moments area)) volume))
   
   
   ("Plist. Returns the moments of the face. The plist contains keys: :area-static-moments, :area-moments-of-inertia,  :area-products-of-inertia, :area-second-moment-about-coordinate-axii, :volume-static-moments,  :volume-moments-of-inertia, :volume-products-of-inertia, and  :volume-second-moment-about-coordinate-axii.

:&key ((desired-accuracy 0.000001) \"Number. Desired accuracy of result. Should not be smaller than 0.00000001 (10e-08).\"
       (face-thickness 0.0) \"Number. Optional thickness.\")"
    moments 
    (&key (desired-accuracy 0.000001) (face-thickness 0.0))
    (multiple-value-bind (area volume moments)
        (the (precise-properties :desired-accuracy desired-accuracy :face-thickness face-thickness))
      (declare (ignore area volume)) moments))
   
   ("3D Vector (ie 3D Point). Returns the Area Static Moments of the face.

:&key ((desired-accuracy 0.000001) \"Number. Desired accuracy of result. Should not be smaller than 0.00000001 (10e-08).\"
       (face-thickness 0.0) \"Number. Optional thickness.\")"
    area-static-moments
    (&key (desired-accuracy 0.000001) (face-thickness 0.0))
    (getf (the (moments :desired-accuracy desired-accuracy :face-thickness face-thickness)) :area-static-moments))
   
   ("3D Vector (ie 3D Point). Returns the Area Moments of Inertia of the face.

:&key ((desired-accuracy 0.000001) \"Number. Desired accuracy of result. Should not be smaller than 0.00000001 (10e-08).\"
       (face-thickness 0.0) \"Number. Optional thickness.\")"
    area-moments-of-inertia
    (&key (desired-accuracy 0.000001) (face-thickness 0.0))
    (getf (the (moments :desired-accuracy desired-accuracy :face-thickness face-thickness)) :area-moments-of-inertia))
      
   ("3D Vector (ie 3D Point). Returns the Area Products of Inertia of the face.

:&key ((desired-accuracy 0.000001) \"Number. Desired accuracy of result. Should not be smaller than 0.00000001 (10e-08).\"
       (face-thickness 0.0) \"Number. Optional thickness.\")"
    area-products-of-inertia
    (&key (desired-accuracy 0.000001) (face-thickness 0.0))
    (getf (the (moments :desired-accuracy desired-accuracy :face-thickness face-thickness)) :area-products-of-inertia))
   
   ("3D Vector (ie 3D Point). Returns the Area Second Moment About Coordinate Axii of the face.

:&key ((desired-accuracy 0.000001) \"Number. Desired accuracy of result. Should not be smaller than 0.00000001 (10e-08).\"
       (face-thickness 0.0) \"Number. Optional thickness.\")"
    area-second-moment-about-coordinate-axii
    (&key (desired-accuracy 0.000001) (face-thickness 0.0))
    (getf (the (moments :desired-accuracy desired-accuracy :face-thickness face-thickness)) :area-second-moment-about-coordinate-axii))
   
   ("3D Vector (ie 3D Point). Returns the Volume Static Moments of the face.

:&key ((desired-accuracy 0.000001) \"Number. Desired accuracy of result. Should not be smaller than 0.00000001 (10e-08).\"
       (face-thickness 0.0) \"Number. Optional thickness.\")"
    volume-static-moments
    (&key (desired-accuracy 0.000001) (face-thickness 0.0))
    (getf (the (moments :desired-accuracy desired-accuracy :face-thickness face-thickness)) :volume-static-moments))
   
   ("3D Vector (ie 3D Point). Returns the Volume Moments of Inertia of the face.

:&key ((desired-accuracy 0.000001) \"Number. Desired accuracy of result. Should not be smaller than 0.00000001 (10e-08).\"
       (face-thickness 0.0) \"Number. Optional thickness.\")"
    volume-moments-of-inertia
    (&key (desired-accuracy 0.000001) (face-thickness 0.0))
    (getf (the (moments :desired-accuracy desired-accuracy :face-thickness face-thickness)) :volume-moments-of-inertia))
   
   ("3D Vector (ie 3D Point). Returns the Volume Products of Inertia of the face.

:&key ((desired-accuracy 0.000001) \"Number. Desired accuracy of result. Should not be smaller than 0.00000001 (10e-08).\"
       (face-thickness 0.0) \"Number. Optional thickness.\")"
    volume-products-of-inertia
    (&key (desired-accuracy 0.000001) (face-thickness 0.0))
    (getf (the (moments :desired-accuracy desired-accuracy :face-thickness face-thickness)) :volume-products-of-inertia))

   
   ("3D Vector (ie 3D Point). Returns the Volume Second Moment about Coordinate Axii of the face.

:&key ((desired-accuracy 0.000001) \"Number. Desired accuracy of result. Should not be smaller than 0.00000001 (10e-08).\"
       (face-thickness 0.0) \"Number. Optional thickness.\")"
    volume-second-moment-about-coordinate-axii
    (&key (desired-accuracy 0.000001) (face-thickness 0.0))
    (getf (the (moments :desired-accuracy desired-accuracy :face-thickness face-thickness)) :volume-second-moment-about-coordinate-axii))
   
   
   ("Multiple values: Number, Number, and Plist. Returns the area, volume, and moments of the face. 
The moments are labeled as: :area-static-moments, :area-moments-of-inertia, :area-products-of-inertia, :area-second-moment-about-coordinate-axii, :volume-static-moments, :volume-moments-of-inertia, :volume-products-of-inertia, and  :volume-second-moment-about-coordinate-axii.

:&key ((desired-accuracy 0.000001) \"Number. Desired accuracy of result. Should not be smaller than 0.00000001 (10e-08).\"
       (face-thickness 0.0) \"Number. Optional thickness of the trimmed surface.\")"
    precise-properties 
    (&key (desired-accuracy 0.000001) (origin (make-point 0 0 0))
          (estimated-area 0.0) (face-thickness 0.0))
    (face-compute-precise-properties *geometry-kernel* (the %native-face%) desired-accuracy origin estimated-area face-thickness))))



(define-object native-curve-sequence (base-object)
  :input-slots (native-curves)
  
  :computed-slots ((curve-segments (list-elements (the curves)))
                   (%curves-to-draw% (append-elements (the curves) (the-element %curves-to-draw%))))
  
  :objects
  ((curves :type 'curve
           :sequence (:size (length (the native-curves)))
           :native-curve-iw (nth (the-child index) (the native-curves)))))

