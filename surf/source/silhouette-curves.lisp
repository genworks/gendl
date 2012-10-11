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


(define-object silhouette-curves (base-object)

  ;;
  ;; FLAG -- detect if surface is a face and take its basis surface.
  ;;
  :input-slots (surface 
                (eye-point-or-vector (the (face-normal-vector :top)))
                (perspective? nil)
                (tolerance (the surface brep adaptive-tolerance))
                (angle-tolerance-radians *angle-tolerance-radians-default*))
  
  :computed-slots 
  ((curves-and-uv-curves 
    (multiple-value-list 
     (surface-create-silhouette-curves  
      *geometry-kernel*
      :surface (if (typep (the surface) 'face)
                   (the surface basis-surface)
                 (the surface))
      :eye-point-or-vector (the eye-point-or-vector)
      :perspective? (the perspective?)
      :tolerance (the tolerance)
      :angle-tolerance (the angle-tolerance-radians))))
                   
   (relevant-curves 
    (remove-if 
     #'(lambda(curve)
         (or (same-direction-vectors? (subtract-vectors (the-object curve end)
                                                        (the-object curve start))
                                      (the eye-point-or-vector))
             (same-direction-vectors? (subtract-vectors (the-object curve end)
                                                        (the-object curve start))
                                      (reverse-vector
                                       (the eye-point-or-vector)))))
     (list-elements (the all-curves))))
                   
                   
   (trimmed-curves-native 
    (when (typep (the surface) 'face)
      (make-trimmed-iso-curves *geometry-kernel*
                               (the surface %native-face%)
                               *display-tolerance*
                               (mapcar #'(lambda(curve)
                                           (the-object curve native-curve-iw))
                                       (list-elements (the curves)))
                               nil))))
  
  :hidden-objects
  ((all-curves 
    :type 'curve
    :sequence (:size (length (first (the curves-and-uv-curves))))
    :native-curve-iw (nth (the-child index) (first (the curves-and-uv-curves)))))
  
  
  :objects
  ((curves :type 'curve
           :hidden? (typep (the surface) 'face)
           :sequence (:size (length (the relevant-curves)))
           :built-from (nth (the-child index) (the relevant-curves)))
   
   (uv-curves :type 'curve
              :hidden? (typep (the surface) 'face)
              :sequence (:size (length (second (the curves-and-uv-curves))))
              :native-curve-iw (nth (the-child index) (second (the curves-and-uv-curves))))
   
   
   (trimmed-curves :type 'curve
                   :hidden? (not (typep (the surface) 'face))
                   :sequence (:size (length (the trimmed-curves-native)))
                   :native-curve-iw (nth (the-child index) (the trimmed-curves-native)))
   ;;
   ;; FLAG -- pass UV curves into this. 
   ;;
   ))


(define-object silhouettes-group ()
  :input-slots (iges-reader eye-point-or-vector)
  
  
  :objects ((silhouettes :type 'silhouette-curves
                         :sequence (:size (the iges-reader breps number-of-elements))
                         :surface (the iges-reader (breps (the-child index)) (faces 0))
                         :pass-down (eye-point-or-vector))
            
            (composed :type 'composed-curves
                      :curves-in (list-elements (the trimmed-curves)))))


(define-object cat-scan-section (planar-section-curves)
  
  :input-slots 
  (plane-base)
  
  :computed-slots ((built-from (the (curves 0)))
                   (min-x 
                    (least 
                     #'get-x 
                     (mapcar #'(lambda(curve)
                                 (getf (the-object curve min-max-x-y-z) :min-x))
                             (list-elements (the curves)))))
                   (max-x 
                    (most 
                     #'get-x 
                     (mapcar #'(lambda(curve)
                                 (getf (the-object curve min-max-x-y-z) :max-x))
                             (list-elements (the curves)))))))

