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

(in-package :geom-base)

(define-object center-line (outline-specialization-mixin base-object)

  :documentation (:description "Creates a dashed single centerline or crosshair centerline on a circle."
                  :examples "<pre>
 (in-package :gdl-user)

 (define-object center-line-test (base-object)
 
  :objects 
  ((circle-sample :type 'circle
                  :display-controls (list :color :green)
                  :center (make-point 10 10 10 )
                  :radius 10)
   
   (center-line-sample :type 'center-line
                       :circle? t
                       :center (the circle-sample center)
                       :size (* 2.1 (the circle-sample radius)))))

  (generate-sample-drawing 
  :objects (list 
            (the-object (make-object 'center-line-test) 
                        circle-sample) 
            (the-object (make-object 'center-line-test) 
                        center-line-sample))
  :projection-direction (getf *standard-views* :top))
 
 </pre>")
  
  :input-slots
  ("Number. The length of the centerline."
   size
   
   ("Boolean. Determines whether this will be a circle crosshair. Defaults to nil."
    circle? nil)
   
   ("Number. Distance between dashed line segments. Defaults to 0.1."
    gap-length 0.1 :defaulting)
   
   ("Number. Length of longer dashed line segments. Defaults to 1.0."
    long-segment-length 1 :defaulting)
   
   ("Number. Length of shorter dashed line segments. Defaults to 0.25."
    short-segment-length .25 :defaulting))
  
  :trickle-down-slots (gap-length long-segment-length short-segment-length)
  
  :computed-slots (
                   
                   (width (if (the circle?) (the size) 0))
                   (length (the size))
                   (height 0)
                   
                   (%lines-to-draw% 
                    (apply #'append (mapcar #'(lambda(object)
                                                (the-object object %lines-to-draw%))
                                            (the outline-objects))))
                   
                   (outline-objects 
                    (append 
                     (maptree (the crosshair) #'identity #'(lambda(obj)
                                                             (the-object obj leaf?)))
                     (maptree (the straight) #'identity #'(lambda(obj)
                                                            (the-object obj leaf?))))))
                                                     
                                                     
  
  :hidden-objects
  ((crosshair :type (if (the circle?) 'crosshair 'null-part)
              :pass-down (size))
   
   (straight :type (if (the circle?) 'null-part 'segmented-line)
             :length (the size))))


(define-object crosshair (base-object)
  :input-slots (size)
  
  :objects
  ((vertical :type 'segmented-line
             :length (the size))
   
   (horizontal :type 'segmented-line
               :orientation (alignment :right (the (face-normal-vector :rear)))
               :length (the size))))

(define-object segmented-line (base-object)
  
  :input-slots (length)
  
  
  :computed-slots ((remaining-length (half (- (the length) 
                                              (+ (the short-segment-length)
                                                 (the gap-length)))))
                   
                   (pitch (+ (the short-segment-length) (the long-segment-length) 
                             (twice (the gap-length))))
                   
                   (short-long-pairs (floor (/ (the remaining-length) (the pitch))))
                   
                   (pair-starts (let ((start (translate (the center) 
                                                        :rear (+ (the gap-length)
                                                                 (half (the short-segment-length)))))
                                      result)
                                  (dotimes (n (the short-long-pairs) (nreverse result))
                                    (push start result)
                                    (setq start (translate start :rear (the pitch))))))
                   
                   (rear-starts (mapcan #'(lambda(start)
                                             (list start (translate start 
                                                                    :rear (+ (the short-segment-length)
                                                                             (the gap-length)))))
                                         (the pair-starts)))
                   
                   (rear-ends (let ((count -1))
                                (mapcar #'(lambda(start)
                                            (translate start :rear (if (oddp (incf count))
                                                                       (the long-segment-length)
                                                                     (the short-segment-length))))
                                        (the rear-starts))))
                   
                   (last-point (or (lastcar (the rear-ends))
                                   (the middle-line end)))
                   
                   (final-segment? (< (+ (the gap-length)
                                         (3d-distance (the last-point) (the center)))
                                      (half (the length))))
                                      
                   
                   (final-start (when (the final-segment?)
                                  (list (translate (the last-point)
                                                   :rear
                                                   (the gap-length)))))
                   
                   (final-end (when (the final-segment?) 
                                (list (the (edge-center :rear :top)))))
                   
                   
                   (starts (append (reverse 
                                    (mapcar #'(lambda(point) 
                                                (the (mirror-point point))) 
                                            (append (the rear-starts) (the final-start))))
                                   (append (the rear-starts) (the final-start))))
                   
                   (ends (append (reverse 
                                  (mapcar #'(lambda(point) 
                                              (the (mirror-point point))) 
                                          (append (the rear-ends) (the final-end))))
                                 (append (the rear-ends) (the final-end)))))
                                        
  
  :objects
  ((middle-line :type 'line
                :start (translate (the center) :front (half (the short-segment-length)))
                :end (translate (the center) :rear (half (the short-segment-length))))
   
   (extension-lines :type 'line
                    :sequence (:size (length (the starts)))
                    :start (nth (the-child index) (the starts))
                    :end (nth (the-child index) (the ends))))
                   
  :functions
  ((mirror-point 
    (point) 
    (let ((distance (3d-distance (the center) point))
          (vector (subtract-vectors (the center) point)))
      (translate-along-vector point vector (twice distance))))))








