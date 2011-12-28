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

#|
(in-package :gdl-user)


(define-object test-planar-section-curve (planar-section-curve)
  
  :input-slots 
  ((
    
    ))
  
  :objects
  ((loft :type 'test-lofted-surface)
   
   (planar-section :type 'planar-section-curve 
                   :surface (the loft)
                   :plane-vector (the (face-normal-vector :right))
                   :plane-point (the center)
                   )))



(define-object test-lofted-surface (lofted-surface)
  
  :input-slots
  ((width 50)
   (length 10)
   
   (display-controls (list :color :blue :transparency 0.5)))

  
  :computed-slots
  ((curves (list (the curve-1) (the curve-2) (the curve-3))))
  
  :hidden-objects
  ((curve-1 :type 'linear-curve
            :start (translate (the center) :left (half (the width)) :rear (half (the length)))
            :end (translate (the-child start) :front (the length)))
   
   (curve-2 :type 'linear-curve
            :start (translate (the center) 
                              :rear (half (the length))
                              :up (half (the length)))
                              
            :end (translate (the center) 
                            :front (half (the length))))

   
   
   (curve-3 :type 'linear-curve
            :start (translate (the center) :right (half (the width)) :rear (half (the length)))
            :end (translate (the-child start) :front (the length)))))


(define-object test-trimmed-surface (trimmed-surface)
  
  :input-slots
  ((width 110) (length 110) (inset 10))

  
  :computed-slots
  (
   (curve-length (- (the length) (the inset)))
   (curve-width (- (the width) (the inset)))
   
   (island (the composed)))
  
  
  :objects
  ((top-curve :type 'linear-curve
              :start (translate (the center) 
                                :rear (half (the curve-length))
                                :right (half (the curve-width)))
              :end (translate (the-child start) :left (the curve-width)))
   
   (left-curve :type 'linear-curve
               :start (the top-curve end)
               :end (translate (the-child start) :front (the curve-length)))
   
   (bottom-curve-1 :type 'linear-curve
                   :start (the left-curve end)
                   :end (translate (the-child start) :right (half (the curve-width))))
   
   (bottom-curve-2 :type 'linear-curve 
                   :start (the bottom-curve-1 end)
                   :end (translate (the-child start) :right (half (the curve-width))))
   
   (bottom-curve :type 'linear-curve 
                 :start (the left-curve end)
                 :end (translate (the-child start) :right (the curve-width)))
   
   (right-curve :type 'linear-curve
                :start (the bottom-curve-2 end)
                :end (the top-curve start))
   
   (composed :type 'composed-curve
             :curves (list (the top-curve)
                           (the left-curve)
                           ;;(the bottom-curve-1)
                           ;;(the bottom-curve-2)
                           (the bottom-curve)
                           (the right-curve)))
   
   (decomposed :type 'decomposed-curves
               :curve-in (the composed))
   
   (projected-curve :type 'projected-curve
                    :curve-in (the composed)
                    :surface (the basis-surface)
                    :projection-vector (the (face-normal-vector :top)))
   
   
   
   (basis-surface :type 'rectangular-surface)))

|#
