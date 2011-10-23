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

(define-object offset-surface (surface)

  :input-slots
  ("GDL Surface object. The original surface from which to make the offset." surface-in

   "Number. The distance to offset. Positive or negative, depending on which direction you want."
   distance

   ("Keyword symbol, one of :uniform, :chord-length, :centripetal, or :inherited. The parameterization
method used to re-fit the points after offsetting. Defaults to :uniform."
    parameterization :uniform)

   ("Integer. The desired u-degree of the resulting surface. Defaults to the u-degree of the input surface-in."
    u-degree (the surface-in u-degree))

   ("Integer. The desired v-degree of the resulting surface. Defaults to the v-degree of the input surface-in."
    v-degree (the surface-in v-degree))

   ("Number. The tolerance of approximation for the re-fitting of points after the offsetting.
Defaults to *3d-approximation-tolerance-default*."
    approximation-tolerance *3d-approximation-tolerance-default*))


  :computed-slots
  ((native-surface (make-offset-surface  *geometry-kernel*
                                         :surface (the surface-in native-surface)
                                         :offset-distance (the distance)
                                         :u-degree (the u-degree)
                                         :v-degree (the v-degree)
                                         :parameterization (the parameterization)
                                         :tolerance (the approximation-tolerance)))))


(define-object test-fitted-surface (fitted-surface) 

   :input-slots
   ((display-controls (list :color :green-spring :isos (list :n-v 19 :n-u 19)))
   
    (grid-width 4 :settable) (grid-length 4 :settable) (grid-height 4 :settable))
  
   :computed-slots
   (
   
    (points (list (list (make-point 0 0 0)
                        (make-point (/ (the grid-width) 4) 0 0)
                        (make-point (half (the grid-width)) 0 0)
                        (make-point (* 3/4 (the grid-width)) 0 0)
                        (make-point (the grid-width) 0 0))
                         
                  (list (make-point 0 (/ (the grid-length) 4) 0)
                        (make-point (/ (the grid-width) 4) (/ (the grid-length) 4) (/ (the grid-height) 4))
                        (make-point (half (the grid-width)) (/ (the grid-length) 4) 
                                    (* (/ (the grid-height) 4) 1.6))
                        (make-point (* 3/4 (the grid-width)) (/ (the grid-length) 4) (/ (the grid-height) 4))
                        (make-point (the grid-width) (/ (the grid-length) 4) 0))
                         
                  (list (make-point 0 (half (the grid-length)) 0)
                        (make-point (/ (the grid-width) 4) (half (the grid-length)) 
                                    (* (/ (the grid-height) 4) 1.8))
                        (make-point (half (the grid-width)) (half (the grid-length)) (the grid-height))
                        (make-point (* 3/4 (the grid-width)) (half (the grid-length)) (* 3/4 (the grid-height)))
                        (make-point (the grid-width) (half (the grid-length)) 0))
                         
                  (list (make-point 0 (* 3/4 (the grid-length)) 0)
                        (make-point (/ (the grid-width) 4) (* 3/4 (the grid-length)) 
                                    (min (* (/ (the grid-height) 4) (* (/ (the grid-height) 4) 1.4)) 
                                         (the grid-height)))
                        (make-point (half (the grid-width)) (* 3/4 (the grid-length)) 
                                    (min (* (/ (the grid-height) 4) (* (/ (the grid-height) 4) 1.8)) 
                                         (the grid-height)))
                        (make-point (* 3/4 (the grid-width)) (* 3/4 (the grid-length)) 
                                    (/ (the grid-height) 4))
                        (make-point (the grid-width) (* 3/4 (the grid-length)) 0))
                         
                  (list (make-point 0 (the grid-length) 0)
                        (make-point (/ (the grid-width) 4) (the grid-length) 0)
                        (make-point (half (the grid-width)) (the grid-length) 0)
                        (make-point (* 3/4 (the grid-width)) (the grid-length) 0)
                        (make-point (the grid-width) (the grid-length) 0))))))


(define-object test-offset-surface (base-object)

  :objects
  ((original :type 'test-fitted-surface)

   (offset :type 'offset-surface
           :surface-in (the original)
           :distance 0.5)))
