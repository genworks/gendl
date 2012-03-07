;;
;; Copyright 2002, 2009 Genworks International and Genworks BV 
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

(in-package :gdl-lift-tests)

(define-object trimmed-surface-test (trimmed-surface)
  
  :computed-slots
  ((basis-surface (the b-spline-surface))
   (holes (list (the hole)))
   (uv-inputs t)
   (reverse-island? t)
   (reverse-holes? t)
   (points-data '(((0 0 0)(4 1 0)(8 1 0)(10 0 0)(8 -1 0)(4 -1 0)(0 0 0))
                   ((0 0 2) (4 2 2) (8 2 2) (10 0 2) (8 -2 2) (4 -2 2) (0 0 2))
                   ((0 0 4) (4 2 4) (8 2 4) (10 0 4) (8 -2 4) (4 -2 4) (0 0 4))
                  ((0 0 7) (4 1 7) (8 1 7) (10 0 7) (8 -1 7) (4 -1 7) (0 0 7))))
   
   
   (regression-test-data (append (multiple-value-list (the precise-properties))
                                 (the %curves-to-draw%)
                                 (the %lines-to-draw%)))
   
   
   )
  
  :objects
  ((b-spline-surface :type 'b-spline-surface
                     :control-points (mapcar #'(lambda(list) (mapcar #'apply-make-point list)) (the points-data)))
   
   (island :type 'b-spline-curve
           :control-points (list (make-point 0 0 0)
                                 (make-point 0 1 0)
                                 (make-point 1 1 0)
                                 (make-point 1 0 0)
                                 (make-point 0 0 0)))
                   
   (hole :type 'b-spline-curve 
         :control-points (list (make-point 0.5 0.5 0)
                               (make-point 0.5 0.75 0)
                               (make-point 0.75 0.75 0)
                               (make-point 0.75 0.5 0)
                               (make-point 0.5 0.5 0)))))


(register-test-definition 'trimmed-surface-test)


(define-object test-trimmed-surface (trimmed-surface)
  :computed-slots
  ((holes (list (the hole)))
   (uv-inputs t)
   (reverse-island? t)
   (reverse-holes? t)
   (display-controls (list :color :periwinkle :line-thickness 2)))
  
  :hidden-objects 
  ((basis-surface :type 'test-b-spline-surface)
                   
   (island :type 'b-spline-curve
           :control-points (list (make-point 0 0 0)
                                 (make-point 0 1 0)
                                 (make-point 1 1 0)
                                 (make-point 1 0 0)
                                 (make-point 0 0 0)))
                   
   (hole :type 'b-spline-curve 
         :control-points (list (make-point 0.5 0.5 0)
                               (make-point 0.5 0.75 0)
                               (make-point 0.75 0.75 0)
                               (make-point 0.75 0.5 0)
                               (make-point 0.5 0.5 0)))
                   
   (view :type 'base-view
         :projection-vector (getf *standard-views* :trimetric)
         :width (* 5 72) :length (* 5 72)
         :objects (list self))))


#+nil
(define-object test-trimmed-surface-2 (trimmed-surface)
  :computed-slots
  ((uv-inputs t)
   (reverse-holes? t)
   (island (the island-container ordered-curves))
   (holes (list (the hole ordered-curves)))
   (display-controls (list :color :periwinkle :line-thickness 2)))

  :hidden-objects 
  ((basis-surface :type 'test-planar-surface)
                   
   (island-container :type 'global-filleted-polyline-curves
                     :default-radius 0.05
                     :vertex-list (list (make-point 0 0 0)
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
         :default-radius 0.05
         :vertex-list (list (make-point 0.5 0.5 0)
                            (make-point 0.75 0.5 0)
                            (make-point 0.75 0.75 0)
                            (make-point 0.5 0.75 0)
                            (make-point 0.5 0.5 0)))
                   
   (view :type 'base-view
         :projection-vector (getf *standard-views* :trimetric)
         :page-width (* 5 72) :page-length (* 5 72)
         :objects (list self))))

#+nil
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
                             (make-point 0.5 0.5 0)))
   
    (view :type 'base-view
          :projection-vector (getf *standard-views* :trimetric)
          :page-width (* 8 72) :page-length (* 7 72)
          :objects (list self (the basis-surface)))))

#+nil
(define-object test-trimmed-from-dropped (trimmed-surface)
  :computed-slots
  ((uv-inputs t)
   (holes (list (the hole)))
   (reverse-island? t) (reverse-holes? t))
  
  :hidden-objects
  ((basis-surface :type 'planar-surface
                  :p00 (make-point 0 0 0)
                  :p01 (make-point 0 10 0)
                  :p10 (make-point 10 0 0)
                  :p11 (make-point 15 15 0))
   
   (island :type 'dropped-curve
           :curve-in (the raised-island)
           :surface (the basis-surface))
   
   (hole :type 'dropped-curve
         :curve-in (the raised-hole)
         :surface (the basis-surface))
   
   (raised-hole :type 'b-spline-curve
                  :control-points (list (make-point 3.5 4.5 10)
                                        (make-point 4.5 6 10) 
                                        (make-point 5.5 7 10) 
                                        (make-point 6 4.5 10) 
                                        (make-point 5.5 2 10) 
                                        (make-point 4.5 2 10) 
                                        (make-point 3.5 4.5 10)))
   
   (raised-island :type 'b-spline-curve
                  :control-points (list (make-point 3 5 1)
                                        (make-point 5 8 1) 
                                        (make-point 7 10 1) 
                                        (make-point 8 5 1) 
                                        (make-point 7 0 1) 
                                        (make-point 5 0 1) 
                                        (make-point 3 5 1)))))


#+nil
(define-object test-trimmed-from-dropped-2 (trimmed-surface)
   :computed-slots
   ((uv-inputs t)
    (holes (list (the hole)))
    (reverse-island? t) (reverse-holes? t)
    (display-controls (list :color :blue :line-thickness 2)))
  
   :hidden-objects
   ((basis-surface :type 'test-fitted-surface
                   :display-controls (list :color :grey-light-very)
                   :length 10 :width 10 :height 5)
   
    (island :type 'dropped-curve
            :curve-in (the raised-island)
            :surface (the basis-surface))
   
    (hole :type 'dropped-curve
          :curve-in (the raised-hole)
          :surface (the basis-surface))
   
    (raised-hole :type 'b-spline-curve
                 :display-controls (list :color :grey-light-very)
                 :control-points (list (make-point 3.5 4.5 7) (make-point 4.5 6 7) (make-point 5.5 7 7) 
                                       (make-point 6 4.5 7) (make-point 5.5 2 7) (make-point 4.5 2 7) 
                                       (make-point 3.5 4.5 7)))
   
    (raised-island :type 'b-spline-curve
                   :display-controls (list :color :grey-light-very)
                   :control-points (list (make-point 3 5 7) (make-point 5 8 7) (make-point 7 10 7) 
                                         (make-point 8 5 7) (make-point 7 0 7) (make-point 5 0 7) 
                                         (make-point 3 5 7)))

    (view :type 'base-view
          :projection-vector (getf *standard-views* :trimetric)
          :page-width (* 8 72) :page-length (* 7 72)
          :objects (list self (the raised-hole) (the raised-island) (the basis-surface)))))


#+nil
(define-object test-trimmed-from-projected (trimmed-surface)
  :computed-slots
  ((uv-inputs t)
   (island (the island-3d uv-curve))
   (holes (list (the hole uv-curve))))
  
  :hidden-objects
  ((basis-surface :type 'planar-surface
                  :p00 (make-point 0 0 0)
                  :p01 (make-point 0 10 0)
                  :p10 (make-point 10 0 0)
                  :p11 (make-point 15 15 0))
   
   (island-3d :type 'projected-curve
              :curve-in (the raised-island)
              :surface (the basis-surface)
              :projection-vector (make-vector 0 0 -1))

   
   (hole :type 'projected-curve
         :curve-in (the raised-hole)
         :surface (the basis-surface)
         :projection-vector (make-vector 0 0 -1))
   
   (raised-hole :type 'b-spline-curve
                :control-points (list (make-point 3.5 4.5 10)
                                      (make-point 4.5 6 10) 
                                      (make-point 5.5 7 10) 
                                      (make-point 6 4.5 10) 
                                      (make-point 5.5 2 10) 
                                      (make-point 4.5 2 10) 
                                      (make-point 3.5 4.5 10)))
   
   (raised-island :type 'b-spline-curve
                  :control-points (list (make-point 3 5 1)
                                        (make-point 5 8 1) 
                                        (make-point 7 10 1) 
                                        (make-point 8 5 1) 
                                        (make-point 7 0 1) 
                                        (make-point 5 0 1) 
                                        (make-point 3 5 1)))))


#+nil
(define-object test-trimmed-from-projected-2 (trimmed-surface)
  :computed-slots
  ((uv-inputs t)
   (island (the island-3d uv-curve))
   (holes (list (the hole uv-curve)))
   (display-controls (list :color :blue :line-thickness 2)))
  
  :objects
  ((basis-surface :type 'test-fitted-surface
                  :display-controls (list :color :pink)
                  :grid-length 10 :grid-width 10 :grid-height 5
                  )
   
   (raised-hole :type 'b-spline-curve
                :display-controls (list :color :grey-light-very)
                :control-points (list (make-point 3.5 4.5 7)
                                      (make-point 4.5 6 7) 
                                      (make-point 5.5 7 7) 
                                      (make-point 6 4.5 7) 
                                      (make-point 5.5 2 7) 
                                      (make-point 4.5 2 7) 
                                      (make-point 3.5 4.5 7)))

   (raised-island :type 'b-spline-curve
                  :display-controls (list :color :grey-light-very)
                  :control-points (list (make-point 3 5 7)
                                        (make-point 5 8 7) 
                                        (make-point 7 10 7) 
                                        (make-point 8 5 7) 
                                        (make-point 7 0 7) 
                                        (make-point 5 0 7) 
                                        (make-point 3 5 7))))
  :hidden-objects
  ((island-3d :type 'projected-curve
              :curve-in (the raised-island)
              :surface (the basis-surface)
              :projection-vector (make-vector 0 0 -1))
   

   (hole :type 'projected-curve
         :curve-in (the raised-hole)
         :surface (the basis-surface)
         :projection-vector (make-vector 0 0 -1))))

#+nil
(define-object test-trimmed-from-projected-3 (trimmed-surface)
  :computed-slots
  ((uv-inputs t)
   (island (reverse (list-elements (the island-curves)))))
  
  :hidden-objects
  ((basis-surface :type 'planar-surface
                  :p00 (make-point 0 0 0)
                  :p01 (make-point 0 10 0)
                  :p10 (make-point 10 0 0)
                  :p11 (make-point 10 10 0))
   
   (island-curves :type 'projected-curve
                  :sequence (:size (length (the-child ordered-curves)))
                  :ordered-curves (the raised-island ordered-curves)
                  :curve-in (nth (the-child index) (the-child ordered-curves))
                  :surface (the basis-surface)
                  :projection-vector (make-vector 0 0 1))
   
   (raised-island :type 'global-filleted-polyline-curves
                  :default-radius 0.5
                  :vertex-list (list (make-point 1 1 -1)
                                     (make-point 9 1 -1)
                                     (make-point 9 9 -1)
                                     (make-point 1 9 -1)
                                     (make-point 1 1 -1))
                  #+nil
                  (list (the basis-surface p00)
                        (the basis-surface p01)
                        (the basis-surface p11)
                        (the basis-surface p10)
                        (the basis-surface p00)))))

#+nil
(define-object basis-copy (b-spline-surface)
   :input-slots
   ("GDL Surface. Surface to provide the source basis data." built-from)

   :computed-slots
   ((b-spline-data (multiple-value-list (the built-from b-spline-data)))
    (control-points (first (the b-spline-data)))
    (weights (second (the b-spline-data)))
    (u-knot-vector (third (the b-spline-data)))
    (v-knot-vector (fourth (the b-spline-data)))
    (u-degree (fifth (the b-spline-data)))
    (v-degree (sixth (the b-spline-data)))))
