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

(in-package :gwl-user)

(define-object display (application-mixin)

  :computed-slots
  ((ui-display-list-leaves (list (the ex-drawing))))

  :objects
  ((ex-drawing :type 'ex-drawing-1)))


(define-object ex-drawing-1 (base-drawing)

  :input-slots
  ((page-length 700) (page-width 800))

  :objects
  ((top-view :type 'base-view
             :projection-vector (getf *standard-views* :top)
             :width (half (the page-width))
             :objects (list (the box-1) (the box-2) (the box-3))
             :annotation-objects (list (the dim-1) (the dim-2) (the dim-3))
             :left-margin  30
             :center (translate (the center) :left (half (the-child width))))

   (detail-view :type 'base-view
                :projection-vector (getf *standard-views* :top)
                :width (/ (the width) 4)
                :length (/ (the length) 4)
                :objects (list (the box-3))
                :annotation-objects (list (the dim-3a))
                :front-margin 40
                :center (translate (the center) 
                                   :right (the-child width) 
                                   :rear (the-child length)))

   (box-1 :type 'box
          :length 10
          :width 10
          :height 4
          :center (make-point (half (the-child width)) (half (the-child length)) (half (the-child height))))

   (box-2 :type 'box
          :length 5
          :width 20
          :height 20
          :center (make-point (half (the-child width)) (+ (the box-1 length) (half (the-child length))) 0))

   (box-3 :type 'box
          :length 2
          :width 1
          :height 1
          :center (translate (the box-1 (face-center :right)) :left (half (the-child width))))

   (dim-1 :type 'vertical-dimension
          :character-size 10
          :dim-scale (/ (the top-view view-scale))
          :dim-text (list "This is" "a multiline" "dim-text")
          :witness-line-length 1
          :witness-line-gap 1
          :start-point (the top-view (view-point (the box-1 (vertex :bottom :right :front))))
          :end-point (the top-view (view-point (the box-2 (:vertex :bottom :right :front)))))

   (dim-2 :type 'vertical-dimension
          :character-size 10
          :dim-scale (/ (the top-view view-scale))
          :witness-line-length 10
          :witness-line-gap 1
          :text-along-axis? t
          :start-point (the top-view (view-point (the box-2 (vertex :bottom :right :rear))))
          :end-point (the top-view (view-point (the box-2 (:vertex :bottom :right :front)))))

   (dim-3 :type 'vertical-dimension
          :character-size 10
          :dim-scale (/ (the top-view view-scale))
          :witness-line-length 15
          :witness-line-gap 1
          :outside-leaders? t
          :leader-line-length (* (the-child arrowhead-length) 3)
          :start-point (the top-view (view-point (the box-3 (vertex :bottom :right :rear))))
          :end-point (the top-view (view-point (the box-3 (:vertex :bottom :right :front)))))
  
  
   (dim-3a :type 'vertical-dimension
           :character-size 10
           :dim-scale (/ (the detail-view view-scale))
           :witness-line-length 15
           :witness-line-gap 1
           :outside-leaders? t
           :leader-line-length (* (the-child arrowhead-length) 3)
           :start-point (the detail-view (view-point (the box-3 (vertex :bottom :right :rear))))
           :end-point (the detail-view (view-point (the box-3 (:vertex :bottom :right :front)))))))


