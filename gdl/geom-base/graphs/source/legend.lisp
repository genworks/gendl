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

(define-object legend (base-object)
  
  :input-slots (upper-left labels&colors
                (background-color :white) (label-font "Helvetica")
                (label-font-size 10) (label-color :black)
                (leading 1.2))

  
  :computed-slots
  ((width (+ (max-of-elements (the labels) (the-element width))
             (the (color-keys 0) width)
             (* (the key-spacing) 3)))
   
   (length (- (* (the leading) (the color-key-size) 
                 (1+ (length (the labels&colors))))
              (the color-key-size)))
   
   (height 0)
   
   (center (translate (the upper-left) :front (half (the length))
                      :right (half (the width))))
   
   (color-key-size (* (the label-font-size) 1.5))
   
   (key-spacing (* (1- (the leading)) (the color-key-size))))

   
  :hidden-objects
  ((boundary :type 'box)
   
   (color-keys :type 'filled-rectangle
               :sequence (:size (length (the labels&colors)))
               :width (the color-key-size)
               :length (the color-key-size)
               :display-controls (list :color (second (nth (the-child index) (the labels&colors))))
               :center (translate (the upper-left) 
                                  :right (+ (half (the-child width)) (the key-spacing))
                                  :front (+ (half (the-child length)) (the key-spacing)
                                            (* (the-child index) (the-child length) (the leading)))))
   
   (labels :type 'general-note
           :sequence (:size (length (the labels&colors)))
           :font (the label-font)
           :character-size (the label-font-size)
           :strings (first (nth (the-child index) (the labels&colors)))
           :start (translate (the (color-keys (the-child index)) (edge-center :right :top))
                             :right (the key-spacing)))))



(define-object filled-rectangle (base-object))


;;
;; FLAG -- update for auto-scaling outside base-view
;;
(define-lens (pdf filled-rectangle)()
  :output-functions
  ((cad-output
    ()
    (with-format-slots (view)
      (let ((corners (list (the (vertex :rear :left :top))
                            (the (vertex :rear :right :top))
                            (the (vertex :front :right :top))
                            (the (vertex :front :left :top)))))
        (destructuring-bind (p0 p1 p2 p3)
            (if view (mapcar #'(lambda(point)(the-object view (view-point point))) corners) corners)
          (pdf:with-saved-state
              (write-the line-thickness-setting)
            (write-the rgb-stroke-setting)
            (apply #'pdf:set-rgb-fill  (coerce (lookup-color (the color-decimal) :ground :background) 'list))
            (pdf:move-to (get-x p1) (get-y p1))
            (pdf:line-to (get-x p2) (get-y p2))
            (pdf:line-to (get-x p3) (get-y p3))
            (pdf:line-to (get-x p0) (get-y p0))
            (pdf:fill-and-stroke))))))))
        

(define-lens (pdf legend)()
  :output-functions
  ((cad-output
    ()
    (write-the boundary (cad-output))
    (mapc #'(lambda(label) (write-the-object label (cad-output))) (list-elements (the labels)))
    (mapc #'(lambda(color-key) (write-the-object color-key (cad-output))) (list-elements (the color-keys))))))

             
