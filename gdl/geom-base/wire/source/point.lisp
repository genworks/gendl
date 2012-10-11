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

(in-package :geom-base)


;;
;; FLAG -- change the rendering of this to scale-independent crosshairs.
;;
(define-object point (sphere)
  
  :documentation (:description "Visual representation of a point as a small view-independent crosshair. This means
the crosshair will always appear in a ``top'' view regardless of the current view transform. The crosshair will
not scale along with any zoom state unless the <tt>scale?</tt> optional input-slot is non-NIL. The default
color for the crosshairs is a light grey (:grey-light-very in the *color-table*)."
                     :examples "<pre>

 (in-package :gdl-user)

 (define-object point-sample (base-object)
   
   :objects
   ((bezier :type 'bezier-curve
            :control-points (list (make-point 0 0 0)
                                  (make-point 1 1 0)
                                  (make-point 2 1 0)
                                  (make-point 3 0 0)))
    (points-to-show :type 'point
                    :sequence (:size (length (the bezier control-points)))
                    :center (nth (the-child :index) 
                                 (the bezier control-points))
                    :radius 0.08
                    :display-controls (list :color :blue))))

 (generate-sample-drawing :object-roots (make-object 'point-sample))

</pre>")
                
  :input-slots
  (("Boolean. Indicates whether the crosshairs drawn to represent the point are scaled along with
any zoom factor applied to the display, or are fixed with respect to drawing space. The default is NIL,
meaning the crosshairs will remain the same size regardless of zoom state."
    scaled? nil)

   (radius 0)
   
   ("Number. Distance from center to end of crosshairs used to show the point. Default value is 3."
    crosshair-length 3)
   
   (color-key (getf (the display-controls) :color :grey-light-very)))
  
  
  :computed-slots
  ((%curves-to-draw% nil)
   
   (%renderer-info% (list :vrml? t :view-default :top))
   
   (%corners% (if (the scaled?) 
                  (list (the (vertex :top :right :rear))
                        (the (vertex :bottom :left :front)))
                (list (translate (the center) :right 1 :rear 1 :top 1)
                      (translate (the center) :left 1 :front 1 :bottom 1))))
   
   (bounding-box (let ((x (get-x (the center)))
                       (y (get-y (the center)))
                       (z (get-z (the center))))
                   (bounding-box-from-points (list (make-point (- x 1) (- y 1) (- z 1))
                                                   (make-point (+ x 1) (+ y 1) (+ z 1))))))))


(define-lens (pdf point)()
  :output-functions
  ((cad-output
    ()
    (with-format-slots (view)
      (let ((center (if view (the-object view (view-point (the center))) (the center)))
            (view-scale (if (and view (the scaled?)) (the-object view view-scale) 1)))
        (let ((start-x (- (get-x center) (* (the crosshair-length) view-scale)))
              (start-y (- (get-y center) (* (the crosshair-length) view-scale))))

          
          (pdf:with-saved-state
            (write-the line-thickness-setting)
            (write-the rgb-stroke-setting)
            (pdf:move-to (to-single-float start-x) (to-single-float (get-y center)))
            (pdf:line-to (to-single-float (+ start-x  (* (twice (the crosshair-length)) view-scale))) 
                         (to-single-float (get-y center)))
            (pdf:move-to (to-single-float (get-x center)) 
                         (to-single-float start-y))
            (pdf:line-to (to-single-float (get-x center)) 
                         (to-single-float (+ start-y (* (twice (the crosshair-length)) view-scale))))
            (pdf:stroke))))))))
          
          

