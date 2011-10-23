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


(define-object circle (arc)
  :documentation (:description "The set of points equidistant from a given point. 
The distance from the center is called the radius, and the point is called 
the center. The start point of the circle is at the 3 o'clock position, and positive
angles are measured anti-clockwise."
                  :examples "<pre>

  (in-package :gdl-user)
                  
  (define-object circle-sample (circle)
    :computed-slots
    ((radius 10)))

  (generate-sample-drawing :objects (make-object 'circle-sample))

                  
</pre>")                  
                  
  :computed-slots
  ((%renderer-info% (list :vrml? t :view-default :top))
   
   ("Number. The perimeter of the circle."
    circumference (* 2pi (the radius)))
   
   ("Number. The area enclosed by the circle."
    area (* pi (^2 (the radius))))
   
   (start-angle 0) (end-angle 2pi)))



;;
;; FLAG -- update for auto-scaling outside base-view
;;
;; FLAG -- remove this and handle in base-view output of generic
;; curves (if possible). Or, do only the fill part here.
;;
;;
;;(define-view (pdf circle)()
;;  :output-functions
;;  ((cad-output
;;    ()
;;    (let ((curves (the curves)))
;;      (pdf:with-saved-state
;;        (write-the line-thickness-setting)
;;      (write-the rgb-stroke-setting)
;;      (when (the fill-color-decimal) (apply #'pdf:set-rgb-fill (the fill-color-decimal)))
;;      (let ((start (world-to-drawing (first (first curves)))))
;;        (pdf:move-to (get-x start) (get-y start))
;;        (mapc #'(lambda(curve)
;;                  (destructuring-bind (p1 p2 p3 p4) (world-to-drawing curve)
;;                    (declare (ignore p1))
;;                    (pdf:bezier-to (get-x p2) (get-y p2)
;;                                   (get-x p3) (get-y p3)
;;                                   (get-x p4) (get-y p4)))) curves))
;;      (if (the fill-color-decimal) (pdf:fill-and-stroke) (pdf:stroke)))))))


