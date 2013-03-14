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

(define-object arrowhead (base-object)
  
  :input-slots
  ((style :wedge)
   (length (* .2 72))
   (width (* .1 72))
   (wedge-ratio 1/3))
   
  
  :computed-slots
  ((height 0)
   (arrow-center (translate (the center)
                            :front (* (the length) (- 1 (the wedge-ratio)))))
   ;;
   ;; FLAG -- remove this when we have more general 2d bounding box machinery
   ;;
   (%lines-to-draw% (append
                     (destructuring-bind (p0 p1 p2 p3)
                         (the outline-points)
                       (list (list p0 p1)
                             (list p1 p2)
                             (list p2 p3)
                             (list p3 p0)))
                     (when (the second-outline-points)
                       (destructuring-bind (p0 p1 p2 p3)
                         (the second-outline-points)
                       (list (list p0 p1)
                             (list p1 p2)
                             (list p2 p3)
                             (list p3 p0))))))
    
   (outline-points (ecase (the style)
                     ((:wedge :double-wedge :triangle)
                      (when (eql (the style) :triangle)
                        (warn "*** ~%~%Triangle not yet supported~%~%"))
                      (list (the center)
                            (translate (the (vertex :front :right :top))
                                       :front (half (the length)))
                            (translate (the center) :front
                                       (* (the length) (- 1 (the wedge-ratio))))
                            (translate (the (vertex :front :left :top))
                                       :front (half (the length)))))))
   
   
   (second-outline-points (when (member (the style) (list :double-wedge))
                            (mapcar #'(lambda(point)
                                        (translate point :front (* (the length) (- 1 (the wedge-ratio)))))
                                    (the outline-points))))))

;;
;; FLAG -- update for auto-scaling outside base-view
;;
(define-lens (pdf arrowhead)()
  :output-functions
  ((cad-output
    ()
    (with-format-slots (view)
      (ecase (the style)
        ((:wedge :double-wedge :triangle)
         (pdf:with-saved-state
             (write-the line-thickness-setting)
           (destructuring-bind (p0 p1 p2 p3)
               (if view 
                   (mapcar #'(lambda(point) (the-object view (view-point point))) (the outline-points))
                 (the outline-points))
             (write-the rgb-stroke-setting)
             (pdf:move-to (get-x p0) (get-y p0))
             (pdf:line-to (get-x p1) (get-y p1))
             (pdf:line-to (get-x p2) (get-y p2))
             (pdf:line-to (get-x p3) (get-y p3))
             (pdf:line-to (get-x p0) (get-y p0))
             (pdf:fill-and-stroke))
           (when (the second-outline-points)
             (destructuring-bind (p0 p1 p2 p3)
                 (if view 
                     (mapcar #'(lambda(point) (the-object view (view-point point))) (the second-outline-points))
                   (the second-outline-points))
               (write-the rgb-stroke-setting)
               (pdf:move-to (get-x p0) (get-y p0))
               (pdf:line-to (get-x p1) (get-y p1))
               (pdf:line-to (get-x p2) (get-y p2))
               (pdf:line-to (get-x p3) (get-y p3))
               (pdf:line-to (get-x p0) (get-y p0))
               (pdf:fill-and-stroke))))))))))
                                   
  

(define-lens (dxf arrowhead)()
  :output-functions
  ((cad-output
    ()
    (with-format-slots (view)
      (ecase (the style)
        ((:wedge :double-wedge :triangle)
         ;;(write-the line-thickness-setting)
         ;;(write-the rgb-stroke-setting)
         (destructuring-bind (p0 p1 p2 p3)
             (mapcar #'(lambda(point)
                         (add-vectors *dxf-translation* (subseq point 0 2)))
                     (if view (mapcar #'(lambda(point) (the-object view (view-point point))) (the outline-points))
                       (the outline-points)))
           
           (format *stream* "  0~%LINE~% 10~%~3,16f~% 20~%~3,16f~% 11~%~3,16f~% 21~%~3,16f~%"
                   (get-x p0) (get-y p0) (get-x p1) (get-y p1))
           (write-the line-thickness-setting)
           (write-the rgb-stroke-setting)
           (format *stream* "  0~%LINE~% 10~%~3,16f~% 20~%~3,16f~% 11~%~3,16f~% 21~%~3,16f~%"
                   (get-x p1) (get-y p1) (get-x p2) (get-y p2))
           (write-the line-thickness-setting)
           (write-the rgb-stroke-setting)
           (format *stream* "  0~%LINE~% 10~%~3,16f~% 20~%~3,16f~% 11~%~3,16f~% 21~%~3,16f~%"
                   (get-x p2) (get-y p2) (get-x p3) (get-y p3))
           (write-the line-thickness-setting)
           (write-the rgb-stroke-setting)
           (format *stream* "  0~%LINE~% 10~%~3,16f~% 20~%~3,16f~% 11~%~3,16f~% 21~%~3,16f~%"
                   (get-x p3) (get-y p3) (get-x p0) (get-y p0))
           (write-the line-thickness-setting)
           (write-the rgb-stroke-setting))

         
         (when (the second-outline-points)
           (destructuring-bind (p0 p1 p2 p3)
               (mapcar #'(lambda(point)
                           (add-vectors *dxf-translation* (subseq point 0 2)))
                       (if view 
                           (mapcar #'(lambda(point) (the-object view (view-point point))) (the second-outline-points))
                         (the second-outline-points)))
             (write-the line-thickness-setting)
             (write-the rgb-stroke-setting)
             (format *stream* "  0~%LINE~% 10~%~3,16f~% 20~%~3,16f~% 11~%~3,16f~% 21~%~3,16f~%"
                     (get-x p0) (get-y p0) (get-x p1) (get-y p1))
             (write-the line-thickness-setting)
             (write-the rgb-stroke-setting)
             (format *stream* "  0~%LINE~% 10~%~3,16f~% 20~%~3,16f~% 11~%~3,16f~% 21~%~3,16f~%"
                     (get-x p1) (get-y p1) (get-x p2) (get-y p2))
             (write-the line-thickness-setting)
             (write-the rgb-stroke-setting)
             (format *stream* "  0~%LINE~% 10~%~3,16f~% 20~%~3,16f~% 11~%~3,16f~% 21~%~3,16f~%"
                     (get-x p2) (get-y p2) (get-x p3) (get-y p3))
             (write-the line-thickness-setting)
             (write-the rgb-stroke-setting)
             (format *stream* "  0~%LINE~% 10~%~3,16f~% 20~%~3,16f~% 11~%~3,16f~% 21~%~3,16f~%"
                     (get-x p3) (get-y p3) (get-x p0) (get-y p0))
             (write-the line-thickness-setting)
             (write-the rgb-stroke-setting)))))))))
