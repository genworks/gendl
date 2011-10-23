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

(define-object stitched-solid (brep)

  :input-slots
  ("List of GDL Surface or Face objects. These will be stitched together into an open shell or possibly a Solid" faces-in)
  
  :computed-slots
  ((plain-surfaces (remove-if #'(lambda (object)(typep object 'trimmed-surface))
                              (remove-if-not #'(lambda(object) (typep object 'surface)) (the faces-in))))
   
   (trimmed-surfaces (remove-if-not #'(lambda (object)(typep object 'trimmed-surface)) (the faces-in)))

   (proper-faces (remove-if-not #'(lambda(object) (typep object 'face)) (the faces-in)))
   
   (%native-brep% (progn (when (or (the trimmed-surfaces) (the proper-faces))
                           (error "stitched-solid is currently only implemented for plain untrimmed surfaces"))
                         (make-stitched-solid-brep *geometry-kernel* 
                                                   :surfaces (the plain-surfaces)
                                                   :proper-faces nil)))))


(define-object test-stitched-solid (stitched-solid)
  :computed-slots ((faces-in (list (the surface))))
  
  :hidden-objects
  ((surface :type 'test-b-spline-surface)))


(define-object test-stitched-solid-box (stitched-solid)
  :computed-slots ((length 20) (width 10) (height 10)
                   
                   (faces-in (list (the top-face) (the bottom-face)
                                   (the right-face) (the left-face)
                                   (the rear-face) (the front-face))))
  :hidden-objects
  ((box :type 'box)
   
   (top-face :type 'planar-surface
             :p00 (the box (vertex :top :right :rear))
             :p01 (the box (vertex :top :right :front))
             :p10 (the box (vertex :top :left :rear))
             :p11 (the box (vertex :top :left :front)))
   
   
   (bottom-face :type 'planar-surface
                :p00 (the box (vertex :bottom :right :rear))
                :p01 (the box (vertex :bottom :right :front))
                :p10 (the box (vertex :bottom :left :rear))
                :p11 (the box (vertex :bottom :left :front)))
   
   
   (right-face :type 'planar-surface
                :p00 (the box (vertex :top :right :rear))
                :p01 (the box (vertex :top :right :front))
                :p10 (the box (vertex :bottom :right :rear))
                :p11 (the box (vertex :bottom :right :front)))
   
   
   (left-face :type 'planar-surface
                :p00 (the box (vertex :top :left :rear))
                :p01 (the box (vertex :top :left :front))
                :p10 (the box (vertex :bottom :left :rear))
                :p11 (the box (vertex :bottom :left :front)))
   
   
   (rear-face :type 'planar-surface
                :p00 (the box (vertex :top :right :rear))
                :p01 (the box (vertex :top :left :rear))
                :p10 (the box (vertex :bottom :right :rear))
                :p11 (the box (vertex :bottom :left :rear)))
   
   (front-face :type 'planar-surface
               :p00 (the box (vertex :top :right :front))
               :p01 (the box (vertex :top :left :front))
               :p10 (the box (vertex :bottom :right :front))
               :p11 (the box (vertex :bottom :left :front)))))
   
