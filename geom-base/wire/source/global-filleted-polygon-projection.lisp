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

(define-object global-filleted-polygon-projection (global-polygon-projection)

  :documentation (:description "Similar to a global-polygon-projection, but the polygon is filleted
as with global-filleted-polygon."
                  :examples "<pre>
 (in-package :gdl-user)

 (define-object global-filleted-polygon-projection-sample 
     (global-filleted-polygon-projection)
     :computed-slots
     ((display-controls (list :color :blue-steel 
                              :transparency 0.3 
                              :shininess 0.7 
                              :spectral-color :white))
      (default-radius 5)
      (projection-depth 5)
      (vertex-list (list (make-point 0 0 0)
                         (make-point 10 10 0)
                         (make-point 30 10 0)
                         (make-point 40 0 0)
                         (make-point 30 -10 0)
                         (make-point 10 -10 0)
                         (make-point 0 0 0)))))

 (generate-sample-drawing :objects 
                          (make-object 'global-filleted-polygon-projection-sample)
                          :projection-direction :trimetric)





</pre>")
  
  :input-slots
  (("List of Numbers. Specifies the radius for each vertex (``corner'') of the filleted-polyline."
    radius-list nil)
   
   ("Number. Specifies a radius to use for all vertices. Radius-list will take precedence over this."
    default-radius nil))
  
  :computed-slots
  ((polygon-type 'global-filleted-polyline)
   
   (%curves-to-draw% (append (the :polygon-original :%curves-to-draw%)
                             (the :polygon-1 :%curves-to-draw%)
                             (when (typep (the :polygon-2) (the :polygon-type))
                               (the :polygon-2 :%curves-to-draw%))))
   
   (%arcs% (append (the :polygon-original :%arcs%)
                   (the :polygon-1 :%arcs%)
                   (when (typep (the :polygon-2) (the :polygon-type))
                     (the :polygon-2 :%arcs%))))
   
   (projection-lines (mapcar #'(lambda(line)
                                 (list (the-object line :start)
                                       (the-object line :end))) (list-elements (the :projection-line-objects)))))
  
  :hidden-objects
  (;; FLAG -- get rid of this quantified set.
   ;;
   (projection-line-objects
    :type 'line
    :sequence (:size (twice (length (the :polygon-original :straights))))
    :start (let ((straight (nth (floor (half (the-child :index)))
                                (the (evaluate (if (typep (the :polygon-2)
                                                          (the :polygon-type))
                                                   :polygon-2 :polygon-original)) :straights))))
             (funcall (if (oddp (the-child :index)) #'first #'second) straight))
    :end (let ((straight (nth (floor (half (the-child :index))) (the :polygon-1 :straights))))
           (funcall (if (oddp (the-child :index)) #'first #'second) straight)))))

