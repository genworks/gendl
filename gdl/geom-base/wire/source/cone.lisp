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

(define-object cone (cylinder)
  :documentation (:description "A pyramid with a circular cross section, with its vertex above 
the center of its base. Partial cones and hollow cones are supported."
                  
                  :examples "<pre>
  (in-package :gdl-user)

   (define-object cone-sample (cone)
     :computed-slots
     ((display-controls (list :color :blue-neon 
                              :transparency 0.5 
                              :shininess 0.8 
                              :specular-color :white))
      (length 10) (radius-1 2)(inner-radius-1 1)
      (radius-2 5) (number-of-sections 5)
      (inner-radius-2 3)))
  
 (generate-sample-drawing :objects (make-object 'cone-sample) 
                          :projection-direction :trimetric) 
  </pre>")
  
  :input-slots
  (
   ("Number. The radius of the top end of the cone."
    radius-1 (the :radius)) 
   ("Number. The radius of the bottom end of the cone."
    radius-2 (the :radius))
   ("Number. The radius of the inner hollow part at the top end for a hollow cone."
    inner-radius-1 (the :inner-radius))
   ("Number. The radius of the inner hollow part at the bottom end for a hollow cone."
    inner-radius-2 (the :inner-radius)))
  
  
  :computed-slots
  ((width (twice (max (the radius-1) (the radius-2))))
   (height (the width))))



