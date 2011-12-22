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


(in-package gdl-lift-tests)

(define-object poly-brep-test (surf::poly-brep)
  :computed-slots
  (;(chord-height-tolerance 0.0)
   ;(curve-tess-angle 10)
   ;(surface-tess-angle 10)
   ;(max-3d-edge 0.0)
   ;(max-aspect 0)
   (smooth-results? nil)
   (min-number-of-segments *tess-min-number-of-segments*)
   (max-3d-edge (* *tess-max-3d-edge-factor* (the brep max-extent)))
   (min-parametric-ratio *tess-min-parametric-ratio*)
   (max-chord-height *tess-max-chord-height*)
   (max-angle-degrees *tess-max-angle-degrees*)
   (min-3d-edge *tess-min-3d-edge*)
   (min-edge-ratio-uv *tess-min-edge-ratio-uv*)
   (max-aspect-ratio *tess-max-aspect-ratio*)
   
   (regression-test-data (the %lines-to-draw%)))
  
  :objects
  ((brep :type 'intersected-solid
	 :brep (the cylinder)
         :other-brep (the box))
   
   (cylinder :type 'cylinder-solid
             :radius 10 
             :length 20)
     
   (box :type 'box-solid
        :length 10
        :height 10
        :width 30)))

(warn "Not registering poly-brep-test, has major memory leak and we think it's not being used 
except maybe by whitebox...")

;;(register-test-definition 'poly-brep-test)
