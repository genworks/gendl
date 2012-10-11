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

(in-package :surf)


(define-object subdivided-curve (curve)
  
  :input-slots
  (curve-in
   
   (continuity-type :discontinuous)
   
   (continuity-angle 0))
  
  :computed-slots
  ((available-continuity-types (list :discontinuous
                                     :c0 :g1 :g1r 
                                     :g1_g2 :c1 :c1_g2 
                                     :c1_c2 :cinfinity))
   
   (native-curves (curve-subdivide-at-discontinuities *geometry-kernel*
                                                      :curve (the curve-in)
                                                      :continuity-type 
                                                      (1+ (position (the continuity-type)
                                                                    (the available-continuity-types)))
                                                      :continuity-angle (the continuity-angle))))
  
  :objects
  ((curves :type 'curve
           :sequence (:size (length (the native-curves)))
           :native-curve-iw (nth (the-child index) (the native-curves)))))
