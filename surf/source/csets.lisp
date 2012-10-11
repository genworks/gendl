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

(define-object face-set (base-object)
  
  :input-slots (brep spec)
  
  :computed-slots
  ((found-faces (ecase (first (the spec))
                  (cset-nearest-to-point 
                   
                   ))))
  
  :objects
  ((faces :type 'face
          :sequence (:size (length (the found-faces)))
          :%native-face% (the-object (nth (the-child index) (the found-faces)) %native-face%)
          :%native-brep% (the brep %native-brep%)
          :brep (the brep))))
