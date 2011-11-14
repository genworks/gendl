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

(define-object validated-solid (brep)

  :input-slots ((brep nil) (%old-native-brep% nil))

  :computed-slots ((%validation-results% (multiple-value-bind 
                                               (new-native-brep
                                                tolerances-updated?
                                                max-edge-uv-trim-curve-gap
                                                max-vertex-edge-gap
                                                max-vertex-face-gap)
                                             (brep-validate-and-update-tolerances *geometry-kernel*
                                                                                  (if (the brep)
                                                                                      (the brep %native-brep%)
                                                                                      (the %old-native-brep%)))
                                           (list :new-native-brep new-native-brep
                                                 :tolerances-updated? tolerances-updated?
                                                 :max-edge-uv-trim-curve-gap max-edge-uv-trim-curve-gap
                                                 :max-vertex-edge-gap max-vertex-edge-gap
                                                 :max-vertex-face-gap max-vertex-face-gap)))


                   (%native-brep% (getf (the %validation-results%) :new-native-brep))

                   (tolerances-updated? (ecase (getf (the %validation-results%) :tolerances-updated?)
                                          (0   nil) (1 t)))

                   (max-edge-uv-trim-curve-gap (getf (the %validation-results%) :max-edge-uv-trim-curve-gap))
                   (max-vertex-edge-gap (getf (the %validation-results%) :max-vertex-edge-gap))
                   (max-vertex-face-gap (getf (the %validation-results%) :max-vertex-face-gap))))

                   
