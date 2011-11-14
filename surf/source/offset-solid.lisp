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

(define-object offset-solid (brep)

  :input-slots
  ("GDL Brep object. The brep to be offset." brep
   "Number. The distance to offset. Can be negative." distance
   
   ("Number. The tolerance to use for the shelling operation. 
Defaults to (the adaptive-tolerance) of the input brep." 
    tolerance (the %actual-brep% adaptive-tolerance)))

  :computed-slots
  ((%actual-brep% 
    (if (typep (the brep) 'brep) (the brep) 
                  (the brep brep)))

   (%native-brep% 
    (make-offset-brep *geometry-kernel* 
                      (the %actual-brep% %native-brep%) 
                      :distance (the distance)
                      :tolerance (the tolerance)))))



(define-object shelled-solid (brep)

  :input-slots
  ("GDL Brep object. Should be an open shell. 
The brep to be shelled into a solid." brep
   
   "Number. The distance to offset. Can be negative." 
   distance

   ("Number. The tolerance to use for the shelling operation. 
Defaults to (the adaptive-tolerance) of the input brep." 
    tolerance (the %actual-brep% adaptive-tolerance))
   
   )

  
  :computed-slots
  ((%actual-brep% 
    (if (typep (the brep) 'brep) (the brep) (the brep brep)))

   (%native-brep% 
    (make-shelled-brep *geometry-kernel* 
                       (the %actual-brep% %native-brep%)
                       :distance (the distance)
                       :tolerance (the tolerance)))))
