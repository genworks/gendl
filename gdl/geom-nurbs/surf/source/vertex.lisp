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

(define-object vertex (base-object)
  :input-slots (%native-vertex%
                %native-brep% ;; FLAG -- should default to get-brep-from-face of the %native-face%.
                brep ;; FLAG -- should default to a new brep constructed from the %native-brep%
                
                )
  
  :computed-slots ((center (get-3d-point-from-vertex *geometry-kernel* (the %native-vertex%)))))
  
  

