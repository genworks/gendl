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

(define-format 2d-output (base-format) 
  :slots ((view nil) (page-width 612) (page-length 792) 
          (view-scale nil) (view-transform nil) (view-center nil)))




(defmethod initialize-instance :after ((object 2d-output) &rest init-options)
  (when (slot-value object 'gdl-acc::view-scale) (warn "view-scale initarg for 2d-output is deprecated."))
  (when (slot-value object 'gdl-acc::view-transform) (warn "view-transform initarg for 2d-output is deprecated."))
  (when (slot-value object 'gdl-acc::view-center) (warn "view-center initarg for 2d-output is deprecated."))
  
  )


  




