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

(in-package :gdl)

(define-object null-object ()
  :documentation (:description "A part with no geometric representation and no children. Use this in a 
conditional <tt>:type</tt> expression if you want to turn off a branch of the tree conditionally.")
  
  
  :computed-slots
  ((%vertex-array% (make-array (list 0)))
   (%line-vertex-indices% nil)
   (%curve-vertex-indices% nil)
   (%lines-to-draw% nil)
   (%curves-to-draw% nil)
   (%2d-bounding-box% nil)
   
   ))


(define-object null-part (null-object))
