;;
;; Copyright 2015 Genworks International 
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


(define-object circular-surface (trimmed-surface)

  :documentation 
  (:description 
   "Represents a planar, full-circular surface controlled
    by a radius, and the normal center and orientation
    for base-object."

   :examples "<pre>
 (in-package :gdl-user)

 (define-object circular-surface-test (surf::circular-surface)
  :computed-slots ((radius 108)))

 (generate-sample-drawing :objects (make-object 'circular-surface-test))

</pre>")


  :input-slots
  ("Number. The radius of the resulting trimmed-surface."  
   radius)

  :hidden-objects
  ((basis-surface :type 'rectangular-surface
		  :length (twice (the radius)) 
		  :width (twice (the radius)))

   (island :type 'arc-curve :pass-down (radius))))


;;
;; FLAG move to main package.lisp. 
;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'circular-surface :surf))

;;
;; FLAG -- add Lift regression test in gendl/regression/source/
;;





		  

   

							   
