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

(define-object points-display (outline-specialization-mixin)
  :input-slots (points)
  
  :computed-slots ((points-array (make-array (length (the points))
                                             :initial-contents (the points))))
  
  :hidden-objects
  ((display-points :type 'point
                   :sequence (:size (length (the points)))
                   :center (aref (the points-array) (the-child index)))))


