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


(in-package :gdl-user)

(define-object t-support-1 (box)
  
  :input-slots
  (cost-per-inch
   (length 120)
   
   )
  
  :computed-slots
  ((height (/ (the width) 2))
   (width 12)
   (support-thickness 2)
     
   (cost (* (the length) (the cost-per-inch))))

  
  :objects
  ((vertical-support :type 'box
		     :center (translate (the center)
					:down
					(half (the-child height)))
		     :width (the support-thickness)
		     :height (the height)
		     :length (the length))
   
   (horizontal-support :type 'box
		       :center (translate (the center)
					    :up
					    (half (the-child height)))
		       :width (the width)
		       :length (the length)
		       :height (the support-thickness))))
    
