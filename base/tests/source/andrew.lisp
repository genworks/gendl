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


(in-package :gdl-base-tests)


(define-object a ()

  :input-slots ((inp-a "hey") inp-b)

  :trickle-down-slots (desc-1 desc-2)

  :computed-slots
  ((desc-1 (format nil "descendant ~a" (the inp-a)))
   (desc-2 (format nil "descendant-2 ~a" (the inp-b))))

  
  :objects
  ((part-1 :type 'b
           :input-1 1
           :input-2 2)

   (part-2 :type 'b
           :input-1 (the part-1 input-2)
           :input-2 (the part-1 input-1))

   (part-3 :type 'b
           :input-1 (the part-2 input-1)
           :input-2 (the part-2 input-2))))

(define-object b ()

  :input-slots
  ((desc-2 "local value of desc-2" :defaulting) 
   (desc-1 "local value desc-1")
   input-1 input-2 )
  
  :computed-slots
  ((strings-for-display "B")))


(define-object c (a)
  :objects
  ((part-1 :type 'b
           :input-1 10)))
  




