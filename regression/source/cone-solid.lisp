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


(in-package :gdl-lift-tests)


(define-object cone-solid-test (cone-solid)
  
   :computed-slots
   ((length 100)
    (radius-1 10)
    (radius-2 20)
    (inner-radius-1 8)
    (inner-radius-2 16)
    
    (regression-test-data (append (multiple-value-list (the precise-properties))
				  (the %curves-to-draw%)
				  (the %lines-to-draw%)))))


(register-test-definition 'cone-solid-test)
