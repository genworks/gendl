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


(define-object planar-surface-test (planar-surface)
  :computed-slots
  ((p00 (make-point 0 0 0))
   (p01 (make-point 0 1 0))
   (p10 (make-point 1 0 0))
   (p11 (make-point 1.5 1.5 0))
   
   (regression-test-data (multiple-value-list (the b-spline-data)))))



(register-test-definition 'planar-surface-test)

;;
;; Memory leak results: 1MB for 10000 iterations.
;;
