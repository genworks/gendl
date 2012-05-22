;;
;; Copyright 2012 Genworks International
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

(define-object lift-calculator ()
  :input-slots
  (rho v A C-of-L)

  :computed-slots
  (;;
   ;; FLAG -- wrong 
   ;; replace with 
   ;; correct expression
   ;; 
   (lift (* (the rho) (the v) (the A) (the C-of-L)))                                         
   ;;
   ))


(define-object lift-forces ()
  :input-slots
  ((data-plists (list (list :rho 1.22 :v 108 :A 25 :C-of-L 0.2)
		      (list :rho 1.22 :v 123 :A 38 :C-of-L 0.2)
		      (list :rho 1.22 :v 142 :A 42 :C-of-L 0.2))))

  :objects
  ((lift-calculators :type 'lift-calculator
		     :sequence (:size (length (the data-plists)))
		     :rho (getf (nth (the-child index) (the data-plists)) :rho)
		     :v (getf (nth (the-child index) (the data-plists)) :v)
		     :A (getf (nth (the-child index) (the data-plists)) :A)
		     :C-of-L (getf (nth (the-child index) (the data-plists)) :C-of-G))))


(define-object lift-forces-alternative ()
  :input-slots
  ((data-plists (list (list :rho 1.22 :v 108 :A 25 :C-of-L 0.2)
		      (list :rho 1.22 :v 123 :A 38 :C-of-L 0.2)
		      (list :rho 1.22 :v 142 :A 42 :C-of-L 0.2))))

  :objects
  ((lift-calculators :type 'lift-calculator
		     :sequence (:size (length (the data-plists)))
		     :parameters (the data-plists))))


(define-object lift-calculator-with-func ()
  :input-slots
  (rho v A)

  :functions
  ((lift 
    (C-of-L)
    ;; FLAG -- wrong 
    ;; replace with 
    ;; correct expression
    ;; 
    (* (the rho) (the v) (the A) C-of-L))))
