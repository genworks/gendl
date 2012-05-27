;;
;; Copyright 2012 Genworks International and the Delft University of
;; Technology
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



(define-object empty-surface (base-object)
  ;;
  ;; Empty specs -- will replace with "real" built-in surface 
  ;; later. 
  ;;
  )


(define-object wing (empty-surface)
  :computed-slots 
  ((b 30)
   (c-root 6)
   (c-tip 3)
   (c-avg (/ (+ (the c-root) (the c-tip)) 2))
   (taper (/ (the c-tip) (the c-root)))))

(define-object wing-with-input (empty-surface)

  :input-slots (b)

  :computed-slots 
  ((c-root 6)
   (c-tip 3)
   (c-avg (/ (+ (the c-root) (the c-tip)) 2))
   (taper (/ (the c-tip) (the c-root)))

   (S (* (the b) (the c-avg)))
   (A (/ (expt (the b) 2) (the S)))))

 
(define-object wing-more-inputs (empty-surface)

  :input-slots
  ((b 20)
   (c-root 6 :settable)
   (c-tip 3 :settable))
  :computed-slots
  ((c-avg (/ (+ (the c-root) (the c-tip)) 2))
   (taper (/ (the c-tip) (the c-root)))
   (S (* (the b) (the c-avg)))
   (A (/ (expt (the b) 2) (the S)))))



(define-object engine (cylinder)

  :input-slots
  (Tmax))


(define-object wing-with-engine (empty-surface)

  :input-slots
  ((Tmax 800)
   (b 20)
   (c-root 6 :settable)
   (c-tip 3 :settable))

  :computed-slots
  ((c-avg (/ (+ (the c-root) (the c-tip)) 2))
   (taper (/ (the c-tip) (the c-root)))
   (S (* (the b) (the c-avg)))
   (A (/ (expt (the b) 2) (the S))))

  :objects
  ((engine :type 'engine
	   :Tmax (the Tmax))))



(define-object wing-with-engines (empty-surface)
  :input-slots
  ((Tmax-list (list 1000 800 900 1200) :settable)
   (c-root 6 :settable)
   (c-tip 3 :settable)
   (b 20))

  :computed-slots 
  ((Tmax-total (sum-elements (the engines) (the-element Tmax)))
   (c-avg (/ (+ (the c-root) (the c-tip)) 2))
   (taper (/ (the c-tip) (the c-root)))
   (S (* (the b) (the c-avg)))
   (A (/ (expt (the b) 2) (the S))))

  :objects
  ((engines :type 'engine
            :sequence (:size (length (the Tmax-list)))
            :Tmax (nth (the-child index) (the Tmax-list)))))


(define-object aircraft (base-object)

  :input-slots
  ((rho 0.73)
   (friction-coefficient 0.005))

  :computed-slots
  ((Sw (+ (* (the right-wing S) 2)
	  (* (the left-wing S) 2)
	  (the fuselage Sw))))

  :objects
  ((right-wing :type 'wing-with-tanks)
   (left-wing :type 'wing-with-tanks)
   (fuselage :type 'fuselage
	     :d 5 :l 42))
  

  :functions
  ((compute-friction-force (speed)
			   (* (the friction-coefficient)
			      1/2 
			      (the rho)
			      (expt speed 2) 
			      (the Sw)))))
