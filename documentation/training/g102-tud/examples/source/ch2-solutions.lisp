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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ATTENTION!!! 
ATTENTION!!! DO NOT LOOK AT THIS SOURCE UNTIL YOU TRY THE EXERCISES YOURSELF!!!
ATTENTION!!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :gdl-user)

(define-object fuselage (cylinder)

  :input-slots
  ((d 6)

   (l 40))

  :computed-slots
  ((A (* 1/4 pi (expt (the d) 2)))

   (C (* pi (the d)))

   (V (* (the A) (the l)))

   (Sw (* (the C) (the l)))

   (slenderness (/ (the l) (the d)))))


(define-object fuel-tank ()

  :input-slots
  (l w h)

  :computed-slots
  ((volume (* (the l) 
	      (the w) 
	      (the h)))))


(define-object wing-with-tanks (empty-surface)
  :input-slots
  ((Tmax-list (list 1000 800 900 1200) :settable)
   (b 30)
   (c-root 6 :settable)
   (c-tip 3 :settable)
   (tank-data (list (list :w 4 :l 3.5 :h .3 )
		    (list :w 2 :l 2.5 :h .2 )
		    (list :w 1.5 :l 2 :h 0.15)))

   (other-inputs "..."))


  :computed-slots 
  ((Tmax-total (sum-elements (the engines) (the-element Tmax)))
   
   (tank-volume (sum-elements (the fuel-tanks) (the-element volume)))

   (c-avg (/ (+ (the c-root) (the c-tip)) 2))
   (taper (/ (the c-tip) (the c-root)))

   (S (* (the b) (the c-avg)))
   (A (/ (expt (the b) 2) (the S))))

  :objects
  ((engines :type 'engine
            :sequence (:size (length (the Tmax-list)))
            :Tmax (nth (the-child index) (the Tmax-list)))
   

   (fuel-tanks :type 'fuel-tank
	       :sequence (:size (length (the tank-data)))
	       :l (getf (nth (the-child index) (the tank-data)) :l)
	       :w (getf (nth (the-child index) (the tank-data)) :w)
	       :h (getf (nth (the-child index) (the tank-data)) :h))))


(define-object aircraft-with-lift (base-object)

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
   (fuselage :type 'fuselage))
  

  :functions
  ((compute-friction-force 
    (speed)
    (* (the friction-coefficient) 1/2 (the rho)
       (expt speed 2) (the Sw)))

   (compute-lift-force 
    (speed CL)
    (* CL 1/2 (the rho)
       (expt speed 2) (+ (the right-wing S)
			 (the left-wing  S))))))
