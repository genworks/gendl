;;
;; Copyright 2012 Genworks International and the Delft University of
;; Technology
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GenDL).
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
