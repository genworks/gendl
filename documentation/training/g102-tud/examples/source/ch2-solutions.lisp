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
