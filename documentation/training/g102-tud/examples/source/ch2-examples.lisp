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

(define-object wing-with-engines (empty-surface)
  :input-slots
  ((Tmax-list (list 1000 800 900 1200) :settable)
   (c-root 6 :settable)
   (c-tip 3 :settable)
   (other-inputs "..."))


  :computed-slots 
  ((Tmax-total (sum-elements (the engines) (the-element Tmax)))

   (other-slots "..."))

  :objects
  ((engines :type 'engine
            :sequence (:size (length (the Tmax-list)))
            :Tmax (nth (the-child index) (the Tmax-list)))))




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
   (fuselage :type 'fuselage))
  

  :functions
  ((compute-friction-force (speed)
			   (* (the friction-coefficient)
			      1/2 
			      (the rho)
			      (expt speed 2) 
			      (the Sw)))))
