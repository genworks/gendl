;;
;; Copyright 2002-2011, 2012 Genworks International
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

(in-package :surf)


(define-object cardinal-spline (curve)

  :documentation (:description 
		  "This object makes a Cardinal Spline, which defaults
to a Catmull-Rom Spline for nil tension-params (which means they all
default to 0.0)."
		  :author "Dave Cooper, Genworks International")

  :input-slots ("List of 3D Points. The points through which the curve will pass." through-points
		(tension-params nil)
		("Boolean. Indicates whether the curve should close back to its start point. Default is <tt>nil</tt>."
		 periodic? nil)
		
		("Keyword symbol. <tt>uniform</tt>, <tt>:chordal</tt> (or <tt>:chord-length</tt>), or <tt>centripetal</tt>. 
Default is <tt>:uniform</tt>."
		 parameterization :uniform))

  :computed-slots ((native-curve-iw (make-cardinal-spline *geometry-kernel* 
							  :periodic? (the periodic?)
							  :tension-params (the tension-params)
							  
							  :control-points (if (and (the periodic?)
										   (coincident-point? (first (the through-points))
												      (lastcar (the through-points))))
									      (butlast (the through-points))
									      (the through-points))
							  :parameterization (the parameterization)))))


(define-object test-cr (base-object)
  :objects
  ((uniform :type 'test-uniform-cr)
   (centripetal :type 'test-centripetal-cr)
   (chordal :type 'test-chordal-cr)))

(define-object test-uniform-cr (cardinal-spline)

  :computed-slots 
  (
   (through-points (list (make-point 0 0 0)
                         (make-point 1 1 0)
                         (make-point 1.1 1 0)
                         (make-point 2 0 0)))

   #+nil
   (through-points
    '(#(0.0 -91.0 0.0) #(96.0 -82.0 0.0) #(100.0 -68.5 0.0)
      #(96.0 -54.99999999999999 0.0) #(0.0 -46.0 0.0)
      #(-96.0 -54.99999999999999 0.0) #(-100.0 -68.5 0.0) #(-96.0 -82.0 0.0)))

   (periodic? t)))

(define-object test-centripetal-cr (cardinal-spline)

  :computed-slots 
  ((parameterization :centripetal)
   #+nil
   (through-points (list (make-point 0 0 0)
                         (make-point 1 0 0)
                         (make-point 1 1 0)
                         (make-point 0 1 0)))
   #+nil
   (through-points (list (make-point 0 0 0)
                         (make-point 1 1 0)
                         (make-point 1.1 1 0)
                         (make-point 2 0 0)))

   (through-points (list (make-point 0 0 0)
                         (make-point 0.9 1 0)
                         (make-point 1.1 1 0)
                         (make-point 2 0 0)))

   #+nil
   (through-points
    '(#(0.0 -91.0 0.0) #(96.0 -82.0 0.0) #(100.0 -68.5 0.0)
      #(96.0 -54.99999999999999 0.0) #(0.0 -46.0 0.0)
      #(-96.0 -54.99999999999999 0.0) #(-100.0 -68.5 0.0) #(-96.0 -82.0 0.0)))

   (periodic? t)))

(define-object test-chordal-cr (cardinal-spline)

  :computed-slots 
  ((parameterization :chordal)
   (through-points (list (make-point 0 0 0)
                         (make-point 1 1 0)
                         (make-point 1.1 1 0)
                         (make-point 2 0 0)))


   #+nil
   (through-points
    '(#(0.0 -91.0 0.0) #(96.0 -82.0 0.0) #(100.0 -68.5 0.0)
      #(96.0 -54.99999999999999 0.0) #(0.0 -46.0 0.0)
      #(-96.0 -54.99999999999999 0.0) #(-100.0 -68.5 0.0) #(-96.0 -82.0 0.0)))
   
   (periodic? t)))





#+nil
(define-object test-b-spline-curves (base-object)

  :input-slots
  ((control-points (list (make-point 0 0 0)
                         (make-point 2 3.0 0.0) 
                         (make-point 4 2.0 0.0) 
                         (make-point 5 0.0 0.0) 
                         (make-point 4 -2.0 0.0) 
                         (make-point 2 -3.0 0.0) 
                         (make-point 0 0 0))))
  
  :hidden-objects
  ((view :type 'base-view
         :page-length (* 5 72) :page-width (* 5 72)
	 :pseudo-inputs (page-length page-width)
         :objects (the children)))

  :objects
  ((note :type 'general-note
         :strings "Hey Now")
   
   (curve :type 'b-spline-curve
           :control-points (the control-points)
           :degree 3
           :display-controls (list :line-thickness 1
                                   :color :orange))

   (kcurve :type 'b-spline-curve
           :control-points (the control-points)
	   :knot-vector '(0.0 0.0 0.0 0.0 1/8 1/2 6/8 1.0 1.0 1.0 1.0)
           :degree 3
           :display-controls (list :line-thickness 1
                                   :color :red))

   (points :type 'point
           :sequence (:size (length (rest (the control-points))))
           :center (nth (the-child index) (rest (the control-points)))
           :display-controls (list :color :green))))


#+nil
(defun normalize (&key knot-vector u-min u-max)
  
  (let* ((new-delta (- (apply #'max knot-vector) (apply #'min knot-vector)))
	 (old-delta (- u-max u-min))
	 (step-ratio (div old-delta new-delta)))
    (mapcar #'(lambda(knot) (* knot step-ratio)) knot-vector)))


#+nil
(define-object display-pts ()
  :objects
  ((through :type (if (typep self 'cardinal-spline) 'points-display 'null-object)
	    :points (when (typep self 'cardinal-spline) (the through-points))
	    :display-controls (list :color :green))

   (control :type 'points-display
	    :points (the control-points)
	    :display-controls (list :color :red))))

#+nil
(define-object cardinal-spline* (cardinal-spline display-pts))


#+nil
(define-object b-spline-curve* (b-spline-curve display-pts))

#+nil
(define-object tom-cr (cardinal-spline*)
  :computed-slots 
  ((periodic? t)
   (through-points 
    #+nil
    (list (make-point 0 0 0) (make-point 1 1 0)
	  (make-point 1.1 1 0) (make-point 2 0 0))

    '(#(0.0 -91.0 6.5) #(96.0 -82.0 6.5) #(100.0 -68.5 6.5)
      #(96.0 -54.99999999999999 6.5) #(0.0 -46.0 6.5)
      #(-96.0 -54.99999999999999 6.5) #(-100.0 -68.5 6.5) #(-96.0 -82.0 6.5)))))


#+nil
(define-object tom (base-object)
  :objects
  ((uniform :type 'tom-cr :parameterization :uniform)
   (centripetal :type 'tom-cr :parameterization :centripetal)
   (chordal :type 'tom-cr :parameterization :chordal)

   (alpha :type 'tom-cr :alpha 0.9)
   
   (frank :type 'b-spline-curve*
	  :control-points (the uniform control-points)


	  :knot-vector (the centripetal knot-vector)
	  #+nil
	  (normalize :knot-vector (the centripetal knot-vector)
		     :u-min (the uniform u-min)
		     :u-max (the uniform u-max)))

   (stein :type 'b-spline-curve*
	  :control-points (the centripetal control-points)
	  :knot-vector (the uniform knot-vector))


   (frank-chordal :type 'b-spline-curve*
		  :control-points (the uniform control-points)
		  :knot-vector (the chordal knot-vector))

   ))
