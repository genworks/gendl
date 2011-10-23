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


(in-package :gdl-user)

#+nil
(define-object ta2.1-test-part (base-object)

  :input-slots
  ((control-points (list (make-point 0 0 0)
			 (make-point 2 3.0 0.0) 
			 (make-point 4 2.0 0.0) 
			 (make-point 5 0.0 0.0) 
			 (make-point 4 -2.0 0.0) 
			 (make-point 2 -3.0 0.0) 
			 (make-point 0 0 0)))
   (text "hello world" :settable)
   )
  
  :hidden-objects
  ((view :type 'base-view
         :page-length (* 5 72) :page-width (* 5 72)
         :objects (the children)))

  :objects
  ((curves :type 'b-spline-curve-new-base-object
           :sequence (:size 6)
	   :control-points (the control-points)
	   :degree (1+ (the-child :index))
	   :display-controls (if (the-child highlighted?)
				 (the-child aux-display-controls)
			       (list :line-thickness (* .3 (the-child index))
				     :color (ecase (the-child index)
					      (0 :red) (1 :orange) (2 :yellow) (3 :green)
					      (4 :blue) (5 :red-violet)))))
   
   (curve-collection :type 'a-collection-curves
		     :control-points (the control-points))
   
   (xbox :type 'my-box
	 :width 2 
	 :length 2
	 :height 3)
	     
   
   #+nil
   (points :type 'point
	   :sequence (:size (length (rest (the control-points))))
	   :center (nth (the-child index) (rest (the control-points)))
	   :display-controls (list :color :green))))


;;
;;FLAG -- Svde @ 14-08-09 -- This define-object has slots that can be incorporated into base-object itself,
;; to enable highlighting of objects in the viewport by switching between the 'aux-display-controls' and 'display-controls' using
;; the boolean 'highlighted?'.
;;
#+nil
(define-object b-spline-curve-new-base-object (b-spline-curve)
  
  :input-slots
  (("optional input-slot that defines the display properties of the object when highlighted in png, jpg, svg and vrml image formats. 
The aux-display-controls are :settable, enabling to define your own highlight settings. aux-display-controls contains a list with the following two keys 
:line-thickness and :color. The aux-display controls defaults to (list :line-thickness 3 :color :orange)"
    aux-display-controls (list :line-thickness 3 :color :orange) :settable)
   ("optional boolean input-slot that indicates whether the object is highlighted in png and jpg image formats. If true, the object will be displayed showing the aux-display-controls. Defaults to nil."
    highlighted? nil :settable))
  )

#+nil
(define-object my-box (box)
  :input-slots
  ((width nil :settable)
   (length nil :settable)
   
   (aux-display-controls (list :line-thickness 3 :color :orange) :settable)
   (display-controls (if (the highlighted?)
				 (the aux-display-controls)))
   (highlighted? nil :settable)
   
   (strings-for-display (the name-for-display) :settable)
   )
  )

#+nil
(define-object a-collection-curves (base-object)
  :input-slots
  (control-points)
  
  :hidden-objects
  ()
  
  :objects
  ((curves :type 'b-spline-curve;;-new-base-object
           :sequence (:size 6)
           :control-points (the control-points)
	   :degree (1+ (the-child :index))

           :display-controls (list :line-thickness (* .3 (the-child index))
                                   :color (ecase (the-child index)
                                            (0 :red) (1 :orange) (2 :yellow) (3 :green)
                                            (4 :blue) (5 :red-violet))))
   ))



  
