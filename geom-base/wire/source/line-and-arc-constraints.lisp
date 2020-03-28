;;
;; Copyright 2012 Genworks International and Liberating Insight
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


(in-package :geom-base)

(define-object constrained-line (line)

  :documentation (:description "This object is intended to simplify the process of
   constructing lines using various constraints. Currently supported
   are 2 through-points or 1 through-point and at-angle. Note the
   line-constraints must be an evaluatable s-expression as this is not
   processed as a macro")

  :input-slots
  (line-constraints)

  :computed-slots
  ((trim-start (getf (the line-constraints) :trim-start))

   (trim-end (getf (the line-constraints) :trim-end))

   (constraints (let ((lst (the line-constraints)))
                  (dolist (key (list :trim-start :trim-end) lst)
                    (setq lst (remove-plist-key lst key)))))

   (keys (plist-keys (the constraints)))

   (exprs (plist-values (the constraints)))

   (constraint-type 
    (cond ((every #'(lambda (key)
		      (eql :through-point key))
		  (the keys))
	   :2-points)
	  ((and (getf (the constraints) :through-point)
		(getf (the constraints) :at-angle))
	   :point-angle)
	  ((and (getf (the constraints) :tangent-to)
		(getf (the constraints) :at-angle))
	   :tangent-angle)

	  ((let ((keys (the keys)))
	     (and (= (length keys) 3)
		  (= (count :tangent-to keys) 1) (= (count :through-point keys) 1)
		  (= (count :side-vector keys) 1)))
	   :tangent-through-point)
	  
	  ((= (count :tangent-to (the keys)) 2)
                                        ;:tangent-tangent
	   (error "ICAD COMPAT line-constrains for keys tangent-to and tangent-to not yet implemented"))
	  (t (error "Constrained-Line -- constraint configuration is not yet supported"))))

   (start (if (the trim-start)
              (proj-point-on-line (the trim-start)
                                  (the constraint-object start)
                                  (the constraint-object direction-vector))
            (the constraint-object start)))

   (end (if (the trim-end)
            (proj-point-on-line (the trim-end)
                                (the constraint-object start)
                                (the constraint-object direction-vector))
          (the constraint-object end))))

  :hidden-objects
  ((constraint-object :type (case (the constraint-type)
			      (:2-points 'line-constraints-2-points)
			      (:point-angle 'line-constraints-point-angle)
			      (:tangent-angle 'line-constraints-tangent-angle)
			      (:tangent-through-point 'line-constraints-tangent-through-point))
                      :constraints (the constraints)))

  :functions
  (("Icad Compat function"
    tangent-point (constraint-index)
    (when (member (the constraint-type) (list :tangent-angle :tangent-tangent))
      (case constraint-index
	    (0 (the constraint-object start))
	    (1 (the constraint-object end)))))

   (angle-at-tangent-point 
    (constraint-index)
    (declare (ignore constraint-index))
    (error "The ICAD COMPAT function Angle-at-tangent-point has not been implemented"))))

(define-object line-constraints-2-points ()

  :input-slots
  (constraints)

  :computed-slots
  ((start (nth 1 (the constraints)))
   (end (nth 3 (the constraints)))

   (direction-vector (subtract-vectors (the end) (the start)))))

(define-object line-constraints-point-angle (base-object)

  :input-slots
  (constraints)

  :computed-slots
  ((start (let ((pos (position :through-point (the constraints))))
            (nth (1+ pos) (the constraints))))

   (at-angle (let ((pos (position :at-angle (the constraints))))
               (nth (1+ pos) (the constraints))))

   (angle-key (getf (the at-angle) :angle))

   (angle (case (the angle-key)
            ((:normal :perpendicular) pi/2)
            (:parallel 0)
            (t (the angle-key)))) ;; angle-key could simply be a number

   (reference-vector (or (getf (the at-angle) :reference-vector)
                         (the (face-normal-vector :right))))

   (length (getf (the at-angle) :length))

   (plane-normal (or (getf (the at-angle) :plane-normal)
                     (the (face-normal-vector :top))))

   (translation-vector (rotate-vector (the reference-vector)
                                      (the angle)
                                      (the plane-normal)))

   (end (translate-along-vector 
	 (the start) (the translation-vector) (the length)))

   (direction-vector (the reference-vector))))

(define-object line-constraints-tangent-angle (base-object)

  :input-slots
  (constraints)

  :computed-slots
  (;;-------------  Break out the constraint keywords --------------
   (tangent-to-expr (getf (the constraints) :tangent-to))

   (arc (getf (the tangent-to-expr) :arc))

   (arc-geometry (getf (the tangent-to-expr) :arc-geometry))

   (arc-center (if (the arc-geometry)
                   (getf (the arc-geometry) :center)
                 (the arc center)))

   (arc-radius (if (the arc-geometry)
                   (getf (the arc-geometry) :radius)
                 (the arc radius)))

   (arc-side-vector (if (the arc-geometry)
                        (getf (the arc-geometry) :side-vector)
                      (getf (the tangent-to-expr) :side-vector)))

   (arc-plane-normal (if (the arc-geometry)
                        (getf (the arc-geometry) :side-vector)
                       (the arc (face-normal-vector :top))))

   (at-angle-expr (getf (the constraints) :at-angle))

   (angle (getf (the at-angle-expr) :angle))

   (length (getf (the at-angle-expr) :length))

   (reference-vector (getf (the at-angle-expr) :reference-vector))

   ;;;-----------  Calculation ----------------------------------
   (circle-intersection-vector (cross-vectors (the arc-plane-normal)
                                              (the reference-vector)))

   (start (inter-line-sphere (the arc-center)
                             (the circle-intersection-vector)
                             (the arc-center)
                             (the arc-radius)
                             (the arc-side-vector)))

   (end (translate-along-vector (the start)
                                (the reference-vector)
                                (the length)))

   (direction-vector (the reference-vector))))



(define-object line-constraints-tangent-through-point (base-object)

  :input-slots
  (constraints)

  :computed-slots
  (;;-------------  Break out the constraint keywords --------------

   (through-point (getf (the constraints) :through-point))
   
   (tangent-to-expr (getf (the constraints) :tangent-to))

   (arc (getf (the tangent-to-expr) :arc))

   (arc-geometry (getf (the tangent-to-expr) :arc-geometry))

   (arc-center (if (the arc-geometry)
                   (getf (the arc-geometry) :center)
                 (the arc center)))

   (arc-radius (if (the arc-geometry)
                   (getf (the arc-geometry) :radius)
                 (the arc radius)))

   (side-vector (getf (the constraints) :side-vector))

   (arc-plane-normal (if (the arc-geometry)
                        (getf (the arc-geometry) :side-vector)
                       (the arc (face-normal-vector :top))))


   ;;;-----------  Calculation ----------------------------------

   (new-circle-center (midpoint (the arc-center) (the through-point)))

   (end-1 (inter-circle-circle (the arc-center) (the arc-radius)
			       (the new-circle-center) (3d-distance (the new-circle-center) (the arc-center))
			       t))

   (end-2 (inter-circle-circle (the arc-center) (the arc-radius)
			       (the new-circle-center) (3d-distance (the new-circle-center) (the arc-center))
			       nil))

   (start (the through-point))

   (end-1-angle-between (angle-between-vectors (the side-vector)
					       (subtract-vectors (the end-1) (the arc-center))
					       (the (face-normal-vector :top))
					       :-ve t))
   
   (end-2-angle-between (angle-between-vectors (the side-vector)
					       (subtract-vectors (the end-2) (the arc-center))
					       (the (face-normal-vector :top))
					       :-ve t))

   (end (if (< (the end-1-angle-between) (the end-2-angle-between))
	    (the end-1) (the end-2)))))





(defun reverse-plist (plist)
  (let (result)
    (mapc #'(lambda(key value)
	      (push value result)
	      (push key result))
	  (plist-keys plist) (plist-values plist)) result))



(define-object constrained-arc (arc)

  :documentation
  (:description 
   "This object is intended to simplify the process of
   constructing lines using various constraints. Currently supported
   are 2 through-points or 1 through-point and at-angle. Note the
   line-constraints must be an evaluatable s-expression as this is not
   processed as a macro")

  :input-slots
  (arc-constraints)

  :computed-slots
  ((center (the constraint-object center))
   (radius (the constraint-object radius))

   (orientation (the constraint-object orientation))
   
   (first-tangent-index (position :tangent-to (plist-keys (the arc-constraints))))
   (second-tangent-index (position :tangent-to (plist-keys (the arc-constraints)) :from-end t))

   (constraint-type
    (let ((constraints (the arc-constraints)))
      (cond ((let ((first-tangent (getf (the arc-constraints) :tangent-to))
		   (second-tangent (getf (reverse-plist (the arc-constraints)) :tangent-to))
		   (radius (getf (the arc-constraints) :radius)))
	       (or 
		(and (eql (first first-tangent) :arc-geometry)
		     (eql (first second-tangent) :line-geometry) radius)
		(and (eql (first first-tangent) :line-geometry)
		     (eql (first second-tangent) :arc-geometry) radius)))
	     :tangent-arc-tangent-line-radius)


	    ((and (getf plist :tangent-to)(getf plist :center-on)(getf plist :through-point) (getf plist :select-side))
	     :tangent-to--center-on--through-point--select-side)
	    

	    ((let ((keys (plist-keys (the arc-constraints))))
	       (and (= (length keys) 3) (= (count :through-point keys) 2) (= (count :tangent-to keys) 1)))
	     :tangent-to-through-two-points)
	  
	    ((equalp (plist-keys (the arc-constraints)) '(:through-point :through-point :through-point))
	     :through-three-points)

	    ((and (getf constraints :center) (getf constraints :radius) (getf constraints :plane-normal))
	     :center-radius-plane-normal)

	    (t (error "Constrained-arc --- constraint configuration is not yet supported"))))))
  
  :hidden-objects
  ((constraint-object :type (ecase (the constraint-type)
			      (:tangent-arc-tangent-line-radius 
			       'arc-constraints-tangent-arc-tangent-line-radius)
			      (:through-three-points
			       'arc-constraints-through-three-points)
			      (:tangent-to-through-two-points
			       'arc-constraint-tangent-to-through-two-points)
			      (:center-radius-plane-normal
			       'arc-constraint-center-radius-plane-normal)
			      (:tangent-to--center-on--through-point--select-side
			       'arc-constraint-tangent-to--center-on--through-point--select-side))
		      :pass-down (first-tangent-index)
                      :constraints (the arc-constraints)))
  
  :functions
  ((tangent-point 
    (constraint-index)
    (the constraint-object (tangent-point constraint-index)))))


(define-object constrained-fillet (constrained-arc)

  :documentation
  (:description 
   "This object is the same as constrained-arc, but it is only
meaningful for arc-constraints which contain two :tangent-to clauses,
and it automatically trims the result to each point of tangency")

  :computed-slots 
  ((angle-0 0)
   (angle-1 (angle-between-vectors (subtract-vectors (the (tangent-point (the first-tangent-index))) (the center))
				     (subtract-vectors (the (tangent-point (the second-tangent-index))) (the center))
				     (the (face-normal-vector :top))
				     :-ve t))
   (start-angle (min (the angle-0) (the angle-1)))
   (end-angle (max (the angle-0) (the angle-1)))))


(define-object constrained-arc-base-mixin (base-object)
  :input-slots (constraints center radius))



(define-object arc-constraint-tangent-to--center-on--through-point--select-side (constrained-arc-base-mixin)

  
  
  )



(define-object arc-constraint-center-radius-plane-normal (constrained-arc-base-mixin)

  :computed-slots ((center (getf (the constraints) :center))
		   (radius (getf (the constraints) :radius))
		   (orientation (alignment :top (getf (the constraints) :plane-normal)))))
  
  


(define-object arc-constraint-tangent-to-through-two-points (base-object)

  :input-slots (constraints first-tangent-index) ;; first-tangent-index is ignored for this case.

  :computed-slots ((point-P (getf (the constraints) :through-point))
		   (point-Q (getf (reverse-plist (the constraints)) :through-point))
		   (tangent-to (getf (the constraints) :tangent-to))
		   (tangent-to-line (getf (the tangent-to) :line)) ;; FLAG -- add arc/circle case. 
		   (tangent-to-side-vector (getf (the tangent-to) :side-vector))


		   (P-Q-vector (subtract-vectors (the point-Q) (the point-P)))
		   (parallel-case? (parallel-vectors? (the tangent-to-line direction-vector) (the P-Q-vector)))

		   (point-T (when (the parallel-case?)
			      (let ((bisector-point (midpoint (the point-P) (the point-Q)))
				    (bisector-vector (cross-vectors (the P-Q-vector) (the (face-normal-vector :top)))))
				(inter-line-line bisector-point bisector-vector
						 (the tangent-to-line start) (the tangent-to-line direction-vector)))))


		   (point-G (unless (the parallel-case?)
			      (inter-line-line (the point-P) (the P-Q-vector)
					       (the tangent-to-line start) (the tangent-to-line direction-vector))))

		   (circle-C-center (midpoint (the point-P) (the point-Q)))

		   (circle-C-radius (3d-distance (the circle-C-center) (the point-P)))

		   (center (if (the parallel-case?)
			       (the parallel-case-arc center)
			       (the constrained-circle center)))

		   (radius (if (the parallel-case?)
			       (the parallel-case-arc radius)
			       (the constrained-circle radius)))

		   (point-A (the tangent-line end))

		   (circle-D-center (the point-G))

		   (circle-D-radius (3d-distance (the point-G) (the point-A)))

		   (point-T1 (inter-line-sphere (the tangent-to-line start)
						(the tangent-to-line direction-vector)
						(the circle-D-center) 
						(the circle-D-radius)
						(the tangent-to-side-vector))))


  :objects ((parallel-case-arc :type (if (the parallel-case?) 'constrained-arc 'null-object)
			       :arc-constraints (list :through-point (the point-P)
						      :through-point (the point-Q)
						      :through-point (the point-T)))

	    (tangent-line :type 'constrained-line
			   :line-constraints (list :through-point (the point-G)
						   :tangent-to (list :arc-geometry
								     (list :center (the circle-C-center)
									   :radius (the circle-C-radius)))
						   :side-vector (cross-vectors (subtract-vectors (the point-G)
												 (the circle-C-center))
									       (the (face-normal-vector :top)))))
	    (constrained-circle :type 'constrained-arc
				:arc-constraints (list :through-point (the point-P)
						       :through-point (the point-Q)
						       :through-point (the point-T1)))))


  

(define-object arc-constraints-through-three-points (base-object)
  :input-slots (constraints first-tangent-index ;; first-tangent-index is ignored for this case. 
			    (length-backoff 99/100))


  :hidden-objects ((sphere-a :type 'sphere :radius (* 1/20 (the radius)) :center (the point-a))
		   (sphere-b :type 'sphere :radius (* 1/20 (the radius)) :center (the point-b))
		   (sphere-c :type 'sphere :radius (* 1/20 (the radius)) :center (the point-c)))
		   
  
  :computed-slots  ((point-a (getf (the constraints) :through-point))
		    (point-b (getf (rest (rest (the constraints))) :through-point))
		    (point-c (getf (rest (rest (rest (rest (the constraints))))) :through-point))

		    (a-b-length (3d-distance (the point-a) (the point-b)))
		    (a-b-vector (subtract-vectors (the point-b) (the point-a)))
		    (a-b-bisector-start (midpoint (the point-a) (the point-b)))
		    (a-b-bisector-vector (cross-vectors (the a-b-vector) (the (face-normal-vector :top))))
		    

		    (b-c-length (3d-distance (the point-b) (the point-c)))
		    (b-c-vector (subtract-vectors (the point-c) (the point-b)))
		    (b-c-bisector-start (midpoint (the point-b) (the point-c)))
		    (b-c-bisector-vector (cross-vectors (the b-c-vector) (the (face-normal-vector :top))))
		    

		    (center (inter-line-line (the a-b-bisector-start) (the a-b-bisector-vector)
					     (the b-c-bisector-start) (the b-c-bisector-vector)))

		    (radius (3d-distance (the center) (the point-a)))))

  

  
(define-object arc-constraints-tangent-arc-tangent-line-radius (base-object)

  :input-slots (constraints first-tangent-index)

  :computed-slots ((radius (getf (the constraints) :radius))

		   (tangent-specs (list (getf (the constraints) :tangent-to)
					(getf (reverse-plist (the constraints)) :tangent-to)))

		   (arc-geometry (second (find :arc-geometry (the tangent-specs) :key #'first)))

		   (line-geometry (second (find :line-geometry (the tangent-specs) :key #'first)))
		   
		   (orientation 
		    (alignment :top (getf (the arc-geometry) :plane-normal)
			       :right (subtract-vectors (the (tangent-point (the first-tangent-index)))
							(the center))))
		   
		   (line-normal (cross-vectors (getf (the line-geometry) :direction-vector)
					       (getf (the arc-geometry) :plane-normal)))
		   

		   (active-sphere (the (spheres (if (getf (the arc-geometry) :inside?) 0 1))))

		   (active-line (the (parallels 
				      (if (or (null (getf (the line-geometry) :side-vector))
					      (< (angle-between-vectors 
						  (the line-normal)
						  (getf (the line-geometry) :side-vector))
						 pi/2)) 0 1))))

		   (select-side (or (getf (the constraints) :select-side)
				    (getf (the line-geometry) :direction-vector)))
		   
		   (center (inter-line-sphere (the active-line start)
					      (the active-line direction-vector)
					      (the active-sphere center)
					      (the active-sphere radius)
					      (the select-side))))

  :objects ((constraint-objects :type 
				'arc-constraints-tangent-arc-tangent-line-radius-constraint-object
				:sequence (:size (length (plist-keys (the constraints))))
				:key (nth (the-child index) (plist-keys (the constraints)))
				:value (nth (the-child index) (plist-values (the constraints)))
				:result-center (the center)
				:result-radius (the radius)
				:reference-object (cond ((and (eql (the-child key) 
								   :tangent-to)
							   (eql (first (the-child value)) 
								:line-geometry))
							 (the-child reference-line))
							((and (eql (the-child key) 
								   :tangent-to)
							      (eql (first (the-child value)) 
								   :arc-geometry))
							 (the-child reference-arc))
							(t nil))

				:reference-line (the reference-line)

				:reference-arc (the reference-arc))

	    (reference-arc :type 'arc
			   :center (getf (the arc-geometry) :center)
			   :radius (getf (the arc-geometry) :radius))

	    (reference-line :type 'line
			    :start (getf (the line-geometry) :line-point)
			    :end (translate-along-vector (the-child start)
							 (getf (the line-geometry) :direction-vector)
							 (the radius)))

	    (parallels :type 'line
		       :sequence (:size 2)
		       :start (translate-along-vector (getf (the line-geometry) :line-point)
						      (ecase (the-child index)
							(0 (the line-normal))
							(1 (reverse-vector (the line-normal))))
						      (the radius))
		       :end (translate-along-vector (the-child start) 
						    (getf (the line-geometry) :direction-vector)
						    (getf (the arc-geometry) :radius)))

	    (spheres :type 'sphere
		     :sequence (:size 2)
		     :center (getf (the arc-geometry) :center)
		     :radius (funcall (ecase (the-child index)
					(0 #'-) (1 #'+))
				      (getf (the arc-geometry) :radius) (the radius))))
  :functions ((tangent-point 
	       (constraint-index)
	       (the (constraint-objects constraint-index) tangent-point))))


(define-object arc-constraints-tangent-arc-tangent-line-radius-constraint-object (base-object)
  :input-slots (key value reference-object reference-line reference-arc result-center result-radius)

  :computed-slots ((tangent-point (etypecase (the reference-object)
				    (line (inter-line-plane 
					   (the reference-line start)
					   (the reference-line direction-vector)
					   (the result-center)
					   (the reference-line direction-vector)))
				    (arc (inter-line-sphere
					  (the reference-arc center)
					  (subtract-vectors (the result-center)
							    (the reference-arc center))
					  (the reference-arc center)
					  (the reference-arc radius)
					  (subtract-vectors (the result-center)
							    (the reference-arc center))
					  ))))))


(define-object test-arc (base-object)

  :objects ((arc :type 'constrained-arc
		 :arc-constraints (list :through-point (make-point 0 0 0)
					:through-point (make-point 1 0 0)
					:through-point (make-point 2 1 0))
					
					
		 
		 :radius 10 :center (make-point 0 0 0)
		 :end-angle (the-child (angle-at-point (make-point 0 1 0))))))


(define-object test-constrained-arc-2 (constrained-arc)
  :computed-slots
  ((arc-constraints (list :tangent-to (list :line (the tangent-line) :side-vector (make-vector -1 0 0))
			  :through-point (make-point -15 5 0)
			  :through-point (make-point 15 5 0))))

  :hidden-objects ((tangent-line :type 'line
				 :start (make-point -15 0 0)
				 :end (make-point 15 1 0))

		   (sphere-P :type 'sphere
			     :radius 1
			     :center (the constraint-object point-P))
		   (sphere-Q :type 'sphere
			     :radius 1
			     :center (the constraint-object point-Q))))
			  

(define-object test-constrained-arc (constrained-arc)

  :computed-slots 
  ((arc-constraints (list :tangent-to 
			  (list :arc-geometry 
				(list :center (the circle center)
				      :radius (the circle radius)
				      :plane-normal (the circle (face-normal-vector :top))
				      :inside? t))
			  :tangent-to (list :line-geometry 
					    (list :line-point (the line start)
						  :direction-vector 
						  (unitize-vector (the line direction-vector))
						  :side-vector (make-vector -1 0 0)
						  ))
			  :radius 3)))

   
  
  :objects ((arc-tangent-point :type 'point
			       :center (the (tangent-point 0)))
	    
	    (line-tangent-point :type 'point
				:center (the (tangent-point 1)))
	    
	    #+nil
	    (pts :type 'point
		 :display-controls (list :color :blue)
		 :sequence (:size 50)
		 :center (translate-along-vector (the center)
						 (rotate-vector
						  (the (face-normal-vector :right))
						  (* (the-child index)
						     (div 2pi (the-child aggregate 
									 number-of-elements)))
						  (the (face-normal-vector :top)))
						 (the radius)))


	    #+nil
	    (circ-pts :type 'point
		      :display-controls (list :color :red)
		      :sequence (:size 200)
		      :center (translate-along-vector (the circle center)
						      (rotate-vector
						       (the (face-normal-vector :right))
						       (* (the-child index)
							  (div 2pi (the-child 
								    aggregate number-of-elements)))
						       (the (face-normal-vector :top)))
						      (the circle radius)))

	    (line :type 'line 
		  :start (make-point 1 1 0)
		  :end (translate-along-vector (the-child start)
					       (make-vector 0 1 0)
					       10))

	    (circle :type 'arc
		    :center (make-point 0 0 0)
		    :start-angle (angle-between-vectors (the-child (face-normal-vector :right))
							(subtract-vectors (the (tangent-point 0))
									  (the center))
							(the (face-normal-vector :top))
							:-ve t)
		    :end-angle 0
		    :orientation (alignment :top (make-vector 0 0 1))

		    :radius 10)))



		
		
