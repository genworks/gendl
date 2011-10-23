(in-package :missile)

(defparameter *brep-isos-default* (list :n-u 11 :n-v 11))

(define-object assembly (application-mixin)

  
  :computed-slots ((plane-tilt 20 :settable)
		   (plane-offset 5 :settable)
		   (plane-center (translate  (the center)  :rear (the plane-offset)))
		   (plane-normal (rotate-vector-d (the (face-normal-vector :rear))
						  (the plane-tilt)
						  (the (face-normal-vector :right))))
		   (ui-display-list-objects (list (the (halves 0))
						  (the top-cog cog-sphere)
						  (the (halves 1))
						  (the bottom-cog cog-sphere)
						  )))


  :objects
  (
   (tank :type 'revolved-surface
	 :curve (the polyline)
	 :axis-vector (the (face-normal-vector :rear)))


   
   (boxes :type 'box-solid
	  :sequence (:size 2)
	  :rear-vector (ecase (the-child index) (0 (the plane-normal)) (1 (reverse-vector (the plane-normal))))
	  :center (translate-along-vector (the plane-center) (the-child rear-vector) (half (the-child length)))
	  :orientation (alignment :rear (the-child rear-vector) :top (the (:face-normal-vector :top)))
	  :width 50 :height 50 :length 50)
   
   (halves :type 'intersected-solid
	   :sequence (:size 2)
	   :display-controls (list :line-thickness 2 :color (ecase (the-child index) (0 :green) (1 :red)) :transparency 0.5)
	   ;;:brep (the tank sewn-brep)
           :brep (the tank)
	   :other-brep (the (boxes (the-child index))))
   
   (polyline :type 'surf:global-filleted-polyline-curve
	     :vertex-list (list (make-point 0 20 0)
				(make-point 10 20 0)
				(make-point 10 -20 0)
				(make-point 0 -20 0))
	     :default-radius 9.9)
   
   
   (total-volume :type 'gwl-rule-object
		 :rule-title "Total Volume"
		 ;;:rule-result (format nil "~2,3f" (the tank sewn-brep volume))
                 :rule-result (format nil "~2,3f" (the tank volume)))
   
   (top-volume :type 'gwl-rule-object
	       :rule-title "Empty Volume"
	       :rule-result (format nil "~2,3f" (the (halves 0) volume)))
   
   (bottom-volume :type 'gwl-rule-object
		  :rule-title "Total Volume"
		  :rule-title "Full Volume"
		  :rule-result (format nil "~2,3f" (the (halves 1) volume)))
   
   (tank-cog :type 'rule-cog
	     :rule-title "Tank CoG"
	     ;;:object-to-display (the tank sewn-brep)
             :object-to-display (the tank)
	     :rule-result (let ((cog ;;(the tank sewn-brep center-of-gravity)
                                     (the tank center-of-gravity)
                                     ))
			    (format nil "[~a, ~a, ~a]" 
				    (number-round (get-x cog) 1)
				    (number-round (get-y cog) 1)
				    (number-round (get-z cog) 1))))

   (top-cog :type 'rule-cog
	    :rule-title "Empty Half CoG"
	    :object-to-display (the  (halves 0))
	    :rule-result (let ((cog (the (halves 0) center-of-gravity)))
			    (format nil "[~a, ~a, ~a]" 
				    (number-round (get-x cog) 1)
				    (number-round (get-y cog) 1)
				    (number-round (get-z cog) 1))))
   
   (bottom-cog :type 'rule-cog
	       :rule-title "Full Half CoG"
	       :object-to-display (the  (halves 1))
	       :rule-result (let ((cog (the (halves 1) center-of-gravity)))
			      (format nil "[~a, ~a, ~a]" 
				      (number-round (get-x cog) 1)
				      (number-round (get-y cog) 1)
				      (number-round (get-z cog) 1))))))

  
;;
;; Defines the user inputs
;;

(define-lens (html-format assembly)()
  :output-functions
  ((model-inputs
    ()
    (html (:table (:tr ((:td  :bgcolor :yellow) "Cutting Plane Offset") (:td ((:input :type :text :name :plane-offset :value (the plane-offset)))))
		  (:tr ((:td  :bgcolor :yellow) "Cutting Plane Angle") (:td ((:input :type :text :name :plane-tilt :value (the plane-tilt))))))
	  (:p ((:input :type :submit :name :submit :value " OK ")))))))



(define-object rule-cog (gwl-rule-object)

  :input-slots
  (object-to-display)
  
  :computed-slots
  ((rule-description "This displays the Center of Gravity for either the Full or Empty portion of the tank, 
or for a (theoretically full) whole tank."))

  
  
  
  :objects
  ((cog-sphere :type 'sphere
	       :color :black
	       :radius 1.5
	       :center (the object-to-display center-of-gravity)))
  
  :hidden-objects
   
  ((view-object :type 'web-drawing
		:objects (list (the object-to-display) (the cog-sphere))
		:page-width 500
		:page-length 500
		:projection-vector (getf *standard-views* (the view)))))
  

(define-view (html-format rule-cog) ()

  :output-functions
  ((bottom-user-area () 
		     (with-html-form () (the write-geometry)))))


