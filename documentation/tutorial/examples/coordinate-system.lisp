(in-package :gdl-user)

;;
;; FLAG -- this will be more useful as a view-scale-independent mixin

(define-object visible-coordinate-system (base-coordinate-system)


  :input-slots ((length 10) (width 10) (height 10)

		(show-axes? t :settable)

		(show-face-labels? t :settable)

		(grid-size 10)

		(grid-pitch (/ (the length) (the grid-size))))

  :objects
  ((box :type 'box
	:display-controls (list :color :orange :transparency 0.8))


   (x-pointer :type 'axis-pointer 
	      :hidden? (not (the show-axes?))
	      :label "X"
	      :display-controls (list :color :red)
	      :length (half (the length))
	      :orientation (alignment :front (the (face-normal-vector :right))))

   (x-lines :type 'line
	    :sequence (:size (1+ (the grid-size)))
	    :midpoint (translate (the (face-center :front)) :rear (* (the grid-pitch)
								     (the-child index)))
	    :pseudo-inputs (midpoint)
	    :start (translate (the-child midpoint) :left (half (the length)))
	    :end (translate (the-child midpoint) :right (half (the length))))
			       

   (y-pointer :type 'axis-pointer 
	      :hidden? (not (the show-axes?))
	      :label "Y"
	      :display-controls (list :color :green)
	      :length (half (the length))
	      :orientation (alignment :front (the (face-normal-vector :rear))))
   
   (y-lines :type 'line
	    :sequence (:size (1+ (the grid-size)))
	    :midpoint (translate (the (face-center :left)) :right (* (the grid-pitch)
								     (the-child index)))
	    :pseudo-inputs (midpoint)
	    :start (translate (the-child midpoint) :front (half (the length)))
	    :end (translate (the-child midpoint) :rear (half (the length))))


   (z-pointer :type 'axis-pointer 
	      :hidden? (not (the show-axes?))
	      :label "Z"
	      :display-controls (list :color :blue)
	      :length (half (the length))
	      :orientation (alignment :front (the (face-normal-vector :top))))

   (front-label :type 'general-note
		:hidden? (not (the show-face-labels?))
		:center (the (face-center :front))
		:orientation (alignment :top (the (face-normal-vector :front)))
		:character-size 3
		:strings "Front")

   (rear-label :type 'general-note
	       :hidden? (not (the show-face-labels?))
	       :center (the (face-center :rear))
	       :orientation (alignment :top (the (face-normal-vector :rear)))
	       :character-size 3
	       :strings "Rear")

   (right-label :type 'general-note
		:hidden? (not (the show-face-labels?))
		:center (the (face-center :right))
		:orientation (alignment :top (the (face-normal-vector :right))
					:rear (the (face-normal-vector :top)))
		:character-size 3
		:strings "Right")

   (left-label :type 'general-note
	       :hidden? (not (the show-face-labels?))
	       :center (the (face-center :left))
	       :orientation (alignment :top (the (face-normal-vector :left))
				       :rear (the (face-normal-vector :top)))
	       :character-size 3
	       :strings "Left")

   (top-label :type 'general-note
	      :hidden? (not (the show-face-labels?))
	      :center (the (face-center :top))
	      :orientation (alignment :top (the (face-normal-vector :top)))
	      :character-size 3
	      :strings "Top")

   (bottom-label :type 'general-note
		 :hidden? (not (the show-face-labels?))
		 :center (the (face-center :bottom))
		 :orientation (alignment :top (the (face-normal-vector :bottom)))
		 :character-size 3
		 :strings "Bottom")))



(define-object axis-pointer (cylinder)

  :input-slots (label (radius (/ (the length) 25)))

  :objects
  ((shaft :type 'cylinder 
	  :radius (the radius)
	  :center (translate (the center) :front (half (the length))))
   
   (tip :type 'cone
	:radius (the radius)
	:center (translate (the shaft center) :front (+ (half (the length))
							(half (the-child length))))
	:orientation (alignment :rear (the (face-normal-vector :front)))
	:length (/ (the length) 10))
   
   #+nil
   (note :type 'general-note
	 :display-controls (list :billboard t)
	 :center (translate (the tip center) :front (the-child character-size))
	 :strings (the label))))


