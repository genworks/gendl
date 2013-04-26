(in-package :gdl-user)


(define-object cyl-fields (base-object)

  :input-slots ((number-of-layers 3)
		
		(height 20)

		(datum (the (face-center :bottom)))
		
		(pitch (/ (the height) (1- (the number-of-layers))))

		
		(colors (list :red :orange :yellow :green :blue :indigo :violet)))



  :objects ((fields :type 'cyl-field
		    :display-controls (list :color (cyclic-nth (the-child index) (the colors)))
		    :sequence (:size (the number-of-layers))
		    :center (translate (the datum) :up (* (the-child index) (the pitch))))))



(define-object cyl-field (base-object)

  :input-slots ((number-of-rows 10)
		(number-of-columns (floor (* (the number-of-rows) +phi+)))
		(cylinder-radius 2) (cylinder-length 3)

		(width (twice (* (the cylinder-radius) (the number-of-rows))))
		(length (twice (* (the cylinder-radius) (the number-of-columns))))
		)

  :objects
  ((cylinders :type 'cylinder
	      :sequence (:matrix :lateral (the number-of-rows) :longitudinal (the number-of-columns))
	      :radius (the cylinder-radius)
	      :length (the cylinder-length)
	      :orientation (alignment :rear (the (face-normal-vector :top)))
	      )))




(define-object cyl-centered (base-object)

  :input-slots ((number-of-rows 10)
		(number-of-columns (floor (* (the number-of-rows) +phi+)))
		(cylinder-radius 2) (cylinder-length 3)

		(width (twice (* (the cylinder-radius) (the number-of-rows))))
		(length (twice (* (the cylinder-radius) (the number-of-columns))))
		)

  :objects
  ((cylinders :type 'cylinder
	      :sequence (:matrix :lateral (the number-of-rows) :longitudinal (the number-of-columns))
	      :radius (the cylinder-radius)
	      :length (the cylinder-length)
	      :center (translate (the center) 
				 :left (* (first (the-child index)) (* 7/4 (twice (the-child radius))))
				 :rear (* (second (the-child index)) (* 8/3 (twice (the-child radius)))))
	      :orientation (alignment :rear (the (face-normal-vector :top)))
	      )))
