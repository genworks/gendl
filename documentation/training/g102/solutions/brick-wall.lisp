(in-package :gdl-user)


(define-object brick-wall (base-object)

  :input-slots ((width 250)
		(height 100)
		(length (the brick-length))
		(spacing (* (the brick-height) 0.07)))

  :computed-slots 
  ((bricks-in-row (floor (/ (the width) (the brick-width))))
   (number-of-rows (floor (/ (the height) (the brick-height))))

   (brick-width (the sample-brick width))
   (brick-height (the sample-brick height))
   (brick-length (the sample-brick length))

   (corner (translate (the center) 
		      :down (half (the height))
		      :left (half (the width))))

   (extra-even-space (/ (the spacing) (1- (the bricks-in-row)))))

  
  :objects 
  ((sample-brick :type 'brick :hidden? t)
   

   (rows :type 'brick-row
	 :sequence (:size (the number-of-rows))
	 :number-of-bricks (the bricks-in-row)
	 :pass-down (brick-width brick-height corner spacing extra-even-space)
	 :height (the brick-height)
	 :width (the width)
	 :center (translate (if (the-child first?) (the corner) (the-child previous center))
			    :up (if (the-child first?) (half (the-child height))
				    (+ (the-child height) (the spacing)))))))

(define-object brick-row (base-object)

  :input-slots (number-of-bricks brick-width brick-height corner spacing extra-even-space)
  
  :computed-slots ((odd-row? (and (the index) (oddp (the index)))))
  
  :objects
  ((bricks :type 'brick
	   :display-controls (list :color (if (the odd-row?) :red :blue))
	   :sequence (:size (if (the odd-row?) (1+ (the number-of-bricks)) (the number-of-bricks)))
	   :center (translate (if (the-child first?) (the center) (the-child previous center))
			      :right (if (the-child first?) (half (the-child width))
					 (+ (half (the-child previous width)) (half (the-child width))
					    (if (the odd-row?) (the spacing)
						(+ (the spacing) (the extra-even-space))))))
	   :width (if (and (the odd-row?) (or (the-child first?) (the-child last?)))
		      (half (the brick-width))
		      (the brick-width)))))

(define-object brick (box)

  :input-slots
  ((length 10)
   (width 20)
   (height 15)
   (display-controls (list :color :blue))))


(define-object brick-wall-display (gwl:base-ajax-graphics-sheet)

  :computed-slots 
  ((use-x3dom? t)
   (use-jquery? t)

   (main-sheet-body 
    (gwl:with-cl-who-string (:indent t)
      (:p (:h1 "Sample Display of a Brick Wall"))
      (:p ((:|X3D| :id "brick_sample"
	     :|width| 500 :|height| 500)
	   (:Scene
	    (with-format (x3d *stream*) 
	      (write-the wall-drawing cad-output))))))))
   
   :objects
   ((wall-drawing :type 'gwl:web-drawing
		  :object-roots (list (the wall)))

    (wall :type 'brick-wall)))


(gwl:publish-gwl-app "/brick-wall-display" 'wall-display)