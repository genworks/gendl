(in-package :gdl-user)

;;
;; Submitted by Hermen de Jong of KE-works
;;

(define-object huge-boxes-sequence (base-object)

  :input-slots
  ((number-of-boxes 100 :settable))

  :computed-slots
  ((wall-width (floor (sqrt (the number-of-boxes)))))

  :objects
  ((boxes :type 'box
	  :sequence (:size (the number-of-boxes))
	  :height 10 
	  :width 20
	  :length 30
	  :display-controls (list :color :red :transparency .5)
	  :center (translate (make-point 0 0 0)
			     :right (* (rem (the-child index)
					    (the wall-width))
				       (the-child width))
			     :top (* (floor (the-child index)
					    (the wall-width))
				     (the-child height))))))