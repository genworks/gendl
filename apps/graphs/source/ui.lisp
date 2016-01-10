(in-package :graph-plot)

(setq *developing?* t)

(define-object ui (base-ajax-sheet)

  :computed-slots
  ((use-raphael? t)
   (use-x3dom? t)

   (function-specs (list (list :function-body
			       (the (function-bodies 0) value)
			       :domain-min -5 :domain-max 5)))


   (number-of-functions 1)

   #+nil
   (html-sections (list (the inputs-section)
			 (the results-section)
			 (the viewport)))
   
   (main-sheet-body 
    (with-cl-who-string ()
      (:p (when *developing?* (str (the development-links))))
      (:p (str (the inputs-section main-div)))
      (:p (str (the results-section main-div)))
      (:p (str (the viewport main-div)))))

   (range-min (apply #'min (mapsend (the graphs) :range-min)))
   (range-max (apply #'max (mapsend (the graphs) :range-max)))

   (domain-min (apply #'min (mapsend (the graphs) :domain-min)))
   (domain-max (apply #'max (mapsend (the graphs) :domain-max)))
   )

  
  :hidden-objects
  ((graphs :type 'assembly
	   :sequence (:size (length (the function-specs)))
	   :pseudo-inputs (specs)
	   :specs (nth (the-child index) (the function-specs))
	   :function (let ((form (getf (the-child specs) :function-body)))
		       `(lambda (gdl-user::x) ,(let ((*package* (find-package :graph-plot))) form)))
	   :parameters (remove-plist-key (the-child specs) :function-body))


   (function-bodies :type 'text-form-control
		    :sequence (:size (the number-of-functions))
		    :ajax-submit-on-change? t
		    :domain :pass-thru
		    :default '(* gdl-user::x 2)))


  :objects
  ((inputs-section :type 'sheet-section
		   :inner-html (with-cl-who-string ()
				 (dolist (function (list-elements (the function-bodies)))
				   (htm (str (the-object function html-string)) :br))))

   (results-section :type 'sheet-section
		    :inner-html (with-cl-who-string ()
				  (fmt "~s" (the (graphs 0) function))))

   (box :type 'box
	:width (twice (apply #'max (mapcar #'abs (list (the range-min) (the range-max)))))
	:length (twice (apply #'max (mapcar #'abs (list (the domain-min) (the domain-max))))))

   (viewport :type 'base-ajax-graphics-sheet
	     :respondent self
	     :view-direction-default :top
	     :image-format-default :raphael
	     :display-list-object-roots (list-elements (the graphs))
	     :display-list-objects (unless (eql (the-child image-format) :x3dom)
				     (list (the box)))
	     :length 500
	     :width 500)))


