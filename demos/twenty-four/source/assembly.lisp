(in-package :www.genworks.com)

(setf *random-state* (make-random-state t))

(define-object assembly-24 (base-site-sheet)

  :computed-slots
  ((title "HTML Game: Twenty-Four")
   (link-title  "Twenty-Four")
   
   
   (valid-numbers? (and (every #'integerp (list (the a) (the b) (the c) (the d)))
			(> (the max) (the min))
			(integerp (the target value))))
   
   (answers (when (the valid-numbers?)
	      (twenty-four (the a) (the b) (the c) (the d) :target (the target value))))

   (whole-plus-answers (getf (the answers) :whole-plus))
   (whole-minus-answers (getf (the answers) :whole-minus))
   (fractional-answers (getf (the answers) :fractions))

   (right-section-inner-html
    (with-cl-who-string ()
      (if (or (the entering?) (not (the valid-numbers?)))
	  (htm
	   (:h2 "Would you like to Play a Game?") 
	   ((:div :id "contact"))
	   (:p (fmt "Please provide four numbers between ~a and ~a, inclusive, to yield "
		    (the min) (the max))
	       (str (the target value)) ":")
	   (:fieldset 
	    (str (the a-input form-control-string))

	    (:p (:table 
			      (:tr (:td (str (the a-input form-control-string)))
				   (:td (str (the b-input form-control-string)))
				   (:td (str (the c-input form-control-string)))
				   (:td (str (the d-input form-control-string)))
				   (:td (str (the randomize-button form-control-string))))
			    
			    (:tr (:td ((:img :src (the a-card))))
				 (:td ((:img :src (the b-card))))
				 (:td ((:img :src (the c-card))))
				 (:td ((:img :src (the d-card))))
				 (:td (str (the go-button form-control-string)))))
			  (:i (:small "card images copyright information: http://www.jfitz.com/cards/"))))
	   
	   (:p "Preferences:")

	   ((:fieldset :style "text-justify: right;")
	    (:p (str (the min-input html-string))
		(str (the max-input html-string))
		(str (the target html-string)))))

	  (htm 
	   (:h2 (fmt "Here is what I know for ~a:" (the target value)))
	   ((:div :id "contact"))
	   (if (or (the whole-plus-answers)
		   (the whole-minus-answers)
		   (the fractional-answers))
	       (htm
		(:p (str (the try-again-button form-control-string)))

		
		(:p (:table 
			(:tr (:td ((:img :src (the a-card))))
			     (:td ((:img :src (the b-card))))
			     (:td ((:img :src (the c-card))))
			     (:td ((:img :src (the d-card))))))

		    (:i (:small "card images copyright information: http://www.jfitz.com/cards/")))

		(when (the whole-plus-answers)
		  (htm (:h3 (fmt "Here are the combinations for ~a, ~a, ~a, and ~a to yield ~a:" 
				 (the a) (the b) (the c) (the d) (the target value)))
		       (:p (:table (dolist (answer (the whole-plus-answers))
				      (htm (:tr (:td (fmt "~s = ~d" answer (the target value))))))))))

		(when (the whole-minus-answers)
		  (htm (:h3 "Answers which involve negative numbers the computation:")
		       (:p (:table (dolist (answer (the whole-minus-answers))
				     (htm (:tr (:td (fmt "~s = ~d" answer (the target value))))))))))

		(when (the fractional-answers)
		  (htm (:h3 "Answers which involve fractions in the computation:")
		       (:p (:table (dolist (answer (the fractional-answers))
				     (htm (:tr (:td (fmt "~s = ~d" answer (the target value)))))))))))

	       (htm ((:p :class "helpful-tip") 
		     (fmt "Welp, I could not find any combinations for ~a, ~a, ~a, and ~a which yield ~a..." 
			  (the a) (the b) (the c) (the d) (the target value)))))
	   (:p (str (the try-again-button form-control-string)))))))


   (entering? t :settable)
   
   (a (the a-input value))
   (b (the b-input value))
   (c (the c-input value))
   (d (the d-input value))

   (a-card (the (random-card-image-for-number (the a))))
   (b-card (the (random-card-image-for-number (the b))))
   (c-card (the (random-card-image-for-number (the c))))
   (d-card (the (random-card-image-for-number (the d))))
   
   (min (the min-input value))
   (max (the max-input value))
   (number-choices (list-of-numbers (the min) (the max)))
   (range (1+ (- (the max) (the min)))))

  
  :functions
  ((random-card-image-for-number
    (number)
    (if (<= number 13)
	(let ((cards (aref *cards-by-number*  (1- number))))
	  (format nil "/newsite-static/images/cards-classic/~a.png" (nth (random (length cards)) cards)))
	"/newsite-static/images/cards-classic/53.png"))
   
   (reset-entering! ()
		    (the (set-slot! :entering? t)))

   (clear! () (dolist (input (list (the a-input)
				   (the b-input)
				   (the c-input)
				   (the d-input)))
		(the-object input restore-defaults!)))

   
   (randomize! () (dolist (input (list (the a-input)
				       (the b-input)
				       (the c-input)
				       (the d-input)))
		    (the-object input (set-slot! :value (1+ (random (the range)))))))

   )

  :objects
  ((min-input :type 'menu-form-control
	      :ajax-submit-on-change? t
	      :choice-list '(1 2 3 4 5 6 7 8  10)
	      :default 1
	      :size 1
	      :prompt "Min")
   
   (max-input :type 'menu-form-control
	      :ajax-submit-on-change? t
	      :choice-list '(8 9 10 11 12 13 14 15 16 17 18 19 20)
	      :default 10
	      :size 1
	      :prompt "Max")

   (target :type 'number-form-control
	   :ajax-submit-on-change? t
	   :default 24
	   :prompt "Target Value: "
	   :size 5)


   (a-input :type 'menu-form-control
	    :ajax-submit-on-change? t
	    :choice-list (the number-choices)
	    :size 1
	    :domain :number
	    :default 1)
   
   (b-input :type 'menu-form-control
	    :ajax-submit-on-change? t
	    :choice-list (the number-choices)
	    :size 1
	    :domain :number
	    :default 1)

   (c-input :type 'menu-form-control
	    :ajax-submit-on-change? t
	    :choice-list (the number-choices)
	    :size 1
	    :domain :number
	    :default 1)

   (d-input :type 'menu-form-control
	    :ajax-submit-on-change? t
	    :choice-list (the number-choices)
	    :size 1
	    :domain :number
	    :default 1)

   
   (go-button :type 'button-form-control
		:default "Solve!"
		:onclick (the (gdl-ajax-call :function-key :set-slot! :arguments (list :entering? nil)
					     :form-controls 
					     (list (the target)))))

   (clear-button :type 'button-form-control
		 :default "Clear!"
		 :onclick (the (gdl-ajax-call :function-key :clear!)))

   (randomize-button :type 'button-form-control
		     :default "Randomize!"
		     :onclick (the (gdl-ajax-call :function-key :randomize!)))

   (try-again-button :type 'button-form-control
		:default "Try Again..."
		:onclick (the (gdl-ajax-call :function-key :reset-entering!)))))
					     
