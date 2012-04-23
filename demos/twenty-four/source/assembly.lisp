(in-package :www.genworks.com)

(define-object assembly-24 (base-site-sheet)

  :computed-slots
  ((title "HTML Game: Twenty-Four")
   (link-title  "Twenty-Four")
   
   (entering? t :settable)
   
   (a (the a-input value))
   (b (the b-input value))
   (c (the c-input value))
   (d (the d-input value))
   
   (valid-numbers? (every #'integerp (list (the a) (the b) (the c) (the d))))
   
   (answers (when (the valid-numbers?)
	      (twenty-four (the a) (the b) (the c) (the d))))

   (whole-plus-answers (getf (the answers) :whole-plus))
   (whole-minus-answers (getf (the answers) :whole-minus))
   (fractional-answers (getf (the answers) :fractions))

   (right-section-inner-html
    (with-cl-who-string ()
      (if (or (the entering?) (not (the valid-numbers?)))
	  (htm
	   (:h2 "Would you like to Play a Game?") 
	   ((:div :id "contact"))
	   (:p "Please provide four numbers between 1 and 10, inclusive:")
	   (:fieldset (:p (str (the a-input form-control-string))
			  (str (the b-input form-control-string))
			  (str (the c-input form-control-string))
			  (str (the d-input form-control-string))

			  ((:span :style "text-align: right;")
			   (str (the clear-button form-control-string)))))
			  
	   (:p (str (the go-button form-control-string))))

	  (htm 
	   (:h2 "Here is what I know:") 
	   ((:div :id "contact"))
	   (if (or (the whole-plus-answers)
		   (the whole-minus-answers)
		   (the fractional-answers))
	       (htm
		(:p (str (the try-again-button form-control-string)))
		(when (the whole-plus-answers)
		  (htm (:h3 (fmt "Here are the combinations for ~a, ~a, ~a, and ~a:" (the a) (the b) (the c) (the d)))
		       (:p (:table (dolist (answer (the whole-plus-answers))
				      (htm (:tr (:td (fmt "~s" answer)))))))))

		(when (the whole-minus-answers)
		  (htm (:h3 "Answers which involve negative numbers the computation:")
		       (:p (:table (dolist (answer (the whole-minus-answers))
				     (htm (:tr (:td (fmt "~s" answer)))))))))

		(when (the fractional-answers)
		  (htm (:h3 "Answers which involve fractions in the computation:")
		       (:p (:table (dolist (answer (the fractional-answers))
				     (htm (:tr (:td (fmt "~s" answer))))))))))

	       (htm ((:p :class "helpful-tip") 
		     (fmt "I could not find any combinations which give 24 for ~a, ~a, ~a, and ~a:" 
			  (the a) (the b) (the c) (the d)))))
	   (:p (str (the try-again-button form-control-string))))))))
  
  :functions
  ((reset-entering! ()
		    (the (set-slot! :entering? t)))

   (clear! () (dolist (input (list (the a-input)
				   (the b-input)
				   (the c-input)
				   (the d-input)))
		(the-object input restore-defaults!))))

  :objects
  ((a-input :type 'menu-form-control
	    :ajax-submit-on-change? t
	    :choice-list '(1 2 3 4 5 6 7 8 9 10)
	    :size 1
	    :domain :number
	    :default 1)
   
   (b-input :type 'menu-form-control
	    :ajax-submit-on-change? t
	    :choice-list '(1 2 3 4 5 6 7 8 9 10)
	    :size 1
	    :domain :number
	    :default 1)

   (c-input :type 'menu-form-control
	    :ajax-submit-on-change? t
	    :choice-list '(1 2 3 4 5 6 7 8 9 10)
	    :size 1
	    :domain :number
	    :default 1)

   (d-input :type 'menu-form-control
	    :ajax-submit-on-change? t
	    :choice-list '(1 2 3 4 5 6 7 8 9 10)
	    :size 1
	    :domain :number
	    :default 1)

   
   (go-button :type 'button-form-control
		:default "Go"
		:onclick (the (gdl-ajax-call :function-key :set-slot! :arguments (list :entering? nil)
					     #+nil
					     :form-controls 
					     #+nil
					     (list (the a-input)
						   (the b-input)
						   (the c-input)
						   (the d-input)))))
   (clear-button :type 'button-form-control
		 :default "Clear!"
		 :onclick (the (gdl-ajax-call :function-key :clear!)))

   (try-again-button :type 'button-form-control
		:default "Try Again"
		:onclick (the (gdl-ajax-call :function-key :reset-entering!)))))
					     
