(in-package :www.genworks.com)


(define-object configurator (base-site-sheet)
  
  :computed-slots
  ((link-title "Configurator")
   (right-section-inner-html (with-cl-who-string ()
			       (:h1 "Configure Your Weapon:  $" (fmt "~:d" (ceiling (the current-price))))
			       (str (the current-choice-screen inner-html))))

   
   ;;
   ;; FLAG -- figure out how to make this whole thing into an object tree instead of this hairy-calc.
   ;; 
   (current-price
    (+ (the gendl-license selected-price)
       (the cl-engine selected-price)
       (the geometry-kernel selected-price)
       (the support-level selected-price)
       (the training-level selected-price)))
   
   
   (current-choice-screen (the gendl-license) :settable)

   
   (selected-gendl-license (the gendl-license choice value))
   (selected-cl-engine (the cl-engine choice value))
   (selected-geometry-kernel (the geometry-kernel choice value))
   (selected-support-level (the support-level choice value))
   (selected-training-level (the training-level choice value))

   (set-current-sheet-function #'(lambda(sheet) 
				   (the (set-slot! :current-choice-screen sheet)))))

  :trickle-down-slots (respondent set-current-sheet-function)

  :objects
  ((gendl-license :type 'gendl-license-choice
		  :pass-down (selected-gendl-license 
			      selected-cl-engine selected-geometry-kernel selected-support-level
			      selected-training-level)
		  :next-sheet (the cl-engine))

   (cl-engine :type 'cl-engine-choice
	      :pass-down (selected-gendl-license 
			  selected-cl-engine selected-geometry-kernel selected-support-level
			  selected-training-level)
	      :previous-sheet (the gendl-license)
	      :next-sheet (the geometry-kernel))


   (geometry-kernel :type 'geometry-kernel-choice
		    :pass-down (selected-gendl-license 
				selected-cl-engine selected-geometry-kernel selected-support-level
				selected-training-level)
		    :previous-sheet (the cl-engine)
		    :next-sheet (the support-level))

   (support-level :type 'support-level-choice
		  :pass-down (selected-gendl-license 
			      selected-cl-engine selected-geometry-kernel selected-support-level
			      selected-training-level)
		  :previous-sheet (the geometry-kernel)
		  :next-sheet (the training-level))

   
   (training-level :type 'training-level-choice
		   :pass-down (selected-gendl-license 
			       selected-cl-engine selected-geometry-kernel selected-support-level
			       selected-training-level)
		   :previous-sheet (the support-level))))



(define-object wizard-screen (sheet-section)
  :input-slots ((previous-sheet nil) (next-sheet nil) set-current-sheet-function
		selected-gendl-license selected-cl-engine selected-geometry-kernel 
		selected-support-level selected-training-level)

  :computed-slots ((inner-html (with-cl-who-string ()
				 (:p
				  (:h2 (str (the heading)))
				  (:fieldset 
				   (str (the choice html-string))))
				 (:p 
				  (when (the previous-sheet)
				    (str (the previous-button form-control-string)))
				  (when (the next-sheet)
				    (str (the next-button form-control-string))))))

		   (selected-price (the selected-item current-price))

		   (radio-price-choices (remove-if-not #'(lambda(child) (typep child 'radio-price-choice))
								 (the children)))

		   (choice-plist
		    (mapcan #'(lambda(radio)
				(list (make-keyword (the-object radio strings-for-display))
				      (the-object radio choice-price-string)))
			    (the radio-price-choices)))

		   (selected-item (dolist (object (the radio-price-choices))
				    (when (eql (the-object object key) (the choice value))
				      (return object)))))
  

  :trickle-down-slots (selected-item selected-price)
  
  :objects ((previous-button :type 'button-form-control
			     :label "&lt;-Previous"  
			     :onclick (the (gdl-ajax-call :function-key :set-current-sheet
							  :arguments (list (the previous-sheet)))))
	    (next-button :type 'button-form-control
			 :label "Next-&gt;"
			 :onclick (the (gdl-ajax-call :function-key :set-current-sheet
						      :arguments (list (the next-sheet)))))

	    
	    (choice 
	     :type 'radio-form-control
	     :choice-plist (the choice-plist)
	     :prompt ""
	     :default (make-keyword (the default strings-for-display))
	     :disabled-keys nil
	     :disabled-keys 
	     (remove-if-not #'(lambda(key) (the (evaluate key) disabled?))
					   (plist-keys (the-child choice-plist)))
	     :ajax-submit-on-change? t
	     ))


  :functions
  ((set-current-sheet
    (sheet)
    (funcall (the set-current-sheet-function) sheet))))



(define-object radio-price-choice ()

  :input-slots (string current-price (disabled? nil) (disabled-message ""))

  :computed-slots ((key (make-keyword (the strings-for-display)))
		   
		   (selected? (eql (the selected-item) self))

		   (delta (- (the current-price) (the selected-price)))

		   (choice-price-string 
		    (if (the disabled?)
			(with-cl-who-string()
			  ((:span :class "disabled") (str (the string)))
			  " " 
			  (unless (string-equal (the disabled-message) "")
			    (htm (:i (str (the disabled-message))))))
			(format nil "~a ~a" (the string) 
				(if (the selected?) "" (format nil "(~a$~:d)" 
							       (cond ((zerop (the delta)) "+")
								     ((plusp (the delta)) "+")
								     ((minusp (the delta)) "-"))
							       (abs (ceiling (the delta))))))))))



(define-object gendl-license-choice (wizard-screen)

  :computed-slots ((heading "Gendl Licensing Level")
		   
		   (default (the agpl)))

  :objects
  ((agpl :type 'radio-price-choice
	 :string "Open Source (AGPL)"
	 :disabled? (eql (the selected-geometry-kernel) :smlib)
	 :disabled-message "Select \"Basic\" Geometry Kernel to enable this option."
	 :current-price 0)

   (trial :type 'radio-price-choice
	  :string "Evaluation"
	  :current-price 0)

   (student :type 'radio-price-choice
	    :string "Student"
	    :current-price 49)

   (professional :type 'radio-price-choice
		 :string "Professional"
		 :current-price 1500)
   
   (enterprise :type 'radio-price-choice
	       :string "Enterprise"
	       :current-price 3000)))




(define-object cl-engine-choice (wizard-screen)

  :computed-slots ((heading "Common Lisp Engine")
		   (default (the none)))
  
  :objects
  ((none :type 'radio-price-choice
	 :string "None (i.e. self-provided)"
	 :current-price 0)

   (acl-32 :type 'radio-price-choice
	   :string "Franz Allegro CL&reg; 32-bit"
	   :current-price 
	   (ecase (the selected-gendl-license)
	     (:agpl 4500) ;; Could maybe also choose Pro Allegro??
	     (:trial 0)
	     (:student 100)
	     (:professional 3000)
	     (:enterprise 4500)))

   (acl-64 :type 'radio-price-choice
	   :string "Franz Allegro CL&reg; 64-bit"
	   :current-price 
	   (ecase (the selected-gendl-license)
	     (:agpl 5500) ;; Could maybe also choose Pro Allegro??
	     (:trial 0)
	     (:student 150)
	     (:professional 4200)
	     (:enterprise 5500)))

   (lw-32 :type 'radio-price-choice
	  :string "LispWorks 32-bit"
	  :current-price 1700)

   (lw-64 :type 'radio-price-choice
	  :string "LispWorks 64-bit"
	  :current-price 5100)))




(define-object geometry-kernel-choice (wizard-screen)

  :computed-slots ((heading "Geometry Kernel")
		   (default (the basic)))

  :objects
  ((basic :type 'radio-price-choice
	  :string "Basic"
	  :current-price 0)

   (smlib :type 'radio-price-choice
	  :string "SMLib&reg;"
	  :disabled? (or (eql (the selected-gendl-license) :agpl)
			 (eql (the selected-cl-engine) :none))
	  :disabled-message (cond ((and (eql (the selected-gendl-license) :agpl)
					(eql (the selected-cl-engine) :none))
				   "Please select a non-AGPL Gendl license and commercial CL engine to enable this option.")
				  ((eql (the selected-gendl-license) :agpl)
				   "Please select a non-AGPL Gendl license to enable this option.")
				  ((eql (the selected-cl-engine) :none)
				   "Please select a commercial CL engine to enable this option."))
	  :current-price (if (eql (the selected-gendl-license) :student)
			     220 4800))))
	  

(define-object support-level-choice (wizard-screen)

  :computed-slots ((heading "Technical Support Level")
		   
		   (default (the none))

		   (surcharge-function (ecase (the selected-cl-engine)
					 (:none #'(lambda(num) (* num 5/2)))
					 ((:acl-32 :acl-64) #'identity)
					 ((:lw-32 :lw-64) #'(lambda(num) (+ 3600 num))))))
  


  :objects ((none :type 'radio-price-choice
		  :string "None (i.e. self-provided or third-party)"
		  :current-price 0)

	    (production :type 'radio-price-choice
			:string "Mission-critical Production Environment"
			:current-price (ecase (the selected-geometry-kernel)
					 (:smlib (funcall (the surcharge-function) 14200))
					 (:basic (funcall (the surcharge-function) 10000))))
	  
	    (development :type 'radio-price-choice
			 :string "Application Code Nondisclosure"
			 :current-price (ecase (the selected-geometry-kernel)
					  (:smlib (funcall (the surcharge-function) 12200))
					  (:basic (funcall (the surcharge-function) 8000))))
	    (how-to :type 'radio-price-choice
		    :string "Technical how-to questions/answers"
		    :current-price (ecase (the selected-geometry-kernel)
				     (:smlib (funcall (the surcharge-function) 9200))
				     (:basic (funcall (the surcharge-function) 5000))))

	    (install :type 'radio-price-choice
		     :string "Installation and Configuration"
		     :current-price (ecase (the selected-geometry-kernel)
				      (:smlib (funcall (the surcharge-function) 1950))
				      (:basic (funcall (the surcharge-function) 750))))))



(define-object training-level-choice (wizard-screen)
  
  :computed-slots ((heading "Training Level")
		   (fraction (if (eql (the selected-gendl-license) :student) 1/3 1))
		   (default (the none)))


  :objects ((none :type 'radio-price-choice
		  :string "(i.e. self-guided, online videos &amp; tutorials, or third-party)"
		  :current-price 0)
	    
	    (remote-3-day :type 'radio-price-choice
			  :string "Remote Three-day"
			  :current-price (* (the fraction) 2400))

	    (onsite-3-day :type 'radio-price-choice
			  :string "Onsite Three-day (excl. travel and expenses)"
			  :current-price (* (the fraction) 4800))

	    (onsite-10-day :type 'radio-price-choice
			   :string "Onsite Ten-day (excl. travel and expenses)"
			   :current-price (* (the fraction) 18000))))




				     
