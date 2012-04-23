(in-package :www.genworks.com)

(define-object gendl-license-choice (wizard-screen)

  :computed-slots ((heading "Gendl Licensing Level")
		   
		   (default (the agpl)))

  :objects
  ((agpl :type 'radio-price-choice
	 :string "Open Source (AGPL)"
	 :disabled? (eql (the selected-geometry-kernel) :smlib)
	 :disabled-message "Please Select \"Basic\" Geometry Kernel to enable this option."
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
	 :current-price 0
	 :disabled? (eql (the selected-geometry-kernel) :smlib)
	 :disabled-message "Please Select \"Basic\" Geometry Kernel to enable this option.")

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
					 ((:lw-32 :lw-64) #'(lambda(num) (+ 3600 num)))))

		   
		   (money-saving-tip (when (eql (the selected-cl-engine) :none)
				       (with-cl-who-string ()
					 "Select a commercial "
					 ((:span :class :clickme
						 :onclick (the (gdl-ajax-call :function-key :set-current-sheet
									      :arguments (list (the cl-engine)))))
					  "Common Lisp engine")
					 " to reduce Technical Support prices."))))
  

  :objects ((none :type 'radio-price-choice
		  :string "None (i.e. self-provided or third-party)"
		  :current-price 0)
	    
	    (install :type 'radio-price-choice
		     :string "Installation and Configuration"
		     :current-price (ecase (the selected-geometry-kernel)
				      (:smlib (funcall (the surcharge-function) 1950))
				      (:basic (funcall (the surcharge-function) 750))))

	    (how-to :type 'radio-price-choice
		    :string "Technical how-to questions/answers"
		    :current-price (ecase (the selected-geometry-kernel)
				     (:smlib (funcall (the surcharge-function) 9200))
				     (:basic (funcall (the surcharge-function) 5000))))
	    
	    (development :type 'radio-price-choice
			 :string "Application Code Nondisclosure"
			 :current-price (ecase (the selected-geometry-kernel)
					  (:smlib (funcall (the surcharge-function) 12200))
					  (:basic (funcall (the surcharge-function) 8000))))

	    (production :type 'radio-price-choice
			:string "Mission-critical Production Environment"
			:current-price (ecase (the selected-geometry-kernel)
					 (:smlib (funcall (the surcharge-function) 14200))
					 (:basic (funcall (the surcharge-function) 10000))))))




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




				     
