(in-package :www.genworks.com)


(define-object configurator (base-site-sheet)
  
  :computed-slots
  ((link-title "Configurator")
   (right-section-inner-html (with-cl-who-string ()
			       (:h1 "Configure Your Weapon:  $" (fmt "~$" (the current-price)))
			       (str (the current-choice-screen inner-html))))

   
   ;;
   ;; FLAG -- figure out how to make this whole thing into an object tree instead of this hairy-calc.
   ;; 
   (current-price
    (+ (the gendl-license current-price)
       (the cl-engine current-price)
       (the geometry-kernel current-price)
       (the support-level current-price)
       (the training-level current-price)))
   
   #+nil
   (current-price (+ (ecase (the selected-gendl-license)
		       (:agpl 0)
		       (:trial 0)
		       (:student 49) ;; possible to add smlib and ??
		       (:professional 1500)
		       (:enterprise 3000))

		     (ecase (the selected-cl-engine)
		       (:none 0)
		       (:acl-32 (ecase (the selected-gendl-license)
				  (:agpl 4500) ;; Could maybe also choose Pro Allegro??
				  (:trial 0)
				  (:student 100)
				  (:professional 3000)
				  (:enterprise 4500)))
		       (:acl-64 (ecase (the selected-gendl-license)
				  (:agpl 5500) ;; Could maybe also choose Pro Allegro??
				  (:trial 0)
				  (:student 150)
				  (:professional 4200)
				  (:enterprise 5500)))
		       (:lw-32 1700)
		       (:lw-64 5100))
		     
		     (ecase (the selected-geometry-kernel)
		       (:basic 0)
		       (:smlib 4800))

		     (let ((base-price
			    (ecase (the selected-support-level)
			      (:none 0)
			      (:production (ecase (the selected-geometry-kernel)
					     (:smlib 14200)
					     (:basic 10000)))
			      (:development (ecase (the selected-geometry-kernel)
					      (:smlib 122000)
					      (:basic 8000)))
			      (:how-to (ecase (the selected-geometry-kernel)
					 (:smlib 9200)
					 (:basic 5000)))
			      (:install 750 (ecase (the selected-geometry-kernel)
					      (:smlib 1950)
					      (:basic 750))))))
		       (+ base-price 
			  (* base-price (ecase (the selected-cl-engine)
					  (:none 3/2)
					  ((:acl-32 :acl-64 :lw-32 :lw-64) 0)))))

		     (ecase (the selected-training-level)
		       (:none 0)
		       (:remote-3-day 2400)
		       (:onsite-3-day 4800)
		       (:onsite-10-day 25000))))

   
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
			 :next-sheet (the cl-engine))

   (cl-engine :type 'cl-engine-choice
	      :pass-down (selected-gendl-license)
	      :previous-sheet (the gendl-license)
	      :next-sheet (the geometry-kernel))


   (geometry-kernel :type 'geometry-kernel-choice
		    :pass-down (selected-gendl-license
				selected-cl-engine)
		    :previous-sheet (the cl-engine)
		    :next-sheet (the support-level))

   (support-level :type 'support-level-choice
		  :pass-down (selected-gendl-license
			      selected-cl-engine
			      selected-geometry-kernel)
		  :previous-sheet (the geometry-kernel)
		  :next-sheet (the training-level))

   
   (training-level :type 'training-level-choice
		   :pass-down (selected-gendl-license
			       selected-cl-engine
			       selected-geometry-kernel
			       selected-support-level)
		   :previous-sheet (the support-level))))



(define-object wizard-screen (sheet-section)
  :input-slots ((previous-sheet nil) (next-sheet nil) set-current-sheet-function)


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

		   (current-price (getf (the current-prices) (the choice value))))

  :objects ((previous-button :type 'button-form-control
			     :label "&lt;-Previous"  
			     :onclick (the (gdl-ajax-call :function-key :set-current-sheet
							  :arguments (list (the previous-sheet)))))
	    (next-button :type 'button-form-control
			 :label "Next-&gt;"
			 :onclick (the (gdl-ajax-call :function-key :set-current-sheet
						      :arguments (list (the next-sheet))))))

  :functions
  ((set-current-sheet
    (sheet)
    (funcall (the set-current-sheet-function) sheet))

   (choice-price-string 
    (string key)
    (let ((selected? (eql key (the choice value)))
	  (delta (- (getf (the current-prices) key)
		    (the current-price))))
      (format nil "~a ~a" string (if selected? "" (format nil "(~a$~$)" 
							  (cond ((zerop delta) "+")
								((plusp delta) "+")
								((minusp delta) "-"))
							  (abs delta))))))))


(define-object gendl-license-choice (wizard-screen)

  :computed-slots ((heading "Gendl Licensing Level")
		   
		   (current-prices (list :agpl 0
					 :trial 0
					 :student 49
					 :professional 1500
					 :enterprise 3000))

		   )

  :objects
  ((choice :type 'radio-form-control
	   :choice-plist (list :agpl (the (choice-price-string "Open Source (AGPL)" :agpl))
			       :trial (the (choice-price-string "Evaluation" :trial))
			       :student (the (choice-price-string "Student" :student))
			       :professional (the (choice-price-string "Professional" :professional))
			       :enterprise (the (choice-price-string "Enterprise" :enterprise)))
	   :default :agpl
	   :prompt ""
	   :ajax-submit-on-change? t)))



(define-object cl-engine-choice (wizard-screen)
  :input-slots (selected-gendl-license)

  :computed-slots ((heading "Common Lisp Engine")

		   (current-prices (list :none 0
					 :acl-32 (ecase (the selected-gendl-license)
						   (:agpl 4500) ;; Could maybe also choose Pro Allegro??
						   (:trial 0)
						   (:student 100)
						   (:professional 3000)
						   (:enterprise 4500))
					 :acl-64 (ecase (the selected-gendl-license)
						   (:agpl 5500) ;; Could maybe also choose Pro Allegro??
						   (:trial 0)
						   (:student 150)
						   (:professional 4200)
						   (:enterprise 5500))
					 :lw-32 1700
					 :lw-64 5100)))

  
  :objects
  ((choice :type 'radio-form-control
	   :choice-plist (list :none (the (choice-price-string "None (i.e. self-provided)" :none))
			       :acl-32 (the (choice-price-string "Franz Allegro CL&reg; 32-bit" :acl-32))
			       :acl-64 (the (choice-price-string "Franz Allegro CL&reg; 64-bit" :acl-64))
			       :lw-32 (the (choice-price-string "LispWorks 32-bit" :lw-32))
			       :lw-64 (the (choice-price-string "LispWorks 64-bit" :lw-64)))
	   :default :none
	   :prompt ""
	   :ajax-submit-on-change? t)))



(define-object geometry-kernel-choice (wizard-screen)
  :input-slots (selected-gendl-license selected-cl-engine)

  :computed-slots ((heading "Geometry Kernel")

		   (current-prices (list :basic 0
					 :smlib (if (eql (the selected-gendl-license) :student)
						    220 4800))))

  :objects
  ((choice :type 'radio-form-control 
	   :choice-plist (list :basic (the (choice-price-string "Basic" :basic))
			       :smlib (the (choice-price-string "SMLib&#8482;" :smlib)))
	   :default :basic
	   :prompt ""
	   :ajax-submit-on-change? t)))



(define-object support-level-choice (wizard-screen)
  :input-slots (selected-gendl-license selected-cl-engine selected-geometry-kernel)
  
  :computed-slots ((heading "Technical Support Level")

		   (current-prices 
		    (let ((surcharge-function (ecase (the selected-cl-engine)
						(:none #'(lambda(num) (* num 5/2)))
						((:acl-32 :acl-64) #'identity)
						((:lw-32 :lw-64) #'(lambda(num) (+ 3600 num))))))

				       
		      (list :none 0
			    :production (ecase (the selected-geometry-kernel)
					  (:smlib (funcall surcharge-function 14200))
					  (:basic (funcall surcharge-function 10000)))
			    :development (ecase (the selected-geometry-kernel)
					   (:smlib (funcall surcharge-function 12200))
					   (:basic (funcall surcharge-function 8000)))
			    :how-to (ecase (the selected-geometry-kernel)
				      (:smlib (funcall surcharge-function 9200))
				      (:basic (funcall surcharge-function 5000)))
			    :install (ecase (the selected-geometry-kernel)
				       (:smlib (funcall surcharge-function 1950))
				       (:basic (funcall surcharge-function 750)))))))


  :objects ((choice :type 'radio-form-control
		    :prompt ""
		    :default :none
		    :ajax-submit-on-change? t
		    :choice-plist 
		    (list :none (the (choice-price-string "None (i.e. self-provided or third-party)" :none))
			  :install (the (choice-price-string "Installation and Configuration" :install))
			  :how-to (the (choice-price-string "Technical how-to questions/answers" :how-to))
			  :development (the (choice-price-string "Application Code Nondisclosure" :development))
			  :production (the (choice-price-string "Mission-critical Production Environment" :production))))))


(define-object training-level-choice (wizard-screen)
  :input-slots (selected-gendl-license selected-cl-engine 
				       selected-geometry-kernel
				       selected-support-level)
  
  :computed-slots ((heading "Training Level")

		   
		   (current-prices (let ((fraction (if (eql (the selected-gendl-license) :student) 1/3 1)))
				     (list :none 0
					   :remote-3-day (* fraction 2400)
					   :onsite-3-day (* fraction 4800)
					   :onsite-10-day (* fraction 24000)))))


  :objects ((choice 
	     :type 'radio-form-control
	     :prompt ""
	     :default :none
	     :ajax-submit-on-change? t
	     :choice-plist 
	     (list :none (the (choice-price-string "(i.e. self-guided, online videos &amp; tutorials, or third-party" :none))
		   :remote-3-day (the (choice-price-string "Remote Three-day" :remote-3-day))
		   :onsite-3-day (the (choice-price-string "Onsite Three-day (excl. travel and expenses)" :onsite-3-day))
		   :onsite-10-day (the (choice-price-string "Onsite Ten-day (excl. travel and expenses)" :onsite-10-day))))))




				     
