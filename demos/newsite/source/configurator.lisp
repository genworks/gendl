(in-package :www.genworks.com)

(defun info-anchor (string)
  (with-cl-who-string ()
    ((:a :href (format nil "#~a" string))
     ((:img :src "/newsite-static/images/information.png" :width 12)))))

(define-object gendl-license-choice (wizard-screen)

  :computed-slots ((heading (locale-string :gendl-licensing-level))
		   
		   (default (the agpl))

		   (extra-controls-html (with-cl-who-string ()
					    (str (the academic-input? html-string)))))
		   
  :objects

  ((agpl :type 'radio-price-choice
	 :string (string-append (locale-string :open-source-agpl)
				(info-anchor "agpl-license"))
	 :disabled? (eql (the selected-geometry-kernel) :smlib)
	 :disabled-message (locale-string :please-select-basic-geom)
	 :current-price 0)

   (trial :type 'radio-price-choice
	  :string (string-append (locale-string :evaluation)
				 (info-anchor "trial-license"))
	  :current-price 0)

   (student :type 'radio-price-choice
	    :string (string-append (locale-string :student)
				   (info-anchor "student-license"))
	    :current-price 49)

   (professional :type 'radio-price-choice
		 :string (string-append (locale-string :professional) 
					(info-anchor "professional-license"))
		 :current-price (funcall (the discount-func) 1990))
   
   (enterprise :type 'radio-price-choice
	       :string (string-append (locale-string :enterprise)
				      (info-anchor "enterprise-license"))
	       :current-price (funcall (the discount-func) 4990))


   (academic-input? :type 'checkbox-form-control
		    :ajax-submit-on-change? t
		    :default (eql (the selected-gendl-license) :student)
		    :label-position :table-td-append
		    :prompt (string-append (locale-string :order-qualifies-for-academic)
					   (info-anchor "academic-pricing")))
   
   (explanation :type 'license-choice-explanation)))



(define-object cl-engine-choice (wizard-screen)
  
  :computed-slots ((heading (locale-string :common-lisp-engine)) 

		   (default (case (the selected-gendl-license)
			      (:agpl (the none))
			      (otherwise (the acl-32))))

		   (lw-uplift 1)
		   (acl-uplift 3/2)

		   (discount-func #'(lambda(price key)
				      (ecase key 
					(:none price)
					(:sbcl price)
					(:acl-32 (* (the acl-uplift) price))
					(:acl-64 (* (the acl-uplift) price))
					(:lw-32-base (* (the lw-uplift) price))
					(:lw-32-full (* (the lw-uplift) price))
					(:lw-64-full (* (the lw-uplift) price))))))
  
  :objects
  ((none :type 'radio-price-choice
	 :string (string-append (locale-string :none) " (" 
				(locale-string :i-e) " "
				(locale-string :self-provided-or-third-party) ")")
	 :current-price (funcall (the discount-func) 0 (first (the-child root-path)))
	 :disabled? (or (eql (the selected-geometry-kernel) :smlib)
			(eql (the selected-gendl-license) :trial))
	 :disabled-message (format nil "~a ~a~a~a ~a."
				   (locale-string :please-select)
				   (if (eql (the selected-geometry-kernel) :smlib)
				       (locale-string :the-basic-geometry-kernel) "")
				   (cond ((and (eql (the selected-geometry-kernel) :smlib)
					       (eql (the selected-gendl-license) :trial))
					  (string-append " " (locale-string :and) " "))
					 (t " "))
				   (if (eql (the selected-gendl-license) :trial)
				       (locale-string :a-non-evaluation-gendl-license) "")
				   (locale-string :to-enable-this-option )))

   
   (sbcl :type 'radio-price-choice
	 :string "SBCL"
	 :current-price (funcall (the discount-func) 0 (first (the-child root-path)))
	 :disabled? (or (eql (the selected-geometry-kernel) :smlib)
			(eql (the selected-gendl-license) :trial))
	 :disabled-message (the none disabled-message))
   

   (acl-32 :type 'radio-price-choice
	   :string (locale-string :franz-allegro-cl-32)
	   :current-price 
	   (funcall (the discount-func)
		    (ecase (the selected-gendl-license)
		      (:agpl 3000)
		      (:trial 0)
		      (:student 100)
		      (:professional 1500)
		      (:enterprise 3000))
		    (first (the-child root-path))))

   (acl-64 :type 'radio-price-choice
	   :string (locale-string :franz-allegro-cl-64)
	   :disabled? (eql (the selected-gendl-license) :trial)
	   :current-price 
	   (funcall (the discount-func) 
		    (ecase (the selected-gendl-license)
		      (:agpl 5500)
		      (:trial 0)
		      (:student 150)
		      (:professional 4200)
		      (:enterprise 5500))
		    (first (the-child root-path))))

   (lw-32-base :type 'radio-price-choice
	       :string (locale-string :lispworks-32-basic)
	       :disabled? (eql (the selected-gendl-license) :trial)
	       :current-price (funcall (the discount-func) (if (the academic?) 1100 1700) (first (the-child root-path))))

   (lw-32-full :type 'radio-price-choice
	       :string (locale-string :lispworks-32-expanded)
	       :disabled? (eql (the selected-gendl-license) :trial)
	       :current-price (funcall (the discount-func) 
				       (if (the academic?) 3300 5100)
				       (first (the-child root-path))))

   (lw-64-full :type 'radio-price-choice
	       :string (locale-string :lispworks-64-expanded) 
	       :disabled? (eql (the selected-gendl-license) :trial)
	       :current-price (funcall (the discount-func) 
				       (if (the academic?) 3300 5100) 
				       (first (the-child root-path))))


   (explanation :type 'cl-engine-explanation)))




(define-object geometry-kernel-choice (wizard-screen)

  :computed-slots ((heading (locale-string :geometry-kernel)) 
		   (default (the basic)))


  :objects
  ((basic :type 'radio-price-choice
	  :string (locale-string :basic) 
	  :current-price (funcall (the discount-func) 0))

   (smlib :type 'radio-price-choice
	  :string "SMLib&reg;"
	  :disabled? (or (member (the selected-gendl-license) '(:agpl :trial))
			 (eql (the selected-cl-engine) :none))

	  :disabled-message 
	  (string-append 
	   (locale-string :please-select) " "
	   (cond ((and (eql (the selected-gendl-license) :agpl)
		       (eql (the selected-cl-engine) :none))
		  (string-append (locale-string :non-agpl-gendl)
				 " "
				 (locale-string :and)
				 " "
				 (locale-string :commercial-cl-engine)))
		 ((and (eql (the selected-gendl-license) :trial)
		       (eql (the selected-cl-engine) :none))
		  (string-append (locale-string :a-non-evaluation-gendl-license)
				 " "
				 (locale-string :and)
				 " "
				 (locale-string :commercial-cl-engine)))
		 ((eql (the selected-gendl-license) :agpl)
		  (locale-string :non-agpl-gendl))
		 ((eql (the selected-gendl-license) :trial)
		  (locale-string :a-non-evaluation-gendl-license))
		 ((eql (the selected-cl-engine) :none)
		  (locale-string :commercial-cl-engine)))
	   " " (locale-string :to-enable-this-option) ".")

	  :current-price (if (eql (the selected-gendl-license) :student)
			     220 (funcall (the discount-func) 6900)))))
	  

(define-object support-level-choice (wizard-screen)

  :computed-slots ((heading (locale-string :technical-support-level))
		   
		   (default (the none))
		   
		   (surcharge-function (ecase (the selected-cl-engine)
					 (:none #'(lambda(num) (* num 5/2)))
					 ((:acl-32 :acl-64) #'identity)
					 ((:lw-32-base :lw-32-full :lw-64-full) #'(lambda(num) (+ 3600 num)))))

		   
		   (money-saving-tip (when (eql (the selected-cl-engine) :none)
				       (with-cl-who-string ()
					 (str (locale-string :select))
					 " "
					 (str (locale-string :a))
					 " "
					 (str (locale-string :commercial))
					 " "
					 ((:span :class :clickme
						 :onclick (the (gdl-ajax-call :function-key :set-current-sheet
									      :arguments (list (the cl-engine)))))
					  (str (locale-string :common-lisp-engine)))
					 (str (locale-string :to-reduce-tech-support-prices))))))
  

  :objects ((none :type 'radio-price-choice
		  :string (string-append (locale-string :none)
					 " ("
					 (locale-string :i-e)
					 " " 
					 (locale-string :self-provided-or-third-party))
		  :current-price (funcall (the discount-func) 0))
	    
	    (install :type 'radio-price-choice
		     :string (locale-string :installation-and-configuration)
		     :disabled? (member (the selected-gendl-license) '(:trial :student))
		     :disabled-message (when (the-child disabled?)
					 (let ((trial? (eql (the selected-gendl-license) :trial))
					       (student? (eql (the selected-gendl-license) :student)))
					   (format nil "~a a ~a ~a ~a."
						   (locale-string :please-select)
						   (cond (trial? (locale-string :non-trial))
							 (student? (locale-string :non-student)))
						   (locale-string :gendl-license)
						   (locale-string :to-enable-this-option)
						   )))
						   
		     :current-price (funcall (the discount-func)
					     (ecase (the selected-geometry-kernel)
					       (:smlib (funcall (the surcharge-function) 1250))
					       (:basic (funcall (the surcharge-function) 550)))))

	    (how-to :type 'radio-price-choice
		    :string (locale-string :technical-how-to-q-a)
		    :disabled? (member (the selected-gendl-license) '(:trial :student))
		    :disabled-message (when (the-child disabled?)
					 (let ((trial? (eql (the selected-gendl-license) :trial))
					       (student? (eql (the selected-gendl-license) :student)))
					   (format nil "~a a ~a ~a ~a."
						   (locale-string :please-select)
						   (cond (trial? (locale-string :non-trial))
							 (student? (locale-string :non-student)))
						   (locale-string :gendl-license)
						   (locale-string :to-enable-this-option))))

		    :current-price (funcall (the surcharge-function)
					    (funcall (the discount-func)
						     (ecase (the selected-geometry-kernel)
						       (:smlib 9200)
						       (:basic 5000)))))
	    
	    (development :type 'radio-price-choice
			 :string (locale-string :application-code-nda) 
			 :disabled? (member (the selected-gendl-license) '(:trial :student))
			 :disabled-message (when (the-child disabled?)
					     (let ((trial? (eql (the selected-gendl-license) :trial))
						   (student? (eql (the selected-gendl-license) :student)))
					       (format nil "~a a ~a ~a ~a."
						       (locale-string :please-select)
						       (cond (trial? (locale-string :non-trial))
							     (student? (locale-string :non-student)))
						       (locale-string :gendl-license)
						       (locale-string :to-enable-this-option))))
			 
			 :current-price (funcall (the surcharge-function)
						 (funcall (the discount-func)
							  (ecase (the selected-geometry-kernel)
							    (:smlib 14200)
							    (:basic 9000)))))

	    (production :type 'radio-price-choice
			:string (locale-string :mission-critical-prod-env)
			:disabled? (not (or (eql (the selected-gendl-license) :enterprise)
					    (eql (the selected-gendl-license) :agpl)))

			:disabled-message (format nil "~a ~a ~a ~a ~a ~a."
						  (locale-string :please-select)
						  (locale-string :an-open-source)
						  (locale-string :or)
						  (locale-string :enterprise-class)
						  (locale-string :gendl-license)
						  (locale-string :to-enable-this-option))
						  

			:current-price (funcall (the surcharge-function)
						(funcall (the discount-func)
							 (ecase (the selected-geometry-kernel)
							   (:smlib 17200)
							   (:basic 12000)))))))




(define-object training-level-choice (wizard-screen)
  
  :computed-slots ((heading (locale-string :training-level))
		   (fraction (if (eql (the selected-gendl-license) :student) 1/3 1))
		   (default (the none)))


  :objects ((none :type 'radio-price-choice
		  :string (string-append "("
					 (locale-string :i-e)
					 (locale-string :self-guided-etc)
					 ")")
		  :current-price (funcall (the discount-func) 0))
	    
	    (remote-3-day :type 'radio-price-choice
			  :string (string-append (locale-string :remote)
						 " "
						 (locale-string :three-day))
			  :current-price (funcall (the discount-func) (* (the fraction) 2400)))

	    (onsite-3-day :type 'radio-price-choice
			  :string (string-append (locale-string :onsite)
						 " "
						 (locale-string :three-day)
						 " ("
						 (locale-string :excl-travel-and-exp)
						 ")")
			  :current-price (funcall (the discount-func) (* (the fraction) 4800)))

	    (onsite-10-day :type 'radio-price-choice
			   :string (string-append (locale-string :onsite)
						  " "
						  (locale-string :ten-day)
						  " ("
						  (locale-string :excl-travel-and-exp)
						  ")")
			   :current-price (funcall (the discount-func) (* (the fraction) 18000)))))




				     
