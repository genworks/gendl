(in-package :www.genworks.com)


(define-object configurator (base-site-sheet)
  
  :computed-slots
  ((link-title "Configurator")
   (right-section-inner-html (with-cl-who-string ()
			       ((:a :name "Top"))
			       (:h1 "Configure Your Weapon:  $" (fmt "~:d" (ceiling (the current-price))))
			       (str (the current-choice-screen inner-html))
			       
			       (str (the summary inner-html))

			       (when (the current-choice-screen explanation-html)
				 (htm (:p 
				       (str (the current-choice-screen explanation-html)))))
			       
			       ))

   
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
   (academic? (the gendl-license academic-input? value))

   (set-current-sheet-function #'(lambda(sheet) 
				   (the (set-slot! :current-choice-screen sheet))))

   
   )


  :trickle-down-slots (respondent set-current-sheet-function)

  :objects
  ((gendl-license :type 'gendl-license-choice
		  :pass-down (selected-gendl-license 
			      selected-cl-engine selected-geometry-kernel selected-support-level
			      selected-training-level academic?)
		  :next-sheet (the cl-engine))

   (cl-engine :type 'cl-engine-choice
	      :pass-down (selected-gendl-license 
			  selected-cl-engine selected-geometry-kernel selected-support-level
			  selected-training-level academic?)
	      :previous-sheet (the gendl-license)
	      :next-sheet (the geometry-kernel))


   (geometry-kernel :type 'geometry-kernel-choice
		    :pass-down (selected-gendl-license 
				selected-cl-engine selected-geometry-kernel selected-support-level
				selected-training-level academic?)
		    :previous-sheet (the cl-engine)
		    :next-sheet (the support-level))

   (support-level :type 'support-level-choice
		  :pass-down (selected-gendl-license 
			      selected-cl-engine selected-geometry-kernel selected-support-level
			      selected-training-level academic?)
		  :previous-sheet (the geometry-kernel)
		  :next-sheet (the training-level))

   
   (training-level :type 'training-level-choice
		   :pass-down (selected-gendl-license 
			       selected-cl-engine selected-geometry-kernel selected-support-level
			       selected-training-level academic?)
		   :previous-sheet (the support-level))
   


   (summary :type 'quotation-summary
	    :selected-items (list (the gendl-license selected-item)
				  (the cl-engine selected-item)
				  (the geometry-kernel selected-item)
				  (the support-level selected-item)
				  (the training-level selected-item))
	    :current-total (the current-price))))



(define-object quotation-summary ()
  :input-slots (selected-items current-total)
  :computed-slots 
  ((inner-html (with-cl-who-string ()
		 ((:div :class "quotation-summary")
		  (:table 
		      (:tr (:th (str (locale-string :gendl-component)))
			   (:th (str (locale-string :your-selection)))
			   (:th (str (locale-string :price))))
		    (dolist (item (the selected-items))
		      (htm (:tr ((:td :class "clickme") (str (the-object item go-to-link)))
				(:td (str (the-object item string)))
				((:td :class "right-justify") 
				 (fmt "$~:d" (ceiling (the-object item selected-price)))))))
		    (:tr (:th :br) ((:th :class "right-justify") "Total") 
			 ((:td :class "look-at-me right-justify") (fmt "$~:d" (the current-total))))))))))


(define-object wizard-screen (sheet-section)
  :input-slots ((previous-sheet nil) (next-sheet nil) (extra-controls-html nil)
		(explanation-html nil)
		set-current-sheet-function
		selected-gendl-license selected-cl-engine selected-geometry-kernel 
		selected-support-level selected-training-level academic?
		(discount-func (if (the academic?)
				   #'(lambda(num) (half num))
				   #'identity)))

  :computed-slots ((inner-html (with-cl-who-string ()
				 (:p
				  (:h2 (str (the heading)))
				  (:fieldset 
				   (str (the choice html-string))
				   
				   (when (the extra-controls-html)
				     (str (the extra-controls-html)))))

				 (when (the money-saving-tip)
				   (htm ((:p :class "helpful-tip") 
					 (str (locale-string :tip)) ": " (str (the money-saving-tip)))))
				 
				 (:p 
				  (when (the previous-sheet)
				    (str (the previous-button form-control-string)))
				  (when (the next-sheet)
				    (str (the next-button form-control-string))))))
		   
		   (current-choice-object self)

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
				      (return object))))

		   (money-saving-tip nil)
		   )
  

  :trickle-down-slots (selected-item selected-price heading current-choice-object)
  
  :objects ((previous-button :type 'button-form-control
			     :label (format nil "&lt;-~a" (locale-string :previous))
			     :onclick (the (gdl-ajax-call :function-key :set-current-sheet
							  :arguments (list (the previous-sheet)))))
	    (next-button :type 'button-form-control
			 :label (format nil "~a-&gt;" (locale-string :next))
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



(define-object radio-price-choice (sheet-section)

  :input-slots (string current-price (disabled? nil) (disabled-message "") heading current-choice-object)

  :computed-slots ((key (make-keyword (the strings-for-display)))
		   
		   (selected? (eql (the selected-item) self))

		   (delta (- (the current-price) (the selected-price)))
		   
		   (go-to-link (let ((choice-object (the current-choice-object)))
				 (with-cl-who-string ()
				   ((:span :onclick (the (gdl-ajax-call 
							  :function-key :set-current-sheet
							  :arguments (list choice-object))))
				    (str (the heading))))))

		   (choice-price-string 
		    (if (the disabled?)
			(with-cl-who-string()
			  ((:span :class "disabled") (str (the string)))
			  " " 
			  (unless (string-equal (the disabled-message) "")
			    (htm :br "&nbsp;" (:i (str (the disabled-message))))))
			(format nil "~a ~a" (the string) 
				(if (the selected?) "" (format nil "(~a$~:d)" 
							       (cond ((zerop (the delta)) "+")
								     ((plusp (the delta)) "+")
								     ((minusp (the delta)) "-"))
							       (abs (ceiling (the delta)))))))))
  :functions ((set-current-sheet (object)
				 (funcall (the set-current-sheet-function) object))))



