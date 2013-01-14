(in-package :gwl-user)

(define-object ajax-json-test (base-ajax-sheet)

  :computed-slots
  ((use-jquery? t)
   
   (main-sheet-body (with-cl-who-string ()
		      (str (the Main-Section main-div)))))

  :objects
  ((Main-Section :type 'sheet-section
		 :inner-html (with-cl-who-string ()
			       (str (the development-links))
			       (str (the Application-Domain html-string))
			       :br
			       "Current value: " (str (the Application-Domain value))
			       :br
			       (str (the send form-control-string))))

   (Application-Domain :type 'text-form-control
		       :size 35
		       :maxlength 30
		       :allow-nil? t
		       :default "Engineering")
   
   (send :type 'button-form-control
	 :label "Send"
	 :onclick ;;(the (ajax-json.js :form-controls (list (the Application-Domain))))
	 (the (gdl-ajax-call :form-controls (list (the Application-Domain)))))))



(publish-gwl-app "/ajt" 'ajax-json-test)