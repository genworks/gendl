(in-package :www.genworks.com)

(defparameter *smtp-server* "smtp.comcast.net")

(define-object contact-us (base-site-sheet)

  :input-slots ((license-info *licensed-emails*))
 
  :computed-slots
  ((title "Genworks International - Contact Us")
   (link-title  "Contact us")
   
   (right-section-inner-html
    (with-cl-who-string ()
      (:h2 "Contact Genworks") ((:div :id "contact"))

      (:p "Please provide your email address so that we may check our records and 
provide the appropriate level of service.")

      (with-html-form (:cl-who? t)
	
	(if (the message-sent?)
	    (htm (:p "Your message has been sent. Thank you for contacting us.")

		 (:p "Please " ((:span :class "clickable" 
				       :onclick (the (gdl-ajax-call :function-key :reset-message-sent)))
				"click here") " if you would like to send another message."))

	    (htm (when (the matched-name)
		   (htm (:h3 "Welcome " (str (the matched-name)) ".")))

		 (:fieldset
		  (:p (str (the email-address html-string)))
		  (:p (str (the subject html-string)))
		  (:p (str (the message-body html-string)))
		  (:p (str (the send-button form-control-string)))))))))

   (matched-email (find (the email-address value) (the license-info)
			:test #'string-equal :key #'first))

   (matched-name (when (the matched-email) (getf (rest (the matched-email)) :name)))
   
   (supported-subject-options (list :tech-support "ask a Technical Support question"
				    :bug-report "report an apparent Software Issue"))

   (message-sent? nil :settable)

   )


  :objects
  (;; FLAG -- change this to email-form-control when available 
   (email-address :type 'text-form-control 
		  :size 42
		  :label-position :prepend
		  :prompt "Your Email Address: "
		  :default ""
		  :ajax-submit-on-change? t)

   (subject :type 'menu-form-control
	    :prompt "Subject: "
	    :size 1
	    :label-position :prepend
	    :default (if (the matched-email) :tech-support :general)
	    :ajax-submit-on-change? t
	    :choice-plist 
	    (append (when (the matched-email) (the supported-subject-options))
		    (list :sales "purchase Genworks GDL and/or Technical Support"
			  :var "become a Genworks Value-added Reseller"
			  :evaluation "do a Software Evaluation"
			  :license-renewal "renew or retrieve license key file(s)"
			  :general "ask a general question about Genworks GDL")))

   
   (message-body :type 'text-form-control
		 :ajax-submit-on-change? t
		 :default ""
		 :label-position :table-td
		 :prompt "Message: "
		 :size 42
		 :rows 25)

   (send-button :type 'button-form-control
		:default "Send"
		:onclick (the (gdl-ajax-call :function-key :send-email)))

   )


  :functions
  ((reset-message-sent 
    ()
    (the (set-slot! :message-sent? nil)))

   (send-email
    ()
    (format t "Sending Email...~%")
    
    (print-variables *smtp-server* (the email-address)
		     (the message-body value) 
		     (the subject value)
		     (getf (the subject choice-plist)
			   (the subject value)))
    
    #+allegro
    (net.post-office:send-letter *smtp-server* 
				 (the email-address value)
				 "info@genworks.com"
				 (the message-body value)
				 :subject (getf (the subject choice-plist)
						(make-keyword (the subject value))))
    

    (the restore-form-controls!)
    (the (set-slot! :message-sent? t)))

   (restore-form-controls!
    ()
    (dolist (item (list (the subject) (the message-body)))
      (the-object item restore-defaults!)))))



(defparameter *licensed-emails*
  (list (list "david.cooper@genworks.com" :name "Dave")
	(list "teodor-gelu@genworks.com" :name "Teodor")
	(list "michel.vantooren@fokker.com" :name "Michel")
	(list "jochem.berends@ke-works.nl" :name "Jochem")
	(list "trobertson@whiteboxlearning.com" :name "Tom")))
   
