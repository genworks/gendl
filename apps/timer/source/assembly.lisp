(in-package :timer-journaler)

(define-object journal-entry () 
  
  :input-slots ((name "" :settable)                 ; name of the user
		(email "" :settable)                ; email of the user
		(time-set "" :settable)             ; amount of time timer was set for
		(universal-time-start 0 :settable)  ; actual time timer was started
		(universal-time-end 0 :settable)    ; actual time timer ended
		(content "" :settable))             ; content of the journal

  :computed-slots 
  ((inner-html (with-cl-who-string ()
		 ((:li :class "journal-entry") 
		  ((:div :class "journal-time") 
		   (fmt "~a" (the time-set)))
		  ((:div :class "journal-descr") 
		   (fmt "~a" (the content))))))))


(define-object assembly (background-timer-mixin base-ajax-sheet)

  :input-slots ((timer-minutes-default 0)
		(timer-seconds-default 3))
  
  :computed-slots
  ((current-journal-entry nil :settable)
   (timer-paused? nil :settable)

   (title "Genworks Timer and Journaler")
   
   (main-sheet-body (with-cl-who-string ()
		      (when gwl:*developing?* (str (the development-links)))
		      ((:div :id "content") 
		       (str (the ajax-scripts))
		       (str (the imported-scripts))
		       ((:h1 :id "header") "timer")
		       (str (the timer-form main-div))
		       (str (the journal-form main-div))
		       ((:ul :id "journal") 
			(str (the journal-entries-display main-div))))))
   (additional-header-content (the imported-css))
   
   ; ------------------------------------------------------------------------------ ;
   ;; Custom javascript and css sheet-sections and ajax calls. 

   ;; CSS that we are importing from the outside world. 
   (imported-css 
    (with-cl-who-string () 
      (:link :href "/timer-static/style/styles.css" 
	     :rel "stylesheet"
	     :type "text/css")))
   
   ;; Scripts that we are importing from the outside world.
   (imported-scripts 
    (with-cl-who-string () 
      ;;(:script :src "https://code.jquery.com/jquery-2.1.4.min.js")
      ;;(:script :src "/timer-static/plugins/hideseek/jquery.hideseek.min.js")
      (:script :src "/timer-static/scripts.js")
      ;;(:script :src "/timer-static/jq-scripts.js")
      ))
   
   
   ;; Ajax calls generated using gdl-ajax-call
   (ajax-scripts 
    (with-cl-who-string () 
      ((:script :type "text/javascript")
       (str (string-append 
	     (format nil "function startTimerAjax () {~a}" 
		     (the (gdl-ajax-call 
			   :form-controls (list 
					   (the timer-form-min)
					   (the timer-form-sec)
					   (the name-form)
					   (the email-form)) 
			   :function-key :start-timer-tasks)))
	     (format nil "function recordJournalAjax () {~a}" 
		     (the (gdl-ajax-call 
			   :form-controls (list (the journal-entry-form)) 
			   :function-key :record-journal-entry)))
	     (format nil "function pauseTimerAjax () {~a}" 
		     (the (gdl-ajax-call 
			   :function-key 
			   :pause-timer-tasks)))
	     (format nil "function resetTimerAjax () {~a}" 
		     (the (gdl-ajax-call 
			   :function-key 
			   :reset-timer-tasks)))
	     (format nil "function endTimerAjax () {~a}" 
		     (the (gdl-ajax-call 
			   :function-key 
			   :end-timer-tasks))))))))

   (journal-recordable? (and (numberp (the background-minutes))
			     (zerop (the background-minutes))
			     (numberp (the background-seconds))
			     (zerop (the background-seconds))
			     (the current-journal-entry)))
   )

  
  :objects
  (
					;

   ;; This section is pretty heavy, so I'm commenting it as good as I can.


   (entries :type 'journal-entry
	    :sequence (:indices nil))
				
					;;; ------------------------------------------------------------------------------ ;
   ; Form field objects. 

   ; The minutes field. 
   (timer-form-min :type 'text-form-control
		   :domain :number
		   :default (format nil "~a" (the timer-minutes-default))
		   :id "minutes"
		   :size 2)
   ; The seconds field. 
   (timer-form-sec :type 'text-form-control
		   :domain :number
		   :default (format nil "~2,'0d" (the timer-seconds-default)) 
		   :id "seconds"
		   :size 2)
   ; The default minutes field. 
   (timer-default-form-min :type 'text-form-control 
			   :ajax-submit-on-change? t
			   :default (format nil "~a" (the timer-minutes-default))
			   :id "default-minutes"
			   :size 2)
   ; The default seconds field. 
   (timer-default-form-sec :type 'text-form-control
			   :ajax-submit-on-change? t
			   :default (format nil "~2,'0d" (the timer-seconds-default)) 
			   :id "default-seconds"
			   :size 2)
   ; The field for your name. 
   (name-form :type 'text-form-control 
	      :default "Name"
	      :id "user-name")
					; The field for your email.
   (email-form :type 'text-form-control 
	       :default "Email"
	       :id "user-email")
   ; The field for your journal entry's content. 
   (journal-entry-form :type 'text-form-control 
		 :default "Description of task" 
		 :id "journal-entry")

   ; ------------------------------------------------------------------------------ ;
   ; Now we have a few buttons: start the timer, pause the timer, and reset the timer, 
   ; as well as a button to record the journal entry. 
   
   ; Starts the timer. Note: You must enter a name and email to start the timer.
   (timer-start-button :type 'button-form-control 
		       :onclick "timerStart();"
		       :label "Start")
   ; Pauses the timer. 
   (timer-pause-button :type 'button-form-control 
		       :onclick "timerPause();"
		       :label "Pause")
   ; Resets the timer. 
   (timer-reset-button :type 'button-form-control 
		       :onclick "timerReset();"
		       :label "Reset")
   ; Records the journal entry. 
   (record-journal-button :type'button-form-control 
			  :onclick "recordJournal();"
			  :label "Record")


   (journal-entries-display :type 'sheet-section
			    :inner-html (with-cl-who-string ()
					  (dolist (entry (list-elements (the entries)))
					    (str (the-object entry inner-html)))))
    

   ;;
   ;; ------------------------------------------------------------------------------ ;
   ;; Finally, some sheet-sections to put these pieces all together.
   
   ;; The form for the timer. 
   (timer-form 
    :type 'sheet-section 
    :inner-html (with-cl-who-string ()
		  "Timer" (:br)
		  
		  (str (the timer-form-min form-control-string)) ":" 
		  (str (the timer-form-sec form-control-string))
		  
		  (:br)

		  (unless (the current-journal-entry)
		    (str (the name-form form-control-string))
		    (str (the email-form form-control-string)))
		  
		  (when (or (not (the current-journal-entry))
			    (the timer-paused?))
		    (str (the timer-start-button form-control-string)))
		  
		  (when (and (the current-journal-entry)
			     (not (the timer-paused?))
			     (not (the journal-recordable?)))
		    (str (the timer-pause-button form-control-string)))

		  #+nil ;; put back in later 
		  (when (or (not (the current-journal-entry))
			    (the timer-paused?))
		    (str (the timer-reset-button form-control-string))
		    (htm "Defaults" (:br) )
		    (str (the timer-default-form-min form-control-string)) ":" 
		    (str (the timer-default-form-sec form-control-string)))))

   
   ;; The form for the journal entry. 
   (journal-form 
    :type 'sheet-section 
    :inner-html (with-cl-who-string ()
		  (when (the journal-recordable?)
		    (str (the journal-entry-form form-control-string))
		    (str (the record-journal-button form-control-string))))))
      
  :functions 
  (
   ; ------------------------------------------------------------------------------ ;
   ; Helper functions. 


   ; ------------------------------------------------------------------------------ ;
   ; Main functions: one for starting the timer, one for stopping it. 

   ; Called when the start button is pressed. Creates and sets current-journal-entry 
   ; but only if the timer was not just paused. Starts the background timer. 
   (start-timer-tasks 
    ()

    (when *debug?* (print-variables (the timer-paused?)))
    
    (unless (the timer-paused?)

      (let* ((name (the name-form value))
	     (gensym (subseq (write-to-string (gensym)) 2))
	     (universal-time (get-universal-time))
	     (start-minutes (the timer-form-min value))
	     (start-seconds (the timer-form-sec value))
	     (index (make-keyword (string-append  name "-" gensym "-"
						  (write-to-string universal-time)))))

	(when *debug?* (print-variables name gensym universal-time
					start-minutes start-seconds index))
	
	(the entries (insert! index))
	(the (entries index) (set-slots! (list :name name
					       :email (the email-form value)
					       :time-set (+ (* start-minutes 60) start-seconds)
					       :universal-time-start universal-time)))
	(the (set-slot! :current-journal-entry (the (entries index))))))

    (when *debug?* (print-variables (the current-journal-entry)))
    
    (the start-background-timer))

   ; Called when the timer runs down to zero. Cancels the background timer and fills in 
   ; the current journal entry's end timestamp. 
   (end-timer-tasks 
    () 
    (the current-journal-entry (set-slot! :universal-time-end (get-universal-time)))
    (the cancel-background-timer))

   ; Called when the pause button is pressed. Cancels the background timer, 
   ; and sets the timer-paused? flag to t. 
   (pause-timer-tasks 
    () 
    (the cancel-background-timer) 
    (the (set-slot! :timer-paused? t))
    (the timer-form-sec (set-slot! :value (the background-seconds)))
    (the timer-form-min (set-slot! :value (the background-minutes)))
    )

   ; Called when the reset button is pressed. Cancels the background timer. 
   (reset-timer-tasks 
    ()
    (the cancel-background-timer)
    (the timer-form-min restore-defaults!)
    (the timer-form-sec restore-defaults!)
    (when (the current-journal-entry)
      (the entries (delete! (the current-journal-entry index)))
      (the (restore-slot-default! :current-journal-entry))))

   ; Called when the button for recording the journal entry is pressed. 
   ; It records the journal entry's content and then writes it to a file.
   (record-journal-entry 
    ()
    (when (the journal-recordable?)
      (the current-journal-entry
	   (set-slot! :content (the journal-entry-form value)))
      (the (restore-slot-default! :current-journal-entry))
      (the reset-timer-tasks)))))



