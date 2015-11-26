(in-package :timer-journaler)

(define-object journal-entry () 
  
  :input-slots ((name nil :settable)                 ; name of the user
		(email nil :settable)                ; email of the user
		(time-set nil :settable)             ; amount of time timer was set for
		(universal-time-start nil :settable)  ; actual time timer was started
		(universal-time-end nil :settable)    ; actual time timer ended
		(content nil :settable))             ; content of the journal

  :computed-slots 
  ((duration (when (and (the universal-time-start)
			(the universal-time-end))
	       (- (the universal-time-end)(the universal-time-start))))

   (pause-time (when (and (the time-set) (the duration))
		 (- (the duration) (the time-set))))

   (inner-html (with-cl-who-string ()
		 (when (the universal-time-start)
		   (htm ((:div :class "journal-time")
			 (fmt "Started: ~a"
			      (iso-8601-date (the universal-time-start) :include-time? t)))))
		 (when (the universal-time-end)
		   (htm ((:div :class "journal-time")
			 (fmt "Finished: ~a"
			      (iso-8601-date (the universal-time-end) :include-time? t)))))
		 (when (the time-set)
		   (htm ((:div :class "journal-time")
			 (fmt "Planned Duration: ~a:~a"
			      (floor (/ (the time-set) 60)) (mod (the time-set) 60)))))
		 (when (the duration)
		   (htm ((:div :class "journal-time")
			 (fmt "Actual Duration: ~a:~a"
			      (floor (/ (the duration) 60)) (mod (the duration) 60)))))
		 (when (the pause-time)
		   (htm ((:div :class "journal-time")
			 (fmt "Total Pause Time: ~a:~a"
			      (floor (/ (the pause-time) 60)) (mod (the pause-time) 60)))))
		 (when (the content)
		   (htm ((:div :class "journal-descr") (fmt "~a" (the content)))))))))


(define-object assembly (background-timer-mixin base-ajax-sheet)

  :input-slots ((timer-minutes-default 0) (timer-seconds-default 3))
  
  :computed-slots
  ((current-journal-entry nil :settable) (timer-paused? nil :settable)

   (title "Genworks Timer and Journaler")
   

   (main-sheet-body (with-cl-who-string ()
		      (when gwl:*developing?* (str (the development-links)))
		      ((:div :id "content") 
		       (str (the ajax-scripts-section main-div))
		       (str (the imported-scripts))
		       ((:h1 :id "header") "timer")
		       (str (the timer-form main-div))
		       (str (the journal-form main-div))
		       ((:ul :id "journal") 
			(str (the journal-entries-display main-div))))))

   (additional-header-content (with-cl-who-string () 
				(:link :href "/timer-static/style/styles.css" 
				       :rel "stylesheet"
				       :type "text/css")))
   

   (imported-scripts (with-cl-who-string () 
		       ;;(:script :src "https://code.jquery.com/jquery-2.1.4.min.js")
		       ;;(:script :src "/timer-static/plugins/hideseek/jquery.hideseek.min.js")
		       (:script :src "/timer-static/scripts.js")
		       ;;(:script :src "/timer-static/jq-scripts.js")
		       ))
   
   
   (journal-recordable? (and (not (the timer-paused?))
			     (zerop (the background-minutes))
			     (zerop (the background-seconds))
			     (the current-journal-entry))))
  
  :objects
  ((entries :type 'journal-entry :sequence (:indices nil))
				
   (timer-form-min :type 'text-form-control
		   :domain :number :id "minutes" :size 2
		   :default (format nil "~2,'0d" (the timer-default-form-min value)))

   (timer-form-sec :type 'text-form-control
		   :domain :number :id "seconds" :size 2
		   :default (format nil "~2,'0d" (the timer-default-form-sec value)))

   (timer-default-form-min :type 'text-form-control 
			   :ajax-submit-on-change? t :id "default-minutes" :size 2
			   :default (format nil "~a" (the timer-minutes-default)))

   (timer-default-form-sec :type 'text-form-control
			   :ajax-submit-on-change? t :id "default-seconds" :size 2
			   :default (format nil "~2,'0d" (the timer-seconds-default)))

   (name-form :type 'text-form-control :default "Name" :id "user-name")

   (email-form :type 'text-form-control :default "Email" :id "user-email")

   (journal-entry-form :type 'text-form-control :default "Description of task" 
		       :id "journal-entry")
   
   (timer-start-button :type 'button-form-control 
		       :onclick "timerStart();"
		       :label (if (the timer-paused?) "Restart" "Start"))

   (timer-pause-button :type 'button-form-control :onclick "timerPause();" :label "Pause")

   (timer-reset-button :type 'button-form-control :onclick "timerReset();" :label "Reset")

   (record-journal-button :type 'button-form-control :onclick "recordJournal();" :label "Record")

   (ajax-scripts-section
    :type 'sheet-section
    :inner-html (with-cl-who-string () 
		  ((:script :type "text/javascript")
		   
		   "function visibilityChanged () {if !(document.hidden) {alert('vis. changed not hidden event')}};"

		   (fmt "function startTimerAjax () {~a}" 
			(the (gdl-ajax-call 
			      :form-controls (list (the timer-form-min)
						   (the timer-form-sec))
			      :function-key :start-timer-tasks)))
		   (fmt "function recordJournalAjax () {~a}" 
			(the (gdl-ajax-call :form-controls (list (the journal-entry-form)) 
					    :function-key :record-journal-entry)))
		   (fmt "function pauseTimerAjax () {~a}" 
			(the (gdl-ajax-call :function-key :pause-timer-tasks)))
		   (fmt "function resetTimerAjax () {~a}" 
			(the (gdl-ajax-call :function-key :reset-timer-tasks)))
		   (fmt "function endTimerAjax () {~a}"
			(the (gdl-ajax-call :function-key :end-timer-tasks))))))

   
   (journal-entries-display :type 'sheet-section
			    :inner-html (with-cl-who-string ()
					  (:ul
					   (dolist (entry (reverse (list-elements (the entries))))
					     (htm 
					      ((:li :class "journal-entry")
					       (str (the-object entry inner-html))))))))
    
   (timer-form 
    :type 'sheet-section 
    :inner-html (with-cl-who-string ()
		  "Timer" (:br)
		  (str (the timer-form-min form-control-string)) ":" 
		  (str (the timer-form-sec form-control-string))
		  (:br)
		  (when (or (not (the current-journal-entry))
			    (the timer-paused?))
		    (str (the timer-start-button form-control-string)))
		  (when (and (the current-journal-entry)
			     (not (the timer-paused?))
			     (not (the journal-recordable?)))
		    (str (the timer-pause-button form-control-string)))))
   
   (journal-form 
    :type 'sheet-section 
    :inner-html (with-cl-who-string ()
		  (when (the journal-recordable?)
		    (str (the journal-entry-form form-control-string))
		    (str (the record-journal-button form-control-string))))))
      
  :functions 
  ((start-timer-tasks 
    ()
    (if (the timer-paused?)
	(the (set-slot! :timer-paused? nil))
	(let* ((name (the name-form value))
	       (gensym (subseq (write-to-string (gensym)) 2))
	       (universal-time (get-universal-time))
	       (start-minutes (the timer-form-min value))
	       (start-seconds (the timer-form-sec value))
	       (index
		(make-keyword (string-append  name "-" gensym "-" (write-to-string universal-time)))))
	  (the entries (insert! index))
	  (the (entries index)
	       (set-slots! (list :name name :email (the email-form value)
				 :time-set (+ (* start-minutes 60) start-seconds)
				 :universal-time-start universal-time)))
	  (the (set-slot! :current-journal-entry (the (entries index))))
	  (the timer-default-form-min (set-slot! :value start-minutes))
	  (the timer-default-form-sec (set-slot! :value start-seconds))))
    (the start-background-timer))

   (end-timer-tasks 
    ()
    (the current-journal-entry (set-slot! :universal-time-end (get-universal-time)))

    (the zero!))
   
   ;;
   ;; FLAG -- split this into separate drift-check and zeroing of the values. 
   ;;
   (zero!
    ()
    (unless (and (zerop (the timer-form-min value))
		 (<= (the timer-form-sec value) 1))
      (warn "Web page timer and background timer have drifted more than one second: ~a:~a~%" 
	    (the timer-form-min value) (the timer-form-sec)))
    (the timer-form-min (set-slot! :value 0))
    (the timer-form-sec (set-slot! :value 0)))

   (pause-timer-tasks 
    () 
    (the timer-form-sec (set-slot! :value (the background-seconds)))
    (the timer-form-min (set-slot! :value (the background-minutes)))
    (the cancel-background-timer) 
    (the (set-slot! :timer-paused? t)))


   (reset-timer-tasks 
    ()
    (the timer-form-min restore-defaults!)
    (the timer-form-sec restore-defaults!)
    (the cancel-background-timer)
    (when (the current-journal-entry)
      (the entries (delete! (the current-journal-entry index)))
      (the (restore-slot-default! :current-journal-entry))))

   (record-journal-entry 
    ()
    (when (the journal-recordable?)
      (the current-journal-entry (set-slot! :content (the journal-entry-form value)))
      (the (restore-slot-default! :current-journal-entry)) (the reset-timer-tasks)))))



