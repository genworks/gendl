(in-package :timer-journaler)

(defparameter *source-path* 
  (make-pathname :directory (pathname-directory (glisp:source-pathname))
					   :name nil :type nil))
(defun initialize ()
  (publish-directory 
   :prefix "/timer-static/"
   :destination (format nil "~a" (probe-file (merge-pathnames "../static/" *source-path*)))))
(initialize)

(define-object journal-entry () 
  
  :input-slots ((name "" :settable)                 ; name of the user
		(email "" :settable)                ; email of the user
		(time-set "" :settable)             ; amount of time timer was set for
		(universal-time-start 0 :settable)  ; actual time timer was started
		(universal-time-end 0 :settable)    ; actual time timer ended
		(content "" :settable))             ; content of the journal

  :computed-slots 
  ((id                                    ; unique identifier for this journal entry
    (concatenate 'string 
		 (the name) 
		 (write-to-string 
		  (the universal-time-start))))
   (time-elapsed                          ; actual amount of time elapsed
    (decode-universal-time 
     (- (the universal-time-end) 
	(the universal-time-start)))))

  :functions
  ; Creates the string serialization of the journal entry. 
  ((to-serialization
    ()
    (write-to-string 
     (list 
      (the name) 
      (the email) 
      (the time-set)
      (the universal-time-start)
      (the universal-time-end)
      (the content)
      (the id))
     :readably t))
   ; Reads from the serialization of the journal entry.
   ; Populates the fields of this journal entry object.
   (from-serialization 
    (serialized-string) 
    (let 
	((deserialization  
	  (read-from-string serialized-string))
	 (fields-list 
	  (list 
	   :name :email :time-set :universal-time-start 
	   :universal-time-end :content :id))) 
      (mapcar 
       (lambda (x y) (the (set-slot! x y))) 
       fields-list deserialization)))
   ; Creates the HTML representation of this journal entry
   (to-html
    ()
    (with-cl-who-string ()
      ((:li :class "journal-entry") 
       ((:div :class "journal-time") 
	(fmt "~a" (the time-set)))
       ((:div :class "journal-descr") 
	(fmt "~a" (the content))))))))

(define-object assembly (background-timer-mixin base-ajax-sheet)

  :input-slots ((timer-minutes-default 20)
		(timer-seconds-default 0)
		(current-journal-entry nil)
		(timer-paused nil))
  
  :computed-slots
  ((force-update-flag nil :settable)
   ; ------------------------------------------------------------------------------ ;
   ; Main sheet body puts everything together. 
   (main-sheet-body (with-cl-who-string ()
		      ((:div :id "content") 
		       (str (the ajax-scripts))
		       (str (the imported-scripts))
		       ((:h1 :id "header") "timer")
		       (str (the timer-defaults-form main-div))
		       (str (the timer-form main-div))
		       (str (the journal-form main-div))
		       ((:ul :id "journal") 
			(str (the journal-entries-display main-div))))))
   (additional-header-content 
    (the imported-css))
   ; ------------------------------------------------------------------------------ ;
   ; Custom javascript and css sheet-sections and ajax calls. 

   ; CSS that we are importing from the outside world. 
   (imported-css 
    (with-cl-who-string () 
      (:link :href "/timer-static/style/styles.css" 
	     :rel "stylesheet"
	     :type "text/css")))
   ; Scripts that we are importing from the outside world.
   (imported-scripts 
    (with-cl-who-string () 
      (:script :src "https://code.jquery.com/jquery-2.1.4.min.js")
      (:script :src "/timer-static/plugins/hideseek/jquery.hideseek.min.js")
      (:script :src "/timer-static/scripts.js")))
   ; Ajax calls generated using gdl-ajax-call
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
			   :function-key 
			   :start-timer-tasks)))
	     (format nil "function recordJournalAjax () {~a}" 
		     (the (gdl-ajax-call 
			   :form-controls (list 
					   (the journal-entry-form)) 
			   :function-key 
			   :record-journal-entry)))
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
			   :end-timer-tasks)))))))))

  :objects
  (; This section is pretty heavy, so I'm commenting it as good as I can.

   ; ------------------------------------------------------------------------------ ;
   ; Form field objects. 

   ; The minutes field. 
   (timer-form-min :type 'text-form-control 
		   :default (format nil "~a" (the timer-minutes-default))
		   :id "minutes"
		   :size 2)
   ; The seconds field. 
   (timer-form-sec :type 'text-form-control 
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

   ; ------------------------------------------------------------------------------ ;
   ; Sheet section for displaying journal entries. There is a toggle flag that 
   ; allows this section to refresh every time a journal entry is added. 
   ; 
   ; Note to developer: This opens the file using the email field, 
   ; reads it into a list of serialized strings, and then does a mapcar 
   ; over the serialized strings to turn them into HTML. 
   ; Finally, it uses reduce over the mapcar to append all the HTML together. 
   (journal-entries-display 
    :type 'sheet-section 
    :inner-html 
    (let ((serialized-strings 
	   (with-open-file (stream (string-append 
				    (namestring (merge-pathnames "../db/" 
								 *source-path*)) 
				    (the email-form value))
				   :direction :input
				   :if-does-not-exist :create)
	     (loop for str = (read stream nil) 
		  while str collect str)))
	  (temp-journal-object 
	   (make-object 'journal-entry))) 
      (the force-update-flag)
      (reduce 'string-append 
	      (mapcar 
	       #'(lambda (s) 
		 (the-object temp-journal-object (from-serialization s))
		 (the-object temp-journal-object to-html)) 
	       serialized-strings))))

   ; ------------------------------------------------------------------------------ ;
   ; Finally, some sheet-sections to put these pieces all together. 

   ; The form for the timer. 
   (timer-form 
    :type 'sheet-section 
    :inner-html (with-cl-who-string ()
		  "Timer" (:br)
		  (str (the timer-form-min form-control-string)) ":" 
		  (str (the timer-form-sec form-control-string))
		  (:br)
		  (str (the name-form form-control-string))
		  (str (the email-form form-control-string))
		  (str (the timer-start-button form-control-string)) 
		  (str (the timer-pause-button form-control-string))
		  (str (the timer-reset-button form-control-string))))
   ; The form for the timer defaults. 
   (timer-defaults-form 
    :type 'sheet-section 
    :inner-html (with-cl-who-string () 
		  "Defaults" (:br) 
		  (str (the timer-default-form-min form-control-string)) ":" 
		  (str (the timer-default-form-sec form-control-string))))
   ; The form for the journal entry. 
   (journal-form 
    :type 'sheet-section 
    :inner-html (with-cl-who-string () 
		  (str (the journal-entry-form form-control-string))
		  (str (the record-journal-button form-control-string)))))
      
  :functions 
  (
   ; ------------------------------------------------------------------------------ ;
   ; Helper functions. 

   ; Toggles the update flag for the journal entries display. 
   (toggle-update-flag! 
    () 
    (the (set-slot! :force-update-flag 
		    (not (the force-update-flag)))))

   ; ------------------------------------------------------------------------------ ;
   ; Main functions: one for starting the timer, one for stopping it. 

   ; Called when the start button is pressed. Creates and sets current-journal-entry 
   ; but only if the timer was not just paused. Starts the background timer. 
   (start-timer-tasks 
    () 
    (if (the timer-paused)
	nil 
	(progn 
	  (the (set-slot! :current-journal-entry 
			 (make-object 'journal-entry 
				      :name (the name-form value)
				      :email (the email-form value)
				      :time-set (format nil "~a:~a" 
							(the timer-form-min value)
							(the timer-form-sec value))
				      :universal-time-start (get-universal-time)))) 
	  (the (set-slot! :timer-paused nil))))
    (the start-background-timer))

   ; Called when the timer runs down to zero. Cancels the background timer and fills in 
   ; the current journal entry's end timestamp. 
   (end-timer-tasks 
    () 
    (the current-journal-entry (set-slot! :universal-time-end 
					 (get-universal-time)))
    (the cancel-background-timer))

   ; Called when the pause button is pressed. Cancels the background timer, 
   ; and sets the timer-paused flag to t. 
   (pause-timer-tasks 
    () 
    (the cancel-background-timer) 
    (the (set-slot! :timer-paused t)))

   ; Called when the reset button is pressed. Cancels the background timer. 
   (reset-timer-tasks 
    ()
    (the cancel-background-timer)
    (the timer-form-min restore-defaults!)
    (the timer-form-sec restore-defaults!))

   ; Called when the button for recording the journal entry is pressed. 
   ; It records the journal entry's content and then writes it to a file.
   (record-journal-entry 
    () 
    (the toggle-update-flag!)
    (if (and (eq (the timer-form-min value) "0")
	     (eq (the timer-form-sec value) "0"))
	(progn (the current-journal-entry (set-slot! :content 
					 (the journal-entry-form value)))
	       (with-open-file 
		   (stream 
		    (string-append (namestring (merge-pathnames "../db/" *source-path*)) 
				   (the email-form value))
		    :direction :output 
		    :if-does-not-exist :create
		    :if-exists :append) 
		 (write 
		  (the current-journal-entry to-serialization) 
		  :stream stream)))
	nil))))



