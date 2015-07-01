(in-package :timer-journaler)

(defparameter *source-path* (make-pathname :directory (pathname-directory (glisp:source-pathname))
					   :name nil :type nil))

(publish-directory 
 :prefix "/timer-static/"
 :destination (format nil "~a" (probe-file (merge-pathnames "../static/" *source-path*))))


(define-object assembly (base-ajax-sheet)
  
  :computed-slots
  ((force-update-flag nil :settable)

   (main-sheet-body (with-cl-who-string ()
		      (:div :id "content" 
			    (:script :src "https://code.jquery.com/jquery-2.1.4.min.js")
			    (:script :src "/timer-static/scripts.js")
			    (str (the timer-header main-div))

			    (str (the timer-form-min form-control-string))
			    (str (the timer-form-sec form-control-string))
			    
			    (str (the timer-section main-div))
			    (str (the journal-section main-div))
			    (:br)
			    (str (the journal-entries-display main-div)))))
   (additional-header-content 
    (with-cl-who-string () 
      (:link :href "/timer-static/style/styles.css" 
	     :rel "stylesheet"
	     :type "text/css")
      
      ((:script :type "text/javascript") (str (the report-to-mother)))))


   (report-to-mother (format nil "
function reportToMother ()
 {~a}"
			     (the (gdl-ajax-call :form-controls (list (the timer-form-min)
								      (the timer-form-sec)))))))

  
  :objects
  					; Header that says "TIMER"
   ((timer-header :type 'sheet-section 
		 :inner-html (with-cl-who-string () 
			       (:h1 :id "header" "timer")))


    (timer-section :type 'sheet-section
		   :inner-html (with-cl-who-string ()
				 (:br)
				 (str (the start-button form-control-string))
				 (str (the pause-button form-control-string))))


    (journal-section :type 'sheet-section
		     :inner-html (with-cl-who-string ()
				   (:hr)
				   (str (the journal-form-name form-control-string))
				   (:br)
				   (str (the journal-form-descr form-control-string))
				   (:br)
				   (str (the journal-button form-control-string))))

				 

					; Form for the timer. 
    (timer-form-min :type 'text-form-control
		    :default "20"
		    :size 2)
    (timer-form-sec :type 'text-form-control
		    :default "00"
		    :size 2)

    (start-button :type 'button-form-control
		  :onclick (format nil  "timerStart('~a','~a')"
				   (symbol-name (the timer-form-min id))
				   (symbol-name (the timer-form-sec id)))
		  :label "Start")

    (pause-button :type 'button-form-control
		  :onclick (format nil  "timerPause()")
		  :label "Pause")
    
    
    ;; Form for the name and description (journal)
    ;; FLAG  -- make these into lookup items (menu-form-control or combo box).
    (journal-form-name :type 'text-form-control 
		       :id "journal-name"
		       :default "Name")
    (journal-form-descr :type 'text-form-control 
			:id "journal-descr"
			:default "Description of task")
    
					; Button that says "START"
    

    (journal-button :type 'button-form-control
		    :id "journal-button"
		    :onclick (the (gdl-ajax-call
				   :form-controls (list (the timer-form-min)
							(the timer-form-sec)
							(the journal-form-name)
							(the journal-form-descr))
				   :function-key :record-journal-entry))
		    :label "Record entry")

    ;; Displays journal entries
    (journal-entries-display :type 'sheet-section 
			     :inner-html (progn (the force-update-flag) 
						(with-cl-who-string () 
						  (fmt "Previous entries: ~{<div class='journal-entry'>~{<div class='journal-time'>~{~a min ~a secs~}</div>~%<div class='journal-descr'>~{~a ~}</div>~%~}</div>~}" 
							  (the read-journal-entry))))))

  :functions 
  ((read-journal-entry () 
		       (let ((file-contents
			      (with-open-file 
				  (stream 
				   (concatenate 'string 
						(namestring (merge-pathnames "../db/" *source-path*)) 
						(the journal-form-name value))
				   :direction :input
				   :if-does-not-exist :create)
				(read stream nil nil)))) 
			 file-contents))
		
   (toggle-update-flag! () 
		       (the (set-slot! :force-update-flag 
				       (not (the force-update-flag)))))


   (record-journal-entry () 
			 (the toggle-update-flag!)
			 (with-open-file 
			     (stream 
			      (concatenate 'string 
					   (namestring (merge-pathnames "../db/" *source-path*))
					   (the journal-form-name value))
			      :direction :output
			      :if-does-not-exist :create
			      :if-exists :overwrite)
			   (write 
			    (append 
			     (list (list 
				    (list (the timer-form-min value)
					  (the timer-form-sec value))
				    (list (the journal-form-descr value))))
			     (the read-journal-entry)) 
			    :stream stream)))))

