(in-package :timer-journaler)

;; meaningless change to test git.

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
			    (:script :src "/timer-static/scripts.js")
			    (str (the timer-header main-div))
			    (str (the timer-form-min form-control))
			    (str (the timer-form-sec form-control))
			    (:br)
			    (str (the timer-button main-div))
			    (:hr)
			    (str (the journal-form-name form-control))
			    (str (the journal-form-descr form-control))
			    (str (the journal-button main-div))
			    (:br)
			    (str (the journal-entries-display main-div)))))
   (additional-header-content 
    (with-cl-who-string () 
      (:link :href "/timer-static/style/styles.css" 
	     :rel "stylesheet"
	     :type "text/css"))))

  :objects
  					; Header that says "TIMER"
   ((timer-header :type 'sheet-section 
		 :inner-html (with-cl-who-string () 
			       (:h1 :id "header" "timer")))

					; Form for the timer. 
    (timer-form-min :type 'text-form-control
		    :default "20"
		    :size 2)
    (timer-form-sec :type 'text-form-control
		    :default "00"
		    :size 2)

					; Form for the name and description (journal)
    (journal-form-name :type 'text-form-control 
		       :default "Name")
    (journal-form-descr :type 'text-form-control 
			:default "Description of task")
    
					; Button that says "START"
    (timer-button :type 'sheet-section 
		  :inner-html (with-cl-who-string () 
				((:button :onclick 
					 (concatenate 'string 
						      "timerStart('"
						      (symbol-name (the timer-form-min id))
						      "','"
						      (symbol-name (the timer-form-sec id))
						      "')"))
					 "Start")))

    (journal-button :type 'sheet-section 
		    :inner-html (with-cl-who-string () 
				  ((:button :onclick (the (gdl-ajax-call
							   :form-controls (list (the timer-form-min)
										(the timer-form-sec)
										(the journal-form-name)
										(the journal-form-descr))
										:function-key :record-journal-entry)))
				   "Record entry")))

					; Displays journal entries
    (journal-entries-display :type 'sheet-section 
			     :inner-html (progn (the force-update-flag) 
						(with-cl-who-string () 
						  (fmt "Previous entries: ~{~a<br>~}" 
							  (the read-journal-entry))))))

  :functions 
  ((read-journal-entry () 
		       (let ((file-contents
			      (with-open-file 
				  (stream 
				   (concatenate 'string 
						(namestring (merge-pathnames "../db/" *source-path*)) 
						(the journal-form-name value))
				   :direction :input)
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

