(in-package :timer-journaler)


(define-object background-timer-mixin ()

  :computed-slots ((background-seconds nil :settable)
		   (background-minutes nil :settable)
		   (background-timer-thread nil :settable))
  
  :functions
  ((timer-finished
    ()
    (sleep 0.2)
    (format t "~&~%Timer Finished. 

 Background-minutes: ~a
 Background-seconds: ~a
  Form-minutes:  ~a
  Form-seconds:  ~a
"
	    (the background-minutes)
	    (the background-seconds)
	    (the timer-form-min value)
	    (the timer-form-sec value))

    (sleep 120)

    #+allegro
    (net.post-office:send-letter 
		       *smtp-server*
		       "support@genworks.com"
		       "david.cooper@genworks.com"
		       (format nil "Dear Citizen,

Your latest time unit has expired and no journal entry has been
received. Please take care of this matter immediately at the following
location:
 
 ~a

Regards,

 Bureau of Records Enforcement 
 The Ministry of Time

"
			       (the url))
		       :subject "Journal Entry needed"))

   
   (cancel-background-timer
    ()
    (when (the background-timer-thread)
      (bt:destroy-thread (the background-timer-thread)))
    (the (restore-slot-default! :background-timer-thread)))
    

   (start-background-timer
    ()

    (when (the background-timer-thread)
      (warn "Background timer already running. Terminating...~%")
      (the cancel-background-timer))
    
    (the (set-slots! (list :background-seconds (the timer-form-sec value)
			   :background-minutes (the timer-form-min value))))

    (format t "~&~%Starting Background Timer, min: ~a, sec: ~a~%~%"
	    (the background-minutes) (the background-seconds))
    

    (let ((thread
	   (bt:make-thread
	    #'(lambda()
		(let ((real-time (get-internal-real-time)))
		  (do ()
		      ((and (zerop (the background-minutes)) (zerop (the background-seconds)))
		       (the timer-finished))
				 
		    (sleep (/ (- 1000 (- (get-internal-real-time) real-time)) 1000))
				 
		    (setq real-time (get-internal-real-time))
			  
		    (let ((seconds (1- (the background-seconds))))
		      (if (= seconds -1)
			  (the (set-slots! (list :background-seconds 59
						 :background-minutes (1- (the background-minutes)))))
			  (the (set-slot! :background-seconds seconds))))

		    (the timer-form-min (set-slot! :value (the background-minutes)))
		    (the timer-form-sec (set-slot! :value (the background-seconds)))
		    

		    (when *debug?*
		      (format t "~&~%Background Timer woke up, min: ~a, sec: ~a~%~%"
			      (the background-minutes) (the background-seconds))))))
	    :name "Background timer")))
      (the (set-slot! :background-timer-thread thread))))))
   

;;
;; FLAG -- for debug use only. 
;;
(defun kill-bt ()
  (let ((threads (remove-if-not #'(lambda(proc) (string-equal (bt:thread-name proc) "Background timer")) (bt:all-threads))))
    (mapc #'bt:destroy-thread threads)))
