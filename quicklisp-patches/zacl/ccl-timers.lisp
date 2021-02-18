;;; mcl-timers contributed by Gary Byers

(in-package "CCL")


;;; A simple timer mechanism for MCL/OpenMCL, which uses a
;;; PERIODIC-TASK to check for expired "timer requests".
;;; In MCL and OpenMCL, PERIODIC-TASKS run at specified
;;; intervals via the same preemption mechanism that the
;;; scheduler uses; they run in the execution context of
;;; whatever thread was preempted, and they're assumed to
;;; run pretty quickly.
;;; This code uses doubly-linked-list elements (DLL-NODEs)
;;; to represent a sorted list of "timer requests"; client
;;; processes use timer requests to schedule an interrupt
;;; action at a specified time.  A periodic task walks this
;;; list once a second (by default), removing those requests
;;; whose time isn't in the future and interrupting the
;;; corresponding processes.


;;; The number of timer interrupts (ticks) per second.
(defmacro ticks-per-second ()
  #+OpenMCL '*ticks-per-second*
  #-OpenMCL 60)


(defun expiration-tick-count (seconds)
  (+ (round (* seconds (ticks-per-second)))
     (get-tick-count)))

(defstruct (timer-request (:include dll-node)
			  (:constructor %make-timer-request))
  expiration-tick			; when the timer expires
  process				; what process to interrupt
  function)				; how to interrupt it


(defun make-timer-request (seconds-from-now function)
  (check-type seconds-from-now (and unsigned-byte fixnum))
  (check-type function function)
  (%make-timer-request
   :expiration-tick (expiration-tick-count seconds-from-now)
   :process *current-process*
   :function function))


;;; the CCL::DEFLOADVAR construct ensures that the variable
;;; will be reinitialized when a saved image is restarted
(defloadvar *timer-request-queue*
    #-openmcl-native-threads (make-dll-header)
    #+openmcl-native-threads (make-locked-dll-header))

;;; Insert the timer request before the first element with a later
;;; expiration time (or at the end of the queue if there's no such
;;; element.)
(defun enqueue-timer-request (r)
  (#-openmcl-native-threads without-interrupts
   #+openmcl-native-threads with-locked-dll-header
   #+openmcl-native-threads (*timer-request-queue*)
   (if (dll-node-succ r)                ;  Already enqueued.
     r                                  ;  Or signal an error.
     (let* ((r-date (timer-request-expiration-tick r)))
       (do* ((node *timer-request-queue* next)
	     (next (dll-node-succ node) (dll-node-succ next)))
	    ((or (eq next *timer-request-queue*)
	         (> (timer-request-expiration-tick next) r-date))
	     (insert-dll-node-after r node)))))))

;;; Remove a timer request.  (It's a no-op if the request has already
;;; been removed.)
(defun dequeue-timer-request (r)
  (#-openmcl-native-threads without-interrupts
   #+openmcl-native-threads with-locked-dll-header
   #+openmcl-native-threads (*timer-request-queue*)
   (when (dll-node-succ r)		;enqueued
     (remove-dll-node r))
   r))

;;; Since this runs in an arbitrary process, it tries to be a little
;;; careful with requests made by the current process (since running
;;; the interrupt function will probably transfer control out of the
;;; periodic task function.)  The oldest (hopefully only) request for
;;; the current process is handled after all other pending requests.
(defun process-timer-requests ()
  (let* ((now (get-tick-count))
         (current-process *current-process*)
         (current-process-action ()))
  (#-openmcl-native-threads progn
   #+openmcl-native-threads with-locked-dll-header
   #+openmcl-native-threads (*timer-request-queue*)

    (do-dll-nodes (r *timer-request-queue*)
      (when (> (timer-request-expiration-tick r) now)
	(return))                       ;  Anything remaining is
                                        ;  in the future.
      (dequeue-timer-request r)
      (let* ((proc (timer-request-process r))
             (func (timer-request-function r)))
        (if (eq proc current-process)
          (if (null current-process-action)
            (setq current-process-action func))
          (process-interrupt (timer-request-process r)
			     (timer-request-function r)))))
    (when current-process-action
      (funcall current-process-action)))))

(%install-periodic-task
 'process-timer-requests                ; Name of periodic task
 'process-timer-requests                ; function to call
 (ticks-per-second)			; Run once per second
 )


(defun invoke-with-timeout (seconds bodyfn timeoutfn)
  (block timeout
    (let* ((timer (ccl::make-timer-request
                    seconds
                    #'(lambda () (return-from timeout (funcall timeoutfn))))))
      (ccl::enqueue-timer-request timer)
      (unwind-protect (funcall bodyfn)
	(ccl::dequeue-timer-request timer)))))


(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  "Execute BODY; if execution takes more than SECONDS seconds, terminate and evaluate TIMEOUT-FORMS."
  `(invoke-with-timeout ,seconds #'(lambda () ,@body)
                        #'(lambda () ,@timeout-forms)))
