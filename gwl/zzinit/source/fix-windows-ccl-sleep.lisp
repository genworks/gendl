(in-package :ccl)

(let (*warn-if-redefine-kernel*)
  (defun %windows-sleep (millis)

    (dotimes (n 3)
      (unless (typep millis '(unsigned-byte 32))
	(setq millis (/ millis 100))))
    
    (do* ((start (floor (get-internal-real-time)
			(floor internal-time-units-per-second 1000))
		 (floor (get-internal-real-time)
			(floor internal-time-units-per-second 1000)))
	  (millis millis (- stop start))
	  (stop (+ start millis)))
	 ((or (<= millis 0)
            (not (eql (#_SleepEx millis #$true) #$WAIT_IO_COMPLETION)))))))
