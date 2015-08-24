(in-package :gwl)

(#+allegro
 excl:without-redefinition-warnings
 #-allegro progn
 (#+allegro
  excl:without-package-locks
  #-allegro progn
  (defun start-gwl (&key (port *aserve-port*) (listeners *aserve-listeners*) 
		      (external-format :utf8-base))
    (net.aserve:shutdown)
    (let ((wait-time 1))
      (block :outer
	(do () ()
	  (let ((port port))
	    (block :inner
	      (do ((port-free? (client-test port) (client-test port)))
		  (port-free?
		   (format t (if (> wait-time 1) "~&Retrying AllegroServe on ~a...~%"
				 "~&Trying to start AllegroServe on ~a...~%") port)
		   (if (ignore-errors
			 (net.aserve:start :port port :listeners listeners
					   #-mswindows :external-format #-mswindows external-format))
		    (return-from :outer port)
		    (progn (sleep (random wait-time)) (return-from :inner))))
		(incf port))))
	  (incf wait-time 0.1)))))))
  
