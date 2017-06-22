;;
;; Copyright 2002-2011 Genworks International 
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;
;; This source file contains free software: you can redistribute it
;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This source file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with this source file.  If not, see
;; <http://www.gnu.org/licenses/>.
;; 

(in-package :gwl)

(defvar *static-home* nil)

(defun ensure-static-relative-pathname (relative)
  (let ((pathname (merge-pathnames relative *static-home*)))
    (or (probe-file pathname)
	(warn "Expected static subdirectory ~a does not appear to exist.~%" pathname))))

(defun publish-images (server)
  (let ((destination (ensure-static-relative-pathname "gwl/images/")))
    (when destination (publish-directory
		       :prefix "/images/gwl/"
		       :server server
		       :destination (namestring destination)))))
		      
(defun publish-statics (server)
  (let ((destination (ensure-static-relative-pathname "")))
    (when destination (publish-directory
		       :prefix "/static/"
		       :server server
		       :destination (namestring destination)))))

(defun publish-style (server)
  (let ((destination (ensure-static-relative-pathname "gwl/style/")))
    (when destination (publish-directory
		       :prefix "/style/"
		       :server server
		       :destination (namestring destination)))))


(dolist (func (list 'publish-images 'publish-statics 'publish-style))
  (pushnew func *publishers*))


(defvar *aserve-listeners* 25)
(defvar *aserve-port* 9000)
(defvar *aserve-start-args* nil)



#+nil
(defun client-test (port)

  #+allegro

  (multiple-value-bind (result error)
      (ignore-errors  
	(glisp:with-timeout (2 (error "AllegroServe port probe timed out on port ~a. 
Perhaps a zombie process is holding port ~a?~%" port port))

	  
	  (#+nil
	   net.aserve.client:do-http-request
	   drakma:http-request (format nil "http://localhost:~a" port))))

    
    (declare (ignore result))
    (when (typep error 'error)
      port))
  
  #-allegro
  (let* ((result
	  (handler-case
	      (let ((sock (usocket:socket-listen "localhost" port)))
		(usocket:socket-close sock))
	    (usocket:address-in-use-error (e) :in-use)
	    (t (e) :unknown))))
    (unless (member result '(:in-use :unknown)) port)))


(defun client-test (port)
  #-ccl
  (multiple-value-bind (result error)
      (ignore-errors  
	(glisp:with-timeout (2 (error "AllegroServe port probe timed out on port ~a. 
Perhaps a zombie process is holding port ~a?~%" port port))
	  (net.aserve.client:do-http-request (format nil "http://localhost:~a" port))))
    (declare (ignore result))
    (when (typep error 'error)
      port))
  #+ccl
  (let* ((result
	  (handler-case
	      (let ((sock (usocket:socket-listen "127.0.0.1" port)))
		(usocket:socket-close sock))
	    (usocket:address-in-use-error (e) (declare (ignore e)) :in-use)
	    (t (e) (declare (ignore e)) :unknown))))
    (unless (member result '(:in-use :unknown)) port)))


(defun start-gwl (&key (port *aserve-port*) (listeners *aserve-listeners*)
		    ;;
		    ;; FLAG -- figure out external-format for the other Lisps. 
		    ;;
		    (external-format #+allegro :utf8-base #-allegro :utf8) aserve-start-args)
  (net.aserve:shutdown)
  (let ((wait-time 1))
    (block :outer
      (do () (nil)
	(let ((port port))
	  (block :inner
	    (do ((port-free? (client-test port) (client-test port)))
		(port-free?
		 (format t (if (> wait-time 1) "~&Retrying AllegroServe on ~a...~%"
			       "~&Trying to start AllegroServe on ~a...~%") port)
		 (if (ignore-errors
		       (apply #'net.aserve:start
			      :port port :listeners listeners
			      ;;#-mswindows :external-format #-mswindows external-format ;; FLAG -- why no external-format for Windows?
			      :external-format external-format
			      aserve-start-args))
		     (return-from :outer port)
		     (progn (sleep (random wait-time)) (return-from :inner))))
	      (incf port))))
	(incf wait-time 0.1))))
  (publish-uris))


#+nil
(defun start-gwl (&key (port *aserve-port*) (listeners *aserve-listeners*) 
		    (external-format #+allegro :utf8-base #+ccl :utf-8 #-(or allegro ccl) (error "find utf-8 external-format for ~a.~%" (lisp-implementation-version)))
		    (aserve-start-args *aserve-start-args*))
  (net.aserve:shutdown :server net.aserve:*wserver*)
  (let ((wait-time 1))
      (block :outer
	(do () (nil)
	  (let ((port port))
	    (block :inner
	      (do ((port-free? (client-test port) (client-test port)))
		  (port-free?
		   (format t (if (> wait-time 1) "~&Retrying AllegroServe on ~a...~%"
				 "~&Trying to start AllegroServe on ~a...~%") port)
		   (if (ignore-errors
			 (setq net.aserve:*wserver*
			       (apply #'net.aserve:start :port port :listeners listeners :server :new
				      #-mswindows :external-format #-mswindows external-format
				      aserve-start-args)))
		    (return-from :outer port)
		    (progn (sleep (random wait-time)) (return-from :inner))))
		(incf port))))
	  (incf wait-time 0.1))))

  (publish-uris))


(defvar *settings* 
  (list (list '*static-home* *static-home* 
	      #'(lambda()
		  (or (and glisp:*gendl-source-home*
			   (probe-file (merge-pathnames "gwl/static/" 
							glisp:*gendl-source-home*)))
		      (and glisp:*gdl-home*
			   (probe-file (merge-pathnames "static/"
							glisp:*gdl-home*)))

		      (and glisp:*gdl-program-home*
			   (probe-file (merge-pathnames "static/"
							glisp:*gdl-program-home*)))
		      
		      (warn "~%Static home not found in source directory or parent of program directory.~%"))))))


;;
;; FLAG -- move publishes into global param with the basic publishing data and 
;;         call general-purpose publish function on this data. 
;;    also detect changes in the publishing to include in return value.
;;
(defun initialize ()
  ;;
  ;; FLAG -- investigate if this initialize-multiprocessing business
  ;; is still needed, currently LW-only.
  ;;
  (glisp:initialize-multiprocessing)

  (when (find-package :zacl)
    (setq excl:*initial-terminal-io* *terminal-io*)
    (setf (slot-value net.aserve:*wserver* 'net.aserve::log-stream) excl:*initial-terminal-io*
	  (slot-value (slot-value net.aserve:*wserver* 'net.aserve::default-vhost) 'net.aserve::log-stream) excl:*initial-terminal-io*))
  
  (setq *iid-random-state* (make-random-state t))
  
  (let (anything-changed?)
    (setq anything-changed? (glisp:set-settings *settings*))
    (start-gwl) 
    anything-changed?))



#+ccl
(in-package :ccl)

#+(and ccl windows)
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
	      (not (eql (funcall (read-from-string "#_SleepEx millis") (read-from-string "#$true"))
			(read-from-string "#$WAIT_IO_COMPLETION"))))))))
