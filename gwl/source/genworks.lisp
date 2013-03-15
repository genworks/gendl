(in-package :com.genworks.lisp)


(defparameter *enable-utf8?* t)

(defparameter *base64-encode-func* 
  #+allegro #'(lambda(string)
		(excl:string-to-base64-string string :external-format (if *enable-utf8?* :utf-8 :default)))
  #-allegro
  #'(lambda(string)
      (if *enable-utf8?*
	  (cl-base64:usb8-array-to-base64-string 
	   (babel:string-to-octets string :encoding :utf-8))
	  (cl-base64:string-to-base64-string string))))


(defparameter *base64-decode-func* 
  #+allegro
  #'(lambda(string)
      (excl:base64-string-to-string string :external-format (if *enable-utf8?* :utf-8 :default)))
  #-allegro
  #'(lambda(string)
      (if *enable-utf8?*
	  (babel:octets-to-string 
	   (cl-base64:base64-string-to-usb8-array string) :encoding :utf-8)
	  (cl-base64:base64-string-to-string string))))



(defun class-slots (class)
  #-(or allegro lispworks sbcl ccl) (error "Need implementation for class-slots for currently running lisp.~%")
  (#+allegro mop:class-slots
   #+lispworks hcl:class-slots 
   #+sbcl sb-mop:class-slots 
   #+ccl ccl:class-slots class))

;;
;; from Hunchentoot:
;;

(defun get-backtrace ()
  "Returns a string with a backtrace of what the Lisp system thinks is
the \"current\" error."
  (handler-case
      (with-output-to-string (s)
        (trivial-backtrace:print-backtrace-to-stream s))
    (error (condition)
      (format nil "Could not generate backtrace: ~A." condition))))


(defun gc-full ()
  "Force a garbage collection."
  #+allegro  (excl:gc t)
  #+lispworks  (hcl:gc-all)
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  )


(defun initialize-multiprocessing ()
  #+lispworks (mp:initialize-multiprocessing)
  ;;
  ;; Don't have to do it in Allegro - see about other CLs when we get here. 
  ;;
  )


(defun local-port (socket)
  (#+allegro 
   socket:local-port 
   #-allegro acl-compat.socket:local-port socket))

(defun match-regexp (string-or-regexp string-to-match
                     &key newlines-special case-fold return
                          (start 0) end shortest)
  (#+allegro excl:match-regexp 
   #-allegro acl-compat.excl:match-regexp string-or-regexp string-to-match 
   :newlines-special newlines-special 
   :case-fold case-fold
   :return return 
   :start start 
   :end end 
   :shortest shortest))

(defun patches-dir ()
  nil)

(defun process-run-function (name-or-options preset-function &rest args)
  (apply #+allegro #'mp:process-run-function
         #-allegro #'acl-compat.mp:process-run-function 
         name-or-options preset-function args))

(defun remote-host (socket)
  #+allegro (socket:remote-host socket)
  #-allegro (acl-compat.socket:remote-host socket))

(defun replace-regexp (string regexp to-string)
  (cl-ppcre:regex-replace-all regexp string to-string))



#+(or allegro sbcl)
(defun room-report (&optional (gc? t))
  "Plist with keys :new-free-cons, :new-used-cons, :new-free-other, :new-used-other, :old-free-cons, :old-used-cons, :old-free-other, :old-used-other. 
 This is the main output from this object and gives a general overview of memory state. The especially
 noteworthy values are usually the :free-used-other and :old-used-other.

:&optional ((gc nil) \"Determines whether to do a full gc before probing the room data.\")
"
  (when gc? 
    (if (integerp gc?) (dotimes (n gc?) (glisp:gc-full)) (glisp:gc-full))) 
  (let ((gdl:self (gdl:make-object 'room-report))) (gdl:the room-data)))
    


;;#+ccl 
#+nil
(gdl:define-object room-report ()

  :computed-slots 
  ((room-string (with-output-to-string (*standard-output*) (room)))

   (room-lines (with-input-from-string (in (gdl:the room-string))
		 (let (lines)
		   (do ((report-line (read-line in nil) (read-line in nil)))
		       ((null report-line) (nreverse lines))
		     (push report-line lines)))))

   (new-data (list :new-free-cons 0 :new-used-cons 0 :new-free-other 0 :new-used-other 0))

   (old-data (list :old-free-cons 0 :old-used-cons (gdl:the cons-bytes) :old-used-other (- (gdl:the space-total) (gdl:the cons-bytes))))
   
   (cons-bytes (gdl:the (parse-line-data "cons objects.")))

   (space-total (gdl:the (parse-line-data "space total.")))

   ("Plist with keys :new-free-cons, :new-used-cons, :new-free-other, :new-used-other, 
     :old-free-cons, :old-used-cons, :old-free-other, :old-used-other. This is the main
     output from this object and gives a general overview of memory state. The especially
     noteworthy values are usually the :free-used-other and :old-used-other."
    room-data (let* ((data (append (gdl:the new-data) (gdl:the old-data)))
		     (total (+ (getf data :new-used-cons)
			       (getf data :old-used-cons)
			       (getf data :new-used-other)
			       (getf data :old-used-other))))
		(let ((mb (/ (gdl:round-to-nearest total 1000000) 1000000)))
		  (append data (list :total total
				     :MB-int mb
				     :MB (format nil "~a MB" mb)))))))

  :functions ((parse-line-data
	       (report-label)
	       (let* ((line (find report-label (gdl:the room-lines) :test #'search))
		      (components (remove "" (glisp:split-regexp "\\s" line) :test #'string-equal))
		      

		 components)))))



#+sbcl
(gdl:define-object room-report ()

  :computed-slots 
  ((room-string (with-output-to-string (*standard-output*) (room)))

   (room-lines (with-input-from-string (in (gdl:the room-string))
		 (let (lines)
		   (do ((report-line (read-line in nil) (read-line in nil)))
		       ((null report-line) (nreverse lines))
		     (push report-line lines)))))

   (new-data (list :new-free-cons 0 :new-used-cons 0 :new-free-other 0 :new-used-other 0))

   (old-data (list :old-free-cons 0 :old-used-cons (gdl:the cons-bytes) :old-used-other (- (gdl:the space-total) (gdl:the cons-bytes))))
   
   (cons-bytes (gdl:the (parse-line-data "cons objects.")))

   (space-total (gdl:the (parse-line-data "space total.")))

   ("Plist with keys :new-free-cons, :new-used-cons, :new-free-other, :new-used-other, 
     :old-free-cons, :old-used-cons, :old-free-other, :old-used-other. This is the main
     output from this object and gives a general overview of memory state. The especially
     noteworthy values are usually the :free-used-other and :old-used-other."
    room-data (let* ((data (append (gdl:the new-data) (gdl:the old-data)))
		     (total (+ (getf data :new-used-cons)
			       (getf data :old-used-cons)
			       (getf data :new-used-other)
			       (getf data :old-used-other))))
		(let ((mb (/ (gdl:round-to-nearest total 1000000) 1000000)))
		  (append data (list :total total
				     :MB-int mb
				     :MB (format nil "~a MB" mb)))))))

  :functions ((parse-line-data
	       (report-label)
	       (let ((line (find report-label (gdl:the room-lines) :test #'search)))
		 (parse-integer (glisp:replace-regexp (first (glisp:split-regexp "\\s" (string-trim (list #\space) line))) "," ""))))))



#+allegro
(gdl:define-object room-report ()

  :computed-slots 
  ((room-string (with-output-to-string (*standard-output*) (room)))

   (room-lines (with-input-from-string (in (gdl:the room-string))
		 (let (lines)
		   (do ((report-line (read-line in nil) (read-line in nil)))
		       ((null report-line) (nreverse lines))
		     (push report-line lines)))))

   (new-data (gdl:the (parse-line-data "New")))
   (old-data (gdl:the (parse-line-data "OTot")))

   ("Plist with keys :new-free-cons, :new-used-cons, :new-free-other, :new-used-other, 
     :old-free-cons, :old-used-cons, :old-free-other, :old-used-other. This is the main
     output from this object and gives a general overview of memory state. The especially
     noteworthy values are usually the :free-used-other and :old-used-other."
    room-data (append (gdl:the new-data) (gdl:the old-data))))

  :functions ((parse-line-data
	       (report-label)
	       (let ((prefix (cond ((string-equal report-label "New") "new")
				   ((string-equal report-label "OTot") "old"))))
		 (let ((report-line (find t (gdl:the room-lines)
				   :test #'(lambda(item report-line)
					     (declare (ignore item))
					     (and (search report-label report-line)
						  (not (search "-----" report-line)))))))
		   (let ((fields (remove "" (glisp:split-regexp "\\s" report-line) :test #'string-equal)))
		 
		     (let ((cons (third fields))
			   (other (fourth fields)))
		       (append
			(destructuring-bind (free used)
			    (glisp:split-regexp ":" cons)
			  (list (gdl:make-keyword (format nil "~a-free-cons" prefix)) (parse-integer free)
				(gdl:make-keyword (format nil "~a-used-cons" prefix)) (parse-integer used)))
			(destructuring-bind (free used)
			    (glisp:split-regexp ":" other)
			  (list (gdl:make-keyword (format nil "~a-free-other" prefix)) (parse-integer free)
				(gdl:make-keyword (format nil "~a-used-other" prefix)) (parse-integer used)))))))))))


(defun slot-definition-name (slot-definition)
  #-(or allegro lispworks sbcl ccl) (error "Need implementation for slot-definition-name for currently running lisp.~%")
  (#+allegro mop:slot-definition-name
   #+lispworks hcl:slot-definition-name 
   #+sbcl sb-mop:slot-definition-name 
   #+ccl ccl:slot-definition-name slot-definition))

(defun socket-bytes-written (socket)
  #-allegro (declare (ignore socket))
  #+allegro (excl::socket-bytes-written socket)
  #-allegro (progn (warn "Returning 0 for socket-bytes-written -- 

please find implementation for the currently running lisp.~%")
                   0))

(defun split-regexp (regexp string)
  (cl-ppcre:split regexp string))

(defmacro with-heuristic-case-mode ((&rest args) &body body)
  (declare (ignore args))
  #-allegro `(let ((*print-case* :downcase)) ,@body)
  #+allegro `(progn ,@body))

(defun with-timeout-sym ()
  "Returns the appropriate symbol for with-timeout, for substitution within macros."
  #+allegro 'sys:with-timeout
  #-allegro 'acl-compat.mp:with-timeout)

(defmacro with-timeout ((seconds &body timeout-body) &body body)
  #+allegro `(mp:with-timeout (,seconds ,@timeout-body)
	       ,@body)
  #-allegro `(acl-compat.mp:with-timeout (,seconds ,@timeout-body)
	       ,@body))

;;
;; FLAG -- clean out symbol dependencies of (:net.aserve :net.aserve.client :net.uri :net.html.generator :cl-who)
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package (list :gwl :net.aserve :net.aserve.client :net.uri :net.html.generator :cl-who) :gwl))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro gwl:define-package (name &rest args)
    `(gdl:define-package ,name 
	 (:shadow #:define-package)
       (:use :gwl :net.aserve :net.aserve.client :net.uri :net.html.generator :cl-who)
       ,@args)))

(gwl:define-package :gwl-user)

