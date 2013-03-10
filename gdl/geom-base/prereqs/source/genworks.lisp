(in-package :com.genworks.lisp)


#-(or allegro lispworks sbcl ccl abcl clisp) (error "Need implementation for get-pid for currently running lisp~%")
(defun get-pid ()
  #+allegro (excl.osi:getpid) 
  #+lispworks (multiple-value-bind (status pid) 
                  (system:call-system-showing-output "echo $PPID" 
                                                     :show-cmd nil :output-stream nil) 
                (declare (ignore status))
                (read-from-string pid))
  #+sbcl (sb-posix:getpid)
  #+ccl (ccl::getpid)

  
  ;;
  ;; This ABCL implementation is lifted from Quicklisp's
  ;; slime-20121125-cvs/swank-abcl.lisp:
  ;;
  #+abcl
  (handler-case 
      (let* ((runtime 
              (java:jstatic "getRuntime" "java.lang.Runtime"))
             (command
              (java:jnew-array-from-array 
               "java.lang.String" #("sh" "-c" "echo $PPID")))
             (runtime-exec-jmethod 		
              ;; Complicated because java.lang.Runtime.exec() is
              ;; overloaded on a non-primitive type (array of
              ;; java.lang.String), so we have to use the actual
              ;; parameter instance to get java.lang.Class
              (java:jmethod "java.lang.Runtime" "exec" 
                            (java:jcall 
                             (java:jmethod "java.lang.Object" "getClass")
                             command)))
             (process 
              (java:jcall runtime-exec-jmethod runtime command))
             (output 
              (java:jcall (java:jmethod "java.lang.Process" "getInputStream")
                          process)))
         (java:jcall (java:jmethod "java.lang.Process" "waitFor")
                     process)
	 (loop :with b :do 
	    (setq b 
		  (java:jcall (java:jmethod "java.io.InputStream" "read")
			      output))
	    :until (member b '(-1 #x0a))	; Either EOF or LF
	    :collecting (code-char b) :into result
	    :finally (return 
		       (parse-integer (coerce result 'string)))))
    (t () 0)))

;;
;; FLAG Lifted from swank-clisp.lisp:
;;
#+clisp
(let ((getpid (or (find-symbol "PROCESS-ID" :system)
		  ;; old name prior to 2005-03-01, clisp <= 2.33.2
		  (find-symbol "PROGRAM-ID" :system)
		  #+win32 ; integrated into the above since 2005-02-24
		  (and (find-package :win32) ; optional modules/win32
		       (find-symbol "GetCurrentProcessId" :win32)))))
  (defun getpid ()			; a required interface
    (cond
      (getpid (funcall getpid))
      #+win32 ((ext:getenv "PID"))	; where does that come from?
      (t -1))))


(defun run-gs (command)
  "Shell out a ghostscript command and handle errors."
  (let ((result (run-program command)))
    (unless (zerop result) (error "Ghostscript threw error"))))

(defun run-program (command &key output ignore-error-status force-shell
			  (element-type uiop:*default-stream-element-type*)
			  (external-format :default)
			  &allow-other-keys)
  (funcall #'uiop:run-program command :output output :ignore-error-status ignore-error-status
	   :force-shell force-shell :element-type element-type :external-format external-format))

(defun run-shell-command (&rest args)
  (warn "~&run-shell-command is deprecated, please use run-program.~%")
  (apply #'run-program args))

(defun set-gs-path (&optional gs-path)
  (setq gdl:*gs-path* 
	(or (and gs-path (probe-file gs-path))
	    (if (featurep :mswindows)
		(or (probe-file (merge-pathnames "gpl/gs/gs8.63/bin/gswin32c.exe" glisp:*gdl-home*))
		    (probe-file (merge-pathnames "c:/gs/gs8.63/bin/gswin32c.exe" glisp:*gdl-home*))
		    (probe-file (merge-pathnames "../gpl/gs/gs8.63/bin/gswin32c.exe" glisp:*gdl-home*)))
		(or (probe-file #p"~/bin/gs")
		    (probe-file #p"/usr/local/bin/gs")
		    (probe-file #p"/sw/bin/gs")
		    (probe-file #p"/opt/local/bin/gs")
		    (probe-file #p"/usr/bin/gs") "gs"))))
  (if gdl:*gs-path*
      (format t "~%~%Gnu GhostScript was detected at ~a and registered with GDL.~%~%" (probe-file gdl:*gs-path*))
      (warn "Gnu Ghostscript was not found. PNG and JPEG output will not function. 

You can set it manually with (glisp:set-gs-path <path-to-gs-executable>).~%")))