(in-package :com.genworks.lisp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :com.genworks.lisp 
    (:use :common-lisp)
    (:export #:get-pid
             #:run-gs
             #:run-shell-command
             #:set-gs-path)))

#-(or allegro lispworks sbcl) (error "Need implementation of featurep for currently running lisp.~%")
(defun featurep (x)
  (#+allegro excl:featurep #+lispworks system:featurep #+sbcl sb-int:featurep x))


#-(or allegro lispworks sbcl) (error "Need implementation for get-pid for currently running lisp~%")
(defun get-pid ()
  #+allegro (excl.osi:getpid) 
  #+lispworks (multiple-value-bind (status pid) 
                  (system:call-system-showing-output "echo $PPID" 
                                                     :show-cmd nil :output-stream nil) 
                (declare (ignore status))
                (read-from-string pid))
  #+sbcl (sb-posix:getpid))


(defun run-gs (command)
  "Shell out a ghostscript command and handle errors."
  #-allegro
  (let ((result (asdf-utilities:run-shell-command command)))
    (unless (zerop result) (error "Ghostscript threw error")))
  #+allegro
  (multiple-value-bind (output error return)
      (excl.osi:command-output command)
    (unless (zerop return) (error "Ghostscript command threw error. Result was:
output: ~a
 error: ~a
return: ~a" 
                                          output error return))))


(defun run-shell-command (command &rest args)
;;
;; FLAG -- add specific keyword args e.g. for hide-window? 
;;
  (apply #'asdf-utilities:run-shell-command command args))


(defun set-gs-path (&optional gs-path)
  (setq gdl:*gs-path* 
	(or (and gs-path (probe-file gs-path))
	    (if (featurep :mswindows)
		(probe-file (merge-pathnames "gpl/gs/gs8.63/bin/gswin32c.exe" glisp:*gdl-home*))
		(or (probe-file #p"~/bin/gs")
		    (probe-file #p"/usr/local/bin/gs")
		    (probe-file #p"/sw/bin/gs")
		    (probe-file #p"/opt/local/bin/gs")
		    (probe-file #p"/usr/bin/gs") "gs"))))
  (if gdl:*gs-path*
      (format t "~%~%Gnu GhostScript was detected at ~a and registered with GDL.~%~%" (probe-file gdl:*gs-path*))
      (warn "Gnu Ghostscript was not found. PNG and JPEG output will not function. 

You can set it manually with (glisp:set-gs-path <path-to-gs-executable>).~%")))