(in-package :com.genworks.lisp)

(eval-when (compile load eval)
  (defpackage :com.genworks.lisp 
    (:use :common-lisp)
    (:export #:get-pid
             #:run-gs
             #:set-gs-path
             
             )))



(defun featurep (x)
  #+allegro (excl:featurep x)
  #+lispworks (system:featurep x)
  #+clozure (ccl::featurep x)
  #-(or allegro lispworks clozure)
  (error "Need implementation of featurep for currently running lisp.~%"))



(defun set-gs-path (&optional gs-path)
  (if (featurep :mswindows)
      (let (found-path)
        (if (and gs-path (probe-file gs-path)) (setq found-path gs-path)
          (block :gotit
            (dolist (toplevel (list 
                               (pathname-directory (translate-logical-pathname "sys:gpl;"))
                               (pathname-directory (translate-logical-pathname "sys:"))
                               (append (pathname-directory (user-homedir-pathname)) (list "bin"))
                               (list :absolute "program files")
                               (list :absolute)))
              (dolist (directory (list "gs" "ghostscript"))
                (dolist (subdir (list "gs8.63" "gs7.06" "gs6.52"))
                  (let ((candidate (make-pathname :directory (append toplevel (list directory subdir "bin"))
                                                  :name "gswin32c" :type "exe")))
                    (when (and candidate (probe-file candidate)) (setq found-path candidate)
                          (return-from :gotit))))))))

        (setq gdl:*gs-path* found-path)
        (when gdl:*gs-path*
          (format t "~%~%Gnu GhostScript was detected at ~a and registered with GDL.~%~%" (probe-file gdl:*gs-path*))))
    (progn
      (setq gdl:*gs-path*
        (or (probe-file #p"~/bin/gs")
            (probe-file #p"/usr/local/bin/gs")
            (probe-file #p"/sw/bin/gs")
            (probe-file #p"/opt/local/bin/gs")
            (probe-file #p"/usr/bin/gs") "gs"))
      (when (and (stringp gdl:*gs-path*) (string-equal gdl:*gs-path* "gs")) (warn "Gnu Ghostscript (\"gs\") was not found 
in either /usr/local/bin or /usr/bin -- we assume it is elsewhere in your execution path
so we are setting it simply to \"gs\". If Gnu Ghostscript is not installed on your Unix/Linux 
system, please contact Genworks for assistance in installing it.")))))


(defun get-pid ()
  #+allegro (excl.osi:getpid) 
  #+lispworks (multiple-value-bind (status pid) 
                  (system:call-system-showing-output "echo $PPID" 
                                                     :show-cmd nil :output-stream nil) 
                (declare (ignore status))
                (read-from-string pid))
  #-(or allegro lispworks)
  (error "Need implementation for get-pid for currently running lisp~%"))


(defun run-gs (command)
  "Shell out a ghostscript command and handle errors."
  #-allegro (let ((result (asdf-utilities:run-shell-command command)))
              (unless (zerop result) (error "Ghostscript threw error")))
  #+allegro
  (multiple-value-bind (output error return)
      (excl.osi:command-output command)
    (unless (zerop return) (error "Ghostscript command threw error. Result was:
output: ~a
 error: ~a
return: ~a" 
                                          output error return))))
