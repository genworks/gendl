;;;; package-sys.lisp

(in-package #:zacl)

(defvar sys:*tilde-expand-namestrings* nil)

(define-symbol-macro sys:*current-process* (current-process))

(defun sys::thread-bindstack-index (&rest args)
  (declare (ignore args))
  (error "Not implemented"))

(defun sys:defpatch (&rest args)
  (declare (ignore args)))

(defun sys:command-line-arguments ()
  (command-line-arguments))

(defun sys:reap-os-subprocess (&key pid wait)
  (declare (ignore pid wait))
  (error "Not implemented -- SYS:REAP-OS-SUBPROCESS"))


#+ccl 
(defmacro sys:with-timeout ((timeout &body timeout-body) &body body)
  `(ccl::with-timeout (,timeout ,@timeout-body) ,@body))


#-ccl
(defmacro sys:with-timeout ((timeout &body timeout-body) &body body)
  `(handler-case
       (with-timeout (,timeout) ,@body)
     (timeout () ,@timeout-body)))

(defun sys:gsgc-parameter (kind)
  (declare (ignore kind))
  42)
