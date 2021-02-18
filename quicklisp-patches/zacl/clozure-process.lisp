;;;; clozure-process.lisp

(in-package #:zacl)

(defclass process (ccl:process)
  ((run-reasons
    :initform nil
    :reader mp:process-run-reasons
    :accessor run-reasons)
   (reasons-lock
    :initform (make-lock)
    :reader reasons-lock)
   (keeps-lisp-alive-p
    :initform nil
    :accessor mp:process-keeps-lisp-alive-p)
   (property-list
    :initform nil
    :accessor mp:process-property-list)))

(defmethod print-object ((process process) stream)
  (print-unreadable-object (process stream :type t :identity t)
    (format stream "[zacl] ~s ~s" (process-name process) (ccl:process-whostate process))))

(defun make-process (name &key (class 'process) initial-bindings)
  (ccl:make-process name
                    :class class
                    :initial-bindings initial-bindings))

(defun process-name (process)
  (ccl:process-name process))

(defun add-run-reason (process object)
  (with-lock-held ((reasons-lock process))
    (push object (run-reasons process)))
  (ccl:process-enable process))

(defun revoke-run-reason (process object)
  (with-lock-held ((reasons-lock process))
    (setf (run-reasons process)
          (delete object (run-reasons process))))
  (unless (run-reasons process)
    (ccl:process-reset process)))

(defun yield (process)
  (when process
    ;; XXX private
    (ccl::process-yield process)))

(defun kill (process)
  (when (string-equal (ccl:process-whostate process) "Reset")
    (ccl:process-enable process))
  (ccl:process-kill process))

(defun preset (process fun &rest args)
  (apply #'ccl:process-preset process fun args))

(defun process-run-function (name-or-plist function &rest arguments)
  (apply #'ccl:process-run-function
         (list* :class 'process name-or-plist)
         function arguments ))

(defun current-process ()
  ccl:*current-process*)
