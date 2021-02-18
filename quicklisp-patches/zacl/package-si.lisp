;;;; package-si.lisp

(in-package #:zacl)

(defmacro si:without-scheduling (&body body)
  `(excl:without-interrupts ,@body))

(defun si:global-symbol-value (symbol)
  #+ccl
  (symbol-value-in-process symbol *initial-process*)
  #-ccl
  (symbol-value symbol))

(defun (setf si:global-symbol-value) (new-value symbol)
  ;;; FIXME: ccl::*initial-process* is internal - see if there's a
  ;;; better way
  #+ccl
  (setf (symbol-value-in-process symbol *initial-process*) new-value)
  #-ccl
  (setf (symbol-value symbol) new-value))
