;;;; package-util.string.lisp

(in-package #:zacl)

(defun util.string:string+ (&rest strings)
  (format nil "~{~A~}" strings))
