;;;; package-util.zip.lisp

(in-package #:zacl)

(defclass util.zip:inflate-stream () ())

(defclass util.zip:deflate-stream () ())

(defun util.zip:deflate-target-stream (stream)
  (error "util.zip:deflate-target-stream is not yet implemented.~%"))
