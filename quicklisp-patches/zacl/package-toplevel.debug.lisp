;;;; package-top-level.debug.lisp

(in-package #:zacl)

(defun top-level.debug:zoom (&optional (stream *standard-output*))
  (print-backtrace-to-stream stream))
