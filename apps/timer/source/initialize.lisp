(in-package :timer-journaler)

(defun initialize ()
  (publish-directory 
   :prefix "/timer-static/"
   :destination (format nil "~a" (probe-file (merge-pathnames "../static/" *static-path*)))))


(initialize)

