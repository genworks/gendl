(in-package :timer-journaler)

(defun initialize ()

  (setq *db-path* (merge-pathnames "../db/" *source-path*))
  (setq *static-path* (merge-pathnames "../static/" *source-path*))
  (publish-directory 
   :prefix "/timer-static/"
   :destination (format nil "~a" (probe-file (merge-pathnames "../static/" *static-path*)))))


(initialize)

