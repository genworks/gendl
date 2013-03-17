(in-package :surf)

(defun initialize! ()
  (let ((anything-changed? nil))
    (unless *geometry-kernel* 
      (setq anything-changed? t)
      (make-geometry-kernel :vanilla))
    anything-changed?))

;;
;; FLAG -- put this into internals package, not gdl itself.
;;
(setq gdl::*packages-to-initialize* (append gdl::*packages-to-initialize* (list :surf)))