(in-package :surf)

(defun initialize ()
  (let ((anything-changed? nil))
    (unless *geometry-kernel* 
      (setq anything-changed? t)
      (make-geometry-kernel :vanilla))
    anything-changed?))

