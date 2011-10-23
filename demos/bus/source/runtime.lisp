(in-package :bus)

(define-object runtime-maker (build-utils:runtime-maker)
  
  :computed-slots
  ((source-directory *source-directory*)))

(defun make-runtime ()
  (let ((self (make-object 'runtime-maker)))
    (the make)))


