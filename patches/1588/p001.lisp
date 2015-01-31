(in-package :gwl)

(defparameter *1588p001-doc* 
  "Makes the respondent default to the bashee for base-html-sheet, same as for base-ajax-sheet.")

(define-object-amendment base-html-sheet ()
  :input-slots 
  ((respondent (the bashee))))


(format t "~&~%Changes in 1588p001:~%~%~a~%~%" *1588p001-doc*)
