(in-package :gdl-user)

(define-object general-note-sample (base-object)

  :objects
  ((x-axis :type 'line :start (make-point 0 0 0) :end (make-point 1 0 0))
   (origin-text :type 'general-note 
		:start (make-point 0 0 0) 
		:outline-shape-type :rectangle 
		:strings "O" :character-size 0.1)))