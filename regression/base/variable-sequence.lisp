(in-package :gdl-user)


(define-object container ()

  :objects ((kids :type 'vkid
		  :sequence (:indices nil))))


(define-object vkid ()
  :computed-slots
  ((a nil :settable)
   (b nil :settable)))


