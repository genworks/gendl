(in-package :gdl-user)


(define-object bad-box (box)
  :computed-slots
  ((length 10)
   (height 20)
   (width (div (the height) 0))))
