(in-package :gdl-surf-user)

(define-object mybox (box-solid)
  
  
  :computed-slots
  ((length 20 :settable) 
   (width (progn (format t "Hey Now~%") (twice (the length))))
   (height 40)
   (display-controls (list :color :blue :transparency 0.3))


   
   ))
   
