(in-package :gdl-user)

(define-object box-with-hidden (outline-specialization-mixin box)
  
  :input-slots
  ((length 10) (width 20) (height 30)
   
   (display-controls (list :color :blue :transparency 0.5)))
  
  :hidden-objects
  ((port-box :type 'box
             :length (half (half (the length)))
             :width (half (half (the width)))
             :length (half (half (the length)))
             :center (the (face-center :left))
             :display-controls (list :color :red))
   
   (starboard-box :type 'box
                  :length (half (half (the length)))
                  :width (half (half (the width)))
                  :length (half (half (the length)))
                  :center (the (face-center :right))
                  :display-controls (list :color :green))))
   
   
                 
                 
  
  
