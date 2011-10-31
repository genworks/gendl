(in-package :gdl-user)

(define-object box-with-immune-dimension (base-object)

  :objects      
  ((drawing :type 'immune-dimension-drawing
            :objects (list (the box)))
     
   (box :type 'box 
        :length 10 
        :width 20 
        :height 30)))

