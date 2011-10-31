(in-package :gdl-user)
(define-object box-with-drawing (base-object)
  
  :objects      
  ((drawing :type 'dimensioned-drawing
            :objects (list (the box) (the length-dim)))
   
   (length-dim :type 'horizontal-dimension
               :start-point (the box (vertex :rear :top :left))
               :end-point (the box (vertex :rear :top :right)))
   
   (box :type 'box 
        :length 10 :width 20 :height 30)))

(define-object dimensioned-drawing (base-drawing)
  :input-slots (objects)
  
  :objects
  ((main-view :type 'base-view 
              :projection-vector (getf *standard-views* :trimetric)
              :objects (the objects))))
