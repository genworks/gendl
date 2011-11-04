(in-package :gdl-user)
(define-object box-with-two-viewed-drawing (base-object)

  :objects 
     
  ((drawing :type 'two-viewed-drawing
            :objects (list (the box) (the length-dim)))
     
   (length-dim :type 'horizontal-dimension
               :start-point (the box (vertex :rear :top :left))
               :end-point (the box (vertex :rear :top :right)))
   
   (box :type 'box 
        :length 10 :width 20 :height 30)))

(define-object two-viewed-drawing (base-drawing)

  :input-slots (objects)
    
  :objects

  ((main-view :type 'base-view 
              :projection-vector (getf *standard-views* :trimetric)
              :length (half (the length))
              :center (translate (the center) 
                      :rear (half (the-child length)))
              :objects (the objects))
   
   (top-view :type 'base-view 
             :projection-vector (getf *standard-views* :top)
             :length (half (the length))
             :center (translate (the center) 
                     :front (half (the-child length)))
             :objects (the objects))))
