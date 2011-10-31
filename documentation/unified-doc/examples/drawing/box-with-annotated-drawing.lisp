(in-package :gdl-user)
(define-object box-with-annotated-drawing (base-object)
  
  :objects      
  ((drawing :type 'box-annotated-drawing
            :objects (list (the box)))
   
   (box :type 'box 
        :length 10 :width 20 :height 30)))

(define-object box-annotated-drawing (base-drawing)
  :input-slots (objects (character-size 15)
                (witness-line-gap 10)
                (witness-line-length 15)
                (witness-line-ext 5))
  
  :objects
  ((main-view :type 'base-view 
              :projection-vector (getf *standard-views* :trimetric)
              :length (half (the length))
              :center (translate (the center) 
                                 :rear (half (the-child length)))
              :objects (the objects)
              :annotation-objects (list (the main-length-dim)))

   (main-length-dim
    :type 'vertical-dimension
    :pass-down (character-size witness-line-gap witness-line-length 
                               witness-line-ext)
    :start-point (the main-view 
                   (view-point (the box (vertex :rear :top :right))))
    :end-point (the main-view 
                 (view-point (the box (vertex :rear :bottom :right))))
    :dim-value (3d-distance (the box (vertex :rear :top :right))
                            (the box (vertex :rear :bottom :right)))
    :text-above-leader? nil)
   
   
   (top-view :type 'base-view 
             :projection-vector (getf *standard-views* :front)
             :length (half (the length))
             :center (translate (the center) 
                                :front (half (the-child length)))
             :objects (the objects)
             :annotation-objects (list (the top-length-dim)))
   
   (top-length-dim 
    :type 'vertical-dimension
    :pass-down (character-size witness-line-gap witness-line-length 
                               witness-line-ext)
    :start-point (the top-view 
                   (view-point (the box (vertex :rear :top :right))))
    :dim-scale (/ (the top-view view-scale))
    :text-above-leader? nil
    :end-point (the top-view 
                 (view-point (the box (vertex :rear :bottom :right))
                             )))))



