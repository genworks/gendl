(in-package :gdl-user)

(define-object immune-dimension-drawing (base-drawing)
  
  :input-slots ((objects) (character-size 20) (witness-line-gap 10)
                (witness-line-length 15) (witness-line-ext 5))
    
  :objects
  ((main-view :type 'base-view 
              :projection-vector (getf *standard-views* :trimetric)
              :objects (append (the objects) (list (the length-dim)))
              :immune-objects (list (the length-dim)))
   
   (length-dim :type 'horizontal-dimension
               :character-size (/ (the character-size) 
                                  (the main-view view-scale))
               :witness-line-gap (/ (the witness-line-gap) 
                                    (the main-view view-scale))
               :witness-line-length (/ (the witness-line-length) 
                                       (the main-view view-scale))
               :witness-line-ext (/ (the witness-line-ext) 
                                    (the main-view view-scale))
               :start-point (the box (vertex :rear :top :left))
               :end-point (the box (vertex :rear :top :right))

               )))










