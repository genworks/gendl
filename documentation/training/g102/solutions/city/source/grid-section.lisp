(in-package :city)

(define-object grid-section (box)
  
  :input-slots (buildings-list)
  
  :computed-slots 
  ((height 0)
   (display-controls (list :color :periwinkle))
		   
   (potential-neighbor-index-pairs (list (list (1- (first (the index))) (second (the index)))
					 (list (1- (first (the index))) (1- (second (the index))))
					 (list (first (the index)) (1- (second (the index))))
					 (list (1+ (first (the index))) (second (the index)))
					 (list (1+ (first (the index))) (1+ (second (the index))))
					 (list (first (the index)) (1+ (second (the index))))))

   (valid-neighbor-index-pairs (let ((number-of-elements (plist-values (the aggregate number-of-elements))))
				 (remove-if #'(lambda(pair)
						(or (minusp (first pair)) (minusp (second pair))
						    (= (first pair) (first number-of-elements))
						    (= (second pair) (second number-of-elements))) )
					    (the potential-neighbor-index-pairs))))
   (neighbors (cons self (mapcar #'(lambda(pair)
				     (the (sections (first pair) (second pair))))
				 (the valid-neighbor-index-pairs))))

		   
   (nearby-buildings 
    (apply #'append 
	   (mapcar #'(lambda(section) (the-object section buildings-list))
		   (the neighbors))))))

		   
		   

