;;; Fuselage example in GDL
;;; 2 Mar 09
;;; Cory Cooper
(in-package :gdl-user)


(define-object fuselage (base-object)
  
  :input-slots
  ((length 30)
   (width 10)
   (height 5)
      
   (diameter 5))
    
  
  :objects  
  ((refbox :type 'box)
   
   (nose    
    :type 'fuselage-cone  ;use this type because I defined it earlier?
    :length (* (the length) 0.15)	;smarter way to define taper ratio?
    :radius-1  (the diameter)
    :number-of-sections 25
    :center (translate (the (face-center :rear))
		       :front
		       (half (the-child length))))
   

   (hull    
    :type 'fuselage-cylinder 
    :length (- (the length) (the nose length) (the tail-cone length))
    :radius (the diameter)
    :center (translate (the nose (face-center :front))
		       :front (half (the-child length))))


   (tail-cone    
    :type 'fuselage-cone 
    :number-of-sections 25
    :length (* (the length) 0.25)
    :radius-1 0
    :radius-2 (the diameter)
    :center (translate (the (face-center :front))
		       :rear
		       (half (the-child length))))))


(define-object fuselage-cylinder (cylinder) ; create a basic cylinder
  :input-slots
  ((length 10)
   (radius 5)))


(define-object fuselage-cone (cone) ; create a cone 
  :input-slots  
  ((length 7)   
   (radius-1 5)   
   (radius-2 3)
   (number-of-sections 5)))


  



   
