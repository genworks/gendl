;;--piston-assembly.lisp--

(in-package :shock-absorber)

(define-object piston-assembly (base-object)

  :input-slots (piston-length piston-radius rot-length piston-y-position )
  
  :computed-slots
  ((rot-center (make-point 0 (+ (get-y (the piston center )) 
				(/ (+ (the piston-length) 
				      (the rot-length)) 2)) 0)))
  :objects	       
  ((piston :type 'cylinder
	   :center (make-point 0 (the piston-y-position ) 0)
	   :radius (the piston-radius)  
	   :length (the piston-length ))
   
   (rod :type 'cylinder
	:center (the rot-center)
	:radius 2.5
	:length (the rot-length))))
