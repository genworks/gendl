;;-- assembly.lisp--
;; this is a new version of the file assembly.lisp 

(in-package :shock-absorber)

(define-object  assembly (base-object)
  :input-slots ()
  :trickle-down-slots ()
  :computed-slots ()
  :objects 
  ((pressure-tube :type 'cone
                  :center (make-point 0 70 0)
                  :length 120
                  :radius-1 13
                  :inner-radius-1 12
                  :radius-2 13
                  :inner-radius-2 12)

   (tube-cap :type 'cone
             :center (make-point 0 5 0)
             :length 10
             :radius-1 5
             :inner-radius-1 0
             :radius-2 13
             :inner-radius-2 0)

   (seal-cap :type 'cone
             :center (make-point 0 135 0)
             :length 10
             :radius-1 13
             :inner-radius-1 2.5
             :radius-2 5
             :inner-radius-2 2.5)
   
   (floating-piston :type 'cylinder
                    :center (make-point 0 35 0)
                    :radius 12
                    :length 10)
   
   (blocking-ring :type 'cone
                  :center (make-point 0 42.5 0)
                  :length 5
                  :radius-1 12
                  :inner-radius-1 10
                  :radius-2 12
                  :inner-radius-2 10)
   
   (piston :type 'cylinder
           :center (make-point 0 125 0)
           :radius 12
           :length 10)
   
   (rod :type 'cylinder
        :center (make-point 0 175 0)
        :radius 2.5
        :length 90))
  
  :hidden-objects ()
  
  :functions ()
  
  :methods ())
