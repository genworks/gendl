;;----------------------------------------
;; -  Delft University of Technology     -
;; -  Teodor-Gelu CHICIUDEAN             -
;; -  PhD researcher                     -
;; -  Design, Integration and Operation  -
;; -  of Aircraft and Rotorcraft (DAR)   -
;; -  Faculty of Aerospace Engineering   -
;; -  Delft University of Technology     -
;; -  Kluyverweg 1 2629 HS Delft         -
;; -  Tel. : +31 (0)15 278 7158          -
;; -  Mob. : +31 (0)61 889 2495          -
;; -  e-mail : T.G.chiciudean@tudelft.nl -
;;----------------------------------------
(in-package :wind)



(define-object test (base-object)

  :objects
  ((test :type 'b-spline-surface 
         :display-controls (list :color :red)
         :control-points  (mapcar #'(lambda(list) (mapcar #'apply-make-point list))
                                  (list '((0 0 0)(4 1 0)(8 1 0)(10 0 0))
                                        '((0 0 2) (4 2 2) (8 2 2) (10 0 2) )
                                        '((0 0 4) (4 2 4) (8 2 4) (10 0 4) )
                                        '((0 0 7) (4 1 7) (8 1 7) (10 0 7) )))  
        
     ))) 
   
   
   
   
   
   
   
   
   
   
   
   
