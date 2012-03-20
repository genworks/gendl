;----------------------------------------
; -  Delft University of Technology     -
; -  Teodor-Gelu CHICIUDEAN             -
; -  PhD researcher                     -
; -  Design, Integration and Operation  -
; -  of Aircraft and Rotorcraft (DAR)   -
; -  Faculty of Aerospace Engineering   -
; -  Delft University of Technology     -
; -  Kluyverweg 1 2629 HS Delft         -
; -  Tel. : +31 (0)15 278 7158          -
; -  Mob. : +31 (0) 618892495           -
; -  e-mail : T.G.chiciudean@tudelft.nl -
;----------------------------------------

(in-package :wind)
;------------------------------------------------------------------------------------------------------------------------
(define-object nacelle (gwl:application-mixin)
;------------------------------------------------------------------------------------------------------------------------ 
  :input-slots
   
  ((nacelle-box-width (getf (the input-data-list) :nacelle-width) :settable)
   (nacelle-box-height  (getf (the input-data-list) :nacelle-height ) :settable)
   (nacelle-box-length (getf (the input-data-list) :nacelle-length) :settable))
 
;-----------------------------------------------------------------------------------------------------------------------  
   :computed-slots
   
   (

    
    (data-directory *data-pathname*)
    (input-data-file (merge-pathnames "inputs-aero.dat" (the data-directory)))
    
    (input-data-list (with-open-file (in (the input-data-file)) (read in))))
;-----------------------------------------------------------------------------------------------------------------------
   :objects
   ((box :type 'box-solid
         :hidden? t
         :width (the nacelle-box-width)
         :height (the nacelle-box-height)          
         :length (the nacelle-box-length)
         :display-controls (list :color :silver :shininess 0.7 ))
    
    (nacelle :type 'box-solid
             :display-controls (list :color :green-yellow2)
             :hidden? nil
             :width (the nacelle-box-width)
             :height (the nacelle-box-height)          
             :length (the nacelle-box-length))
     
             
    #+nil ;;;;moment memory leaks
    (nacelle :type 'blended-solid
             :display-controls (list :color :blue)
             :default-radius 1
             :brep (the box))
    
    ))   
              

