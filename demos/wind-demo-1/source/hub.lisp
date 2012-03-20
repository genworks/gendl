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
(define-object hub (base-object)
;------------------------------------------------------------------------------------------------------------------------ 
  :input-slots
   
  ((hub-cylinder-radius (getf (the input-data-list) :hub-radius) :settable) 
   (hub-cylinder-length (getf (the input-data-list) :hub-length) :settable)

   )
 
;-----------------------------------------------------------------------------------------------------------------------  
   :computed-slots
   
   ((data-directory *data-pathname*)
    (input-data-file (merge-pathnames "inputs-aero.dat" (the data-directory)))
    (input-data-list (with-open-file (in (the input-data-file)) (read in))))
;-----------------------------------------------------------------------------------------------------------------------
   :hidden-objects
   ((ucs-hub :type 'axes))
    
     
   :objects
   ((hub-cylinder :type 'cylinder
                  :hidden? t
                  :length (the hub-cylinder-length) 
                  :radius (the hub-cylinder-radius)            
                  :number-of-sections 150
                  :display-controls (list :color :green-yellow2))

    (hub-arc :type 'arc-curve
             :hidden? t
             :radius (the hub-cylinder-radius) 
             :start-angle 0
             :end-angle (half pi)
             :center  (translate 
                       (the center) 
                       :rear (* (the hub-cylinder-length) 0.5)))
    
    (hub-cylinder-line :type 'linear-curve
                       :hidden? t
                       :start (the hub-arc start )
                       :end (translate (the hub-arc start ):front (the hub-cylinder-length)))
    
    (composed :type 'composed-curve
              :hidden? t
              :curves (list (the hub-cylinder-line) (the hub-arc )))
    
    (normalized-curve :type 'normalized-curve 
                      :hidden? t
                      :curve-in (the composed)
                      :u-min 0.0
                      :u-max 1.0)
                     
    
    (hub-con :type 'smlib::revolved-surface-nlib ;;the smlib revolved-surface from GDL has at the moment memory leaks
             :display-controls (list :color :green-yellow2)
             :curve (the normalized-curve)
             :axis-point (the hub-arc :center)
             :axis-vector (the hub-arc (face-normal-vector :rear)))))
            ;;;:curve (the composed )
            ;;; :display-controls (list :color :silver :shininess 0.7 )
            ;;; :arc (* 4 (half pi))
            ;;;:axis-point  (the hub-arc :center)
            ;;; :axis-vector (the hub-arc (face-normal-vector :rear)))))

(in-package :smlib)

(define-object revolved-surface-nlib (surface)


:input-slots 
(curve axis-point axis-vector)
:computed-slots 
((native-surface (smlib::make-revolved-surface *geometry-kernel* 
                  :curve (the curve native-curve)
                  :axis-point (the axis-point)
                  :axis-vector (the axis-vector )))))
