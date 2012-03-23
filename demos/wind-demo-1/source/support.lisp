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

(define-object turbine-support (application-mixin)

  :input-slots 
  ((new-center)
   (support-data (getf (the input-data-list) :support-profile) :settable )
   (diameter-data (getf (the input-data-list) :diameter) :settable)
   (support-thickness-data (getf (the input-data-list) :support-thickness) :settable)
   (support-radius-data (getf (the input-data-list) :support-radius) :settable)  
   (support-axis-data (getf (the input-data-list) :support-axis) :settable) 
 
   )
  
  
  :computed-slots
  
  ((possible-nils (list :show-surfaces?))
   
   (data-directory *data-pathname*)
   (input-data-file (merge-pathnames "inputs-aero.dat" (the data-directory)))
    
   (input-data-list (with-open-file (in (the input-data-file)) (read in)))
   
   (ui-display-list-objects (if (the show-surfaces?) (list-elements (the transformed)) (the display-box)))
   
   )
  
  :objects
  ((test :type 'cone-surface
          :length 10
          :radius-1 5
          :inner-radius-1 3
          :radius-2 2
          :inner-radius-2 1)
   
   (tower :type 'cone-surface
          :length (car (last (the support-radius-data)))
          :radius-1 (first (the diameter-data))
          :inner-radius-1 (first (the diameter-data)) 
          :radius-2 (car (last (the diameter-data))) 
          :inner-radius-2 (car (last (the diameter-data))))                                  
  
  )
  
 :hidden-objects
  ((display-box :type 'box
                :center (translate (the center)
                                   :up
                                   (half (the height)))
                :length 6
                :width 6
                :height 87.6)))


(define-object cone-surface (surface)
  :input-slots 
  (length radius-1 inner-radius-1 radius-2 inner-radius-2)
  
  :computed-slots ((native-surface (the cone-surface native-surface)))
    
   :hidden-objects
  ((radius-1-arc :type 'arc-curve
                 :radius (half (the radius-1))
                 :center (make-point 0 0 0) 
                 :start-angle 0
                 :end-angle 2pi)
   
   (inner-radius-1-arc :type 'arc-curve
                       :radius (half (the inner-radius-1))
                       :center (make-point 0 0 0) 
                       :start-angle 0
                       :end-angle 2pi)
   
   (radius-2-arc :type 'arc-curve
                 :radius (half (the radius-2))
                 :center (make-point 0 0 (the length)) 
                 :start-angle 0
                 :end-angle 2pi)
   
   (inner-radius-2-arc :type 'arc-curve
                       :radius (half (the inner-radius-2))
                       :center (make-point 0 0 (the length)) 
                       :start-angle 0
                       :end-angle 2pi)
   
   (outer-surface :type 'ruled-surface
                  :curve-1 (the radius-1-arc)
                  :curve-2 (the radius-2-arc))
   
   (inner-surface :type 'ruled-surface
                  :curve-1 (the inner-radius-2-arc)
                  :curve-2 (the inner-radius-1-arc))
   
   (top-surface :type 'ruled-surface
                  :curve-1 (the radius-2-arc)
                  :curve-2 (the inner-radius-2-arc))
   
   (bottom-surface :type 'ruled-surface
                   :curve-1 (the inner-radius-1-arc)
                   :curve-2 (the radius-1-arc))
  
   (join-A :type 'joined-surfaces
           :surface (the outer-surface)
           :other-surface (the top-surface))
   
   (join-B :type 'joined-surfaces
           :surface (the join-A)
           :other-surface (the inner-surface))
   
   (cone-surface :type 'joined-surfaces
                 :surface (the bottom-surface)
                 :other-surface (the join-B))))


