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

(define-object assembly (base-object)

  :input-slots
  (;;(blade-concepts :classic-blade)
   (blade-concepts :test)
   (azimuth-angle-data (getf (the input-data-list) :azimuth-angle) :settable) 
   (nr-blades-data (getf (the input-data-list) :blades-nr) :settable)    
   (hub-height-data (getf (the input-data-list) :hub-height) :settable)
   (hub-length-data (getf (the input-data-list) :hub-length) :settable)
   (hub-overhang-data (getf (the input-data-list) :hub-overhang) :settable)
   (nacelle-box-width (getf (the input-data-list) :nacelle-width) :settable)
   (nacelle-box-height  (getf (the input-data-list) :nacelle-height ) :settable)
   (nacelle-box-length (getf (the input-data-list) :nacelle-length) :settable)
   (nacelle-height-data (getf (the input-data-list) :nacelle-height-geo) :settable)
   (nacelle-overhang-data (getf (the input-data-list) :nacelle-overhang-geo) :settable)
   (precone-angle-data (getf (the input-data-list) :precone-angle):settable)
   (tilt-angle-data 0 :settable) ;; get from file
   (support-data (getf (the input-data-list) :support-profile) :settable )
   (diameter-data (getf (the input-data-list) :diameter) :settable)
   (support-thickness-data (getf (the input-data-list) :support-thickness) :settable)
   (support-radius-data (getf (the input-data-list) :support-radius) :settable)  
   (support-axis-data (getf (the input-data-list) :support-axis) :settable) 
   (pitch-angle-data (getf (the input-data-list) :pitch-angle) :settable))


  :trickle-down-slots (azimuth-angle-data 
                       nr-blades-data hub-height-data hub-overhang-data
                       nacelle-height-data nacelle-overhang-data
                       precone-angle-data tilt-angle-data
                       support-data diameter-data support-thickness-data
                       support-radius-data support-axis-data
                       show-surfaces? pitch-angle-data input-data-list)
  
  
  :objects
  
  ((rotor-assembly :type 'rotor-assembly
                   :blade-concepts (the blade-concepts) 
                   :nr-blades-data (the nr-blades-data))
                  
   
   (turbine-nacelle :type 'nacelle 
                     :orientation (alignment :rear 
                                        (rotate-vector-d 
                                         (the (face-normal-vector :rear))
                                         90
                                         (the (face-normal-vector :top))))
                    
                   
                    :center (translate (the center) 
                                       :right (+ (half (the-child box length)) 
                                                 (half (the hub-length-data)))))
  
   (tower :type 'boxed-surface
          :display-controls (list :color :green-yellow2)
          :surface-in  (the turbine-support tower outer-surface)
          :center (translate (the  center) 
                             :right (half (the nacelle-box-length))
                             :bottom (+  (half (the nacelle-box-height)) 
                                         (car (last (the support-radius-data))))
                             )))
  
  
  :computed-slots
  
  (
   #+nil
   (upload-data-file-name "" :settable)
    #+nil
   (iges-url (let ((url "/wind-assembly.igs"))
               (publish :path url
                        :function #'(lambda(req ent)
                                      (with-http-response(req ent)
                                        (with-http-body (req ent)
                                          (let ((stream (request-reply-stream req)))
                                            (with-format (iges stream)
                                              (print-variables stream)
                                              (write-the cad-output-tree)))))))
                url))
   
   
   (strings-for-display "UpWind 5MW Ref")
   
   (data-directory *data-pathname*)
   (input-data-file (merge-pathnames "inputs-aero.dat" (the data-directory))  :settable)
   (input-data-list (with-open-file (in (the input-data-file)) (read in))))
  
   :hidden-objects
   ((turbine-support :type 'turbine-support )))


  
