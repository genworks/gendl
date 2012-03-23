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


(define-object rotor-assembly (base-object)

   ;;------------------------------------------------------------------------------------
   :input-slots
   ;; These slots are all given default values so the blade can be made by itself
   ;; as a standalone object (for testing).
   ;;---------------------------------------------------------------------------------
   ;; Internal inputs parameters
   ((fit-tolerance 0.025 )
    (nr-blades-data 3)
    (blade-concepts :classic-blade)
    (unite 10 :settable)
    ;;-------------------------------------------------------------------------------
    ;;This routine is used to locate and open the input file see also "parameters.lisp"
    (input-data-list (with-open-file (in (the input-data-file)) (read in)))
    (input-data-file (merge-pathnames "inputs-aero.dat" (the data-directory)))
    (xfoil-data-directory (merge-pathnames "xfoil\\" (the data-directory)))
    (data-directory *data-pathname*)
    ;;------------------------------------------------------------------------------
    ;;This routine is used to extract the data from the input file for flaps  
    (flaps-pitch-axis-offset   (getf (the input-data-list) :flaps-pitch-axis) :settable) 
    (flaps-rotor-radius-start (getf (the input-data-list) :flaps-rotor-radius-s) :settable) 
    (flaps-rotor-radius-end (getf (the input-data-list) :flaps-rotor-radius-e) :settable) 
    ;;------------------------------------------------------------------------------
    ;;This routine is used to extract the data from the input file 
    (chord-data (getf (getf (the input-data-list) :aero-section) :chord))  
    (twist-data (getf (getf (the input-data-list) :aero-section) :twist)) 
    (airfoil-data (getf (getf (the input-data-list) :aero-section) :airfoil))
    (rotor-radius-data (getf (getf (the input-data-list) :aero-section) :rotor-radius))
    (pitch-axis-data (getf (getf (the input-data-list) :aero-section) :pitch-axis) )
    (thickness-data (getf (getf (the input-data-list) :aero-section) :thickness) )
    (pitch-angle-data (getf (getf (the input-data-list) :aero-section) :pitch-angle) )
   ;;------------------------------------------------------------------------------
    ;;This routine is used to extract the data from the input file 
    (root-chord-data (getf (getf (the input-data-list) :root-section) :chord))  
    (root-twist-data (getf (getf (the input-data-list) :root-section) :twist)) 
    (root-airfoil-data (getf (getf (the input-data-list) :root-section) :airfoil))
    (root-rotor-radius-data (getf (getf (the input-data-list) :root-section) :rotor-radius))
    (root-pitch-axis-data (getf (getf (the input-data-list) :root-section) :pitch-axis) )
    (root-thickness-data (getf (getf (the input-data-list) :root-section) :thickness) )
    (root-pitch-angle-data (getf (getf (the input-data-list) :root-section) :pitch-angle) ))
  
  :objects
  ((blade :type 'rotor
          :hidden? nil
          :blade-concepts (the blade-concepts)
          :sequence (:size (the nr-blades-data))
          :blade-in (the rotor-hidded) ;;;;in blade-sectios
          :vector (make-vector 1.0 0.0 0.0)
          :x-rotation (* (the-child index) (/ 360 (the nr-blades-data))))
   
   (hub :type 'hub 
                :orientation (alignment :rear 
                                        (rotate-vector-d 
                                         (the (face-normal-vector :rear))
                                         90
                                         (the (face-normal-vector :top)))))
   )
 
  :hidden-objects

  ((rotor-hidded :type 'turbine-blade
               
                 :blade-concepts (the blade-concepts)
                 )
   (hub-hidded :type 'hub)))
   


(define-object rotor (base-object)
  
  :input-slots ((blade-in) (vector)  (x-rotation) (blade-concepts))
  :computed-slots (;;(surface-in (list-elements (the blade-in ))))
                    
                    (surface-in (if (eql (the blade-concepts) :classic-blade)
                        (list-elements (the blade-in ))
                      (list-elements (the blade-in blade-with-flaps )))))
                  
                  
  :objects
  ((section :type 'sur-gen-rot
            :sequence (:size (length (the surface-in)))
            :isos (list :n-u 7 :n-v 9)
            ;;;:display-controls (list :color :silver  :shininess 0.7 :line-thickness 1)
             :display-controls (the-object (nth (the-child index)(the surface-in)) 
                                             display-controls )
            :strings-for-display  (the-object (nth (the-child index)(the surface-in)) 
                                              strings-for-display)
            :surface (nth (the-child index)(the surface-in))
            :vector  (the vector)
            :point   (make-point 0 0 0)
            :degree  (the  x-rotation))))







   
   
   
   
   
   
   
   
   
   
   
   
