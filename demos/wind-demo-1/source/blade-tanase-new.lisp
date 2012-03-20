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

(defun read-points-list (file-name)
  (with-open-file (in file-name)
    (let (result)
      (do ((line (read-line in nil nil) (read-line in nil nil)))
          ((or (null line) (string-equal line "")) (nreverse result))
        (let ((xyz-list (read-from-string (string-append "(" line ")"))))
          (when (and (= (length xyz-list) 3)
                     (every #'numberp xyz-list))
            (push xyz-list result)))))))


(define-object proces-pints (base-object)
  :input-slots
  ((points-data)
   (points)
   (scale-x)(scale-y)(scale-z)
   (translate-x)(translate-z)
   (rotate))
  :computed-slots
  ((scale-points (mapcar #'apply-make-point (mapcar #'(lambda(x)(list (* (first x)(the scale-x))(* (second x) (the scale-y))(third x)))(the points))))
   (tranzlate-x (mapcar #'(lambda (point) (translate-along-vector point #(-1.0 0.0 0.0) (the translate-x))) (the scale-points)))
   (tranzlate-z (mapcar #'(lambda (point) (translate-along-vector point #(0.0 0.0 1.0) (the translate-z))) (the tranzlate-x)))
   (procesed-points  (mapcar #'(lambda (point) (rotate-point point #(0.0 0.0 0.0) #(0.0 0.0 1.0) :angle (degree (the rotate)))) (the tranzlate-z)))
   ))

;;------------------------------------------------------------------------------------
(define-object turbine-blade (base-object)
  ;;------------------------------------------------------------------------------------
   :input-slots
   ;; These slots are all given default values so the blade can be made by itself
   ;; as a standalone object (for testing).
   ;;---------------------------------------------------------------------------------
   ;; Internal inputs parameters
   ((blade-concepts)
    (fit-tolerance 0.025 )
    (unite 10 :settable)
    ;;-------------------------------------------------------------------------------
    ;;This routine is used to locate and open the input file see also "parameters.lisp"
    (input-data-list (with-open-file (in (the input-data-file)) (read in)))
    (input-data-file (merge-pathnames "inputs-aero.dat" (the data-directory)))
    (xfoil-data-directory (merge-pathnames "xfoil\\" (the data-directory)))
    (data-directory *data-pathname*)
    ;;------------------------------------------------------------------------------
    ;;This routine is used to extract the data from the input file for flaps 
    (flaps-pitch-axis (list 1.0 0.8 0.5):settable)
   (flaps-rotor-radius-start (list 30.0 41.5 50.0 ):settable)
   (flaps-rotor-radius-end (list 40.0 48.5 56.0):settable)
    
    
    
    ;;(flaps-pitch-axis-offset   (getf (the input-data-list) :flaps-pitch-axis) :settable) 
    ;;(flaps-rotor-radius-start (getf (the input-data-list) :flaps-rotor-radius-s) :settable) 
   ;; (flaps-rotor-radius-end (getf (the input-data-list) :flaps-rotor-radius-e) :settable) 
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
   
   ;;------------------------------------------------------------------------------
   :computed-slots
   ((points-data-file (mapcar #'(lambda(x) 
                                  (merge-pathnames (format nil "~a.dat" x)
                                                   (the data-directory)))
                              (the airfoil-data)))
    (root-points-data-file (mapcar #'(lambda(x) 
                                  (merge-pathnames (format nil "~a.dat" x)
                                                   (the data-directory)))
                              (the root-airfoil-data)))
    (points-list (mapcar #'(lambda(x) (read-points-list x)) (the points-data-file)))
    (root-points-list (mapcar #'(lambda(x) (read-points-list x)) (the root-points-data-file)))
    (nr-airfoils (length (the airfoil-data)))
    (root-nr-airfoils (length (the root-airfoil-data)))
    (IGES-model (with-format (iges (merge-pathnames "IGES-model.igs" (the data-directory)))
                  (write-the cad-output-tree)) :uncached)
    
    (VRML-model (with-format  (vrml (merge-pathnames "VRML-model.wrl" (the data-directory)))
                  (write-the  cad-output-tree)) :uncached))
  
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   :objects
;;;;--------------------------------blade-aero-surface-up--------------------------
    ((transform-points-up 
     :type        'proces-pints
     :sequence    (:size (the nr-airfoils))
     :hidden?     t
     :points      (member (list 0.0 0.0 0.0) (nth (the-child index) (the points-list)) 
                          :test #'equalp) 
     :scale-x     (nth (the-child index) (the chord-data))
     :scale-y     (* (/ (nth (the-child index)(the thickness-data)) 100) 
                     (nth (the-child index) (the chord-data)))
     :translate-x (* (nth (the-child index)(the chord-data)) 
                     (nth (the-child index)(the pitch-axis-data)));;pozitive
     :translate-z (nth (the-child index)(the rotor-radius-data)) ;;;pozitive
     :points-data (the-child procesed-points) 
     :rotate      (nth (the-child index)(the twist-data )))
    
     (profiles-up-in :type 'profile-curve 
                    :hidden? t
                    :tolerance 0.0001
                    :sequence (:size (the nr-airfoils))
                    :points (the (transform-points-up (the-child index)) points-data ))
    
    (aero-surface-up :type 'fitted-surface
                     :hidden? t
                     :display-controls (list :color  :green-yellow2 )
                     :isos (list :n-u 11 :n-v 5)
                     :points (mapcar #'(lambda (points) (the-object points sample-points-list))
                                     (list-elements (the profiles-up-in))))
   
;;;;--------------------------------blade-aero-surface-down--------------------------    
    
    (transform-points-down :type 'proces-pints
                           :hidden? t
                           :sequence (:size (the nr-airfoils))
                           :points (member (list 0.0 0.0 0.0) (reverse (nth (the-child index)(the points-list))) :test #'equalp) 
                           :scale-x   (nth (the-child index) (the chord-data))
                           :scale-y   (* (/ (nth (the-child index)(the thickness-data)) 100) 
                                         (nth (the-child index) (the chord-data)))
                           :translate-x (* (nth (the-child index)(the chord-data)) 
                                           (nth (the-child index)(the pitch-axis-data)));;pozitive
                           :translate-z (nth (the-child index)(the rotor-radius-data)) ;;;pozitive
                           :points-data (the-child procesed-points) 
                           :rotate (nth (the-child index)(the twist-data )))
    
      (profiles-down-in :type 'profile-curve 
                        :hidden? t
                        :tolerance 0.0001
                        :sequence (:size (the nr-airfoils))
                        :points (the (transform-points-down (the-child index)) points-data))
    
    (aero-surface-down :type 'fitted-surface
                       :hidden? t
                       :display-controls (list :color  :green-yellow2 )
                       :isos (list :n-u 11 :n-v 5)
                       :points (mapcar #'(lambda (points) (the-object points sample-points-list))
                                       (list-elements (the profiles-down-in))))


;;;;--------------------------------blade-root-surface-up--------------------------
     (transform-root-points-up 
     :type        'proces-pints
     :sequence    (:size (the root-nr-airfoils))
     :hidden?     t
     :points      (member (list 0.0 0.0 0.0) (nth (the-child index) (the root-points-list)) 
                          :test #'equalp) 
     :scale-x     (nth (the-child index) (the root-chord-data))
     :scale-y     (* (/ (nth (the-child index)(the root-thickness-data)) 100) 
                     (nth (the-child index) (the root-chord-data)))
     :translate-x (* (nth (the-child index)(the root-chord-data)) 
                     (nth (the-child index)(the root-pitch-axis-data)));;pozitive
     :translate-z (nth (the-child index)(the root-rotor-radius-data)) ;;;pozitive
     :points-data (the-child procesed-points) 
     :rotate      (nth (the-child index)(the root-twist-data )))
    
    (profiles-root-up-in :type 'profile-curve 
                    :hidden? t
                    :tolerance 0.0001
                    :sequence (:size (the root-nr-airfoils))
                    :points (the (transform-root-points-up (the-child index)) points-data ))
    
    (root-surface-up :type 'fitted-surface
                     :hidden? t
                     :display-controls (list :color  :green-yellow2 )
                     :isos (list :n-u 3 :n-v 5)
                     :points (mapcar #'(lambda (points) (the-object points sample-points-list))
                                     (list-elements (the profiles-root-up-in))))
    
    
;;;;--------------------------------blade-root-surface-down--------------------------
     (transform-root-points-down 
     :type        'proces-pints
     :sequence    (:size (the root-nr-airfoils))
     :hidden?     t
     :points (member (list 0.0 0.0 0.0) (reverse (nth (the-child index) (the root-points-list))) :test #'equalp) 
     :scale-x     (nth (the-child index) (the root-chord-data))
     :scale-y     (* (/ (nth (the-child index)(the root-thickness-data)) 100) 
                     (nth (the-child index) (the root-chord-data)))
     :translate-x (* (nth (the-child index)(the root-chord-data)) 
                     (nth (the-child index)(the root-pitch-axis-data)));;pozitive
     :translate-z (nth (the-child index)(the root-rotor-radius-data)) ;;;pozitive
     :points-data (the-child procesed-points) 
     :rotate      (nth (the-child index)(the root-twist-data )))
    
    (profiles-root-down-in :type 'profile-curve 
                    :hidden? t
                    :tolerance 0.0001
                    :sequence (:size (the root-nr-airfoils))
                    :points (the (transform-root-points-down (the-child index)) points-data ))
    
    (root-surface-down :type 'fitted-surface
                       :hidden? t
                       :display-controls (list :color  :green-yellow2 )
                       :isos (list :n-u 3 :n-v 5)
                       :points (mapcar #'(lambda (points) (the-object points sample-points-list))
                                       (list-elements (the profiles-root-down-in))))
    
    (root-iso-curve :type 'iso-curve
                    :hidden? t
                    :sequence (:size 2)
                    :surface (ecase (the-child index) 
                               (0 (the root-surface-up))(1 (the root-surface-down)))
                    :parameter 1.0
                    :u-or-v :u)
    
    (aero-iso-curve :type 'iso-curve
                    :hidden? t
                    :sequence (:size 2)
                    :surface (ecase (the-child index) 
                               (0 (the aero-surface-up))(1 (the aero-surface-down )))
                    :parameter 0.0
                    :u-or-v :u)
    
    (tranzition-surface-down :type 'edge-blend-surface-wind
                             :hidden? t
                             :isos (list :n-u 4 :n-v 5)
                             :display-controls (list :color :green-yellow2)
                             :curve-side-1 :left-side
                             :curve-side-2 :right-side
                             :curve-2 (the (root-iso-curve 1))
                             :surface-2 (the root-surface-up)
                             :curve-1 (the (aero-iso-curve 1)) 
                             :surface-1 (the aero-surface-up))
    
    (tranzition-surface-up :type 'edge-blend-surface-wind
                           :hidden? t
                           :isos (list :n-u 4 :n-v 5)
                           :display-controls (list :color :green-yellow2)
                           :curve-side-1 :left-side
                           :curve-side-2 :right-side
                           :curve-2 (the (root-iso-curve 0))
                           :surface-2 (the root-surface-up)
                           :curve-1 (the (aero-iso-curve 0)) 
                           :surface-1 (the aero-surface-up))
    
    (compatible-blade-surface-up :type 'compatible-surfaces
                                 :hidden? t
                                 :display-controls (list :color :green-yellow2)
                                 :surface-list (list (the root-surface-up )
                                                     (the tranzition-surface-up) 
                                                     (the aero-surface-up)))
    
    (compatible-blade-surface-down :type 'compatible-surfaces
                                  :hidden? t
                                   :display-controls (list :color :green-yellow2)
                                   :surface-list (list (the root-surface-down )
                                                       (the tranzition-surface-down) 
                                                       (the aero-surface-down)))
    
    (join-r-t-up :type 'joined-surfaces
                 :hidden? t
                 :tolerance 0.1
                 :direction :v
                 :display-controls (list :color :green-yellow2)
                 :surface (the compatible-blade-surface-up (surfaces 0) )
                 :other-surface  (the compatible-blade-surface-up (surfaces 1)))
    
    (join-r-t-down :type 'joined-surfaces
                   :hidden? t
                   :tolerance 0.1
                   :direction :v
                   :display-controls (list :color :green-yellow2)
                   :surface (the compatible-blade-surface-down (surfaces 0) )
                   :other-surface  (the compatible-blade-surface-down (surfaces 1)))
    
    (blade-up-crud  :type 'joined-surfaces
                    :hidden? t
                    :tolerance 0.1
                    :direction :v
                    :display-controls (list :color :green-yellow2)
                    :surface (the join-r-t-up)
                    :other-surface  (the compatible-blade-surface-up (surfaces 2)))
    
    (blade-down-crud :type 'joined-surfaces
                     :hidden? t
                     :tolerance 0.1
                     :direction :v
                     :display-controls (list :color :green-yellow2)
                     :surface (the join-r-t-down)
                     :other-surface  (the compatible-blade-surface-down(surfaces 2)))

     (blade-down-u-r :type 'reversed-surface
                     :hidden? t
                     :surface (the blade-down-crud)
                     :direction :u)
     
     (blade-down :type 'surface-knot-reduction
                 :hidden? t
                 :display-controls (list :color :green-yellow2)
                 :surface (the blade-down-crud))
     
     
     
     (blade-up :type 'surface-knot-reduction
               :hidden? t
               :display-controls (list :color :green-yellow2)
               :surface (the blade-up-crud))
     
     
     (blade-joined :type 'joined-surfaces
                   ;;:hidden? (if (eql (the blade-concepts) :classic-blade) nil t)
                   :hidden? t
                   :tolerance 0.0001
                   :direction :u
                   :display-controls (list :color :green-yellow2)
                   :surface (the blade-down-u-r)
                   :other-surface(the blade-up-crud))
     
     (blade :type 'normalize-surface
            :hidden? t
            :surface (the blade-joined ))
     
     (blade-for-display  :type 'surface-knot-reduction
                         :hidden? (if (eql (the blade-concepts) :classic-blade) nil t)
                         :display-controls (list :color :green-yellow2)
                         :surface (the blade-joined ))
     
     (blade-with-flaps :type 'flaps-blade
                       ;;;:hidden? nil
                       :hidden? (if (eql (the blade-concepts) :blade-with-flaps) nil t)
                       :blade  (the blade)
                       :flaps-pitch-axis (the flaps-pitch-axis)
                       :flaps-rotor-radius-start (the flaps-rotor-radius-start)
                       :flaps-rotor-radius-end (the flaps-rotor-radius-end))
     
   
     
     )) 

   

   
   
   
   
   
   
   
   
   
