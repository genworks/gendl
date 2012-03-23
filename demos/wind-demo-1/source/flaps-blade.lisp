1;----------------------------------------
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
;;------------------------------------------------------------------------------------------------------------------------
(define-object flaps-blade (base-object)
  ;;------------------------------------------------------------------------------------------------------------------------ 
  :input-slots
  (blade flaps-pitch-axis flaps-rotor-radius-start flaps-rotor-radius-end)
  :computed-slots ()
  :hidden-objects ()
  :objects
  
    ((stiff-section :type 'split-surface
           :display-controls (list :color :green :line-thickness 1)
           :surface-in (the blade)
           :u-or-v :v
           :parameter (get-y (get-uv-point-of 
                              (the blade (projected-point 
                                           (make-point 0.0 0.0 (first (the flaps-rotor-radius-start))) 
                                           (make-vector 0.0 1.0 0.0))))))

     (tip-section :type 'split-surface
                  :display-controls (list :color :green :line-thickness 1)
                  :surface-in (the blade)
                  :keep-side  :right
                  :u-or-v :v
                  :parameter (get-y (get-uv-point-of 
                                     (the blade (projected-point 
                                           (make-point 0.0 0.0 (car (last (the flaps-rotor-radius-end)))) 
                                           (make-vector 0.0 1.0 0.0))))))
     
     
     (split-f1 :type 'split-surface
               :sequence (:size (-  (length (the flaps-rotor-radius-start)) 1))
               :hidden? t
               :display-controls (list :color :red :line-thickness 1)
               :surface-in (the blade)
               :keep-side  :right
               :u-or-v :v
               :parameter (get-y (get-uv-point-of 
                                   (the blade (projected-point 
                                               (make-point 0.0 0.0 (nth (the-child index) (the flaps-rotor-radius-end))) 
                                               (make-vector 0.0 1.0 0.0))))))
     
     (inter-section :type 'split-surface
                    :hidden? nil
                    :sequence (:size (- (length (the flaps-rotor-radius-start)) 1))
                    :display-controls (list :color :green :line-thickness 1)
                    :surface-in (the (split-f1 (the-child index)))
                    :keep-side  :left
                    :u-or-v :v
                    :parameter (get-y (get-uv-point-of 
                                   (the blade (projected-point 
                                               (make-point 0.0 0.0 (nth (+ 1(the-child index)) (the flaps-rotor-radius-start))) 
                                               (make-vector 0.0 1.0 0.0))))))

     
     (split-f0 :type 'split-surface
                :sequence (:size (length (the flaps-rotor-radius-start)))
                :hidden? t
                :display-controls (list :color :red :line-thickness 1)
                :surface-in (the blade)
                :keep-side  :right
                :u-or-v :v
                :parameter (get-y (get-uv-point-of 
                                   (the blade (projected-point 
                                               (make-point 0.0 0.0 (nth (the-child index) (the flaps-rotor-radius-start))) 
                                               (make-vector 0.0 1.0 0.0))))))
     
     (flap-ref-surf :type 'split-surface
                    :hidden? t
                    :sequence (:size (length (the flaps-rotor-radius-start)))
                    :display-controls (list :color :red :line-thickness 1)
                    :surface-in (the (split-f0 (the-child index)))
                    :keep-side  :left
                    :u-or-v :v
                    :parameter (get-y (get-uv-point-of 
                                   (the blade (projected-point 
                                               (make-point 0.0 0.0 (nth (the-child index) (the flaps-rotor-radius-end))) 
                                               (make-vector 0.0 1.0 0.0))))))
                       
     (flap-x1 :type 'split-surface
              :hidden? t
              :sequence (:size (length (the flaps-rotor-radius-start)))
              :display-controls (list :color :red :line-thickness 1)
              :surface-in (the (flap-ref-surf (the-child index)))
              :keep-side  :left
              :u-or-v :u
              :parameter (get-x (get-uv-point-of (the blade  (projected-point
                                                              (make-point (nth (the-child index) (the flaps-pitch-axis)) 
                                                                          0.0 
                                                                          (nth (the-child index) (the flaps-rotor-radius-end))) 
                                                              (make-vector 0.0 1.0 0.0))))))

     (flaps-surf-b :type 'split-surface
                   :sequence (:size (length (the flaps-rotor-radius-start)))
                   :display-controls (list :color :green :line-thickness 1)
                   :surface-in (the (flap-x1 (the-child index)))
                   :keep-side  :right
                   :u-or-v :u
                   :parameter(get-x (get-uv-point-of (the blade (projected-point
                                                                 (make-point (nth (the-child index) (the flaps-pitch-axis)) 
                                                                             0.0 
                                                                             (nth (the-child index) (the flaps-rotor-radius-end))) 
                                                                 (make-vector 0.0 -1.0 0.0))))))
     
     (flaps-surf-up :type 'split-surface
                    :sequence (:size (length (the flaps-rotor-radius-start)))
                    :display-controls (list :color :red :line-thickness 1)
                    :surface-in (the (flap-ref-surf (the-child index)))
                    :keep-side  :left
                    :u-or-v :u
                    :parameter(get-x (get-uv-point-of (the blade (projected-point
                                                                  (make-point (nth (the-child index) (the flaps-pitch-axis)) 
                                                                              0.0 
                                                                              (nth (the-child index) (the flaps-rotor-radius-end))) 
                                                                  (make-vector 0.0 -1.0 0.0))))))
     
     (flaps-surf-down :type 'split-surface
                      :sequence (:size (length (the flaps-rotor-radius-start)))
                      :display-controls (list :color :red :line-thickness 1)
                      :surface-in (the (flap-ref-surf (the-child index)))
                      :keep-side  :right
                      :u-or-v :u
                      :parameter(get-x (get-uv-point-of (the blade (projected-point
                                                                    (make-point (nth (the-child index) (the flaps-pitch-axis)) 
                                                                                0.0 
                                                                                (nth (the-child index) (the flaps-rotor-radius-end))) 
                                                                    (make-vector 0.0 1.0 0.0))))))
    
     
     ))


