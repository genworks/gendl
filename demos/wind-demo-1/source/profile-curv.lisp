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

(define-object profile-curve (curve)
          
  :input-slots ((points)
                (curve-in )
                (tolerance)
                (number-of-sample-points 31)
                (fit? t))
         
  :computed-slots ((built-from (the fitted))

                   (%curve-in (cond ((the curve-in) (the curve-in))
                                    ((the points) (the fitted-in))
                                    (t (error "In profile-curve, you must specify either curve-in or points"))))
                   
                   (total-length (the %curve-in total-length))
                   
                   (interval (/ (the total-length) (the number-of-sample-points)))
                   
                   (pi-factor (/ (* 4 pi) (the number-of-sample-points)))
                   
                   (sample-points-list (list-elements (the sample-points) (the-element center))))
    :objects
 
   
    ((fitted-in :type (if (the points) 'curve-fitcae 'null-part)
                :tolerance (the tolerance)
                :points (the points))
    

   (fitted :type 'curve-fitcae
           :hidden? t
           :tolerance (the tolerance)
           :points (the sample-points-list))

           

   (sample-points :type 'sample-point
                  :sequence (:size (the number-of-sample-points))
                  :distance                     
                  (if (the-child first?) 
                      0
                    (if (the-child last?) (the total-length) 
                      (let ((increment
                             (* (the interval)
                                (- 1 
                                   (cos (* (the pi-factor)
                                           (the-child index)))))))
                        (+ increment (the-child previous distance)))))
                  :center (the %curve-in
                            (point (the %curve-in (parameter-at-length (the-child distance))))))))


(define-object sample-point (point)
  :input-slots (distance))


(in-package :wind)

(define-object curve-fitcae (curve)  
  :input-slots ((tolerance) 
                (points)) 
  :computed-slots

    ((native-curve  (smlib::fit-cae (the points) :tolerance (the tolerance)))))

(in-package :smlib)
  
(defun fit-cae (points &key (tolerance))
  
  (let* ((length (1- (length points)))
         (curve (make-nurbs-curve))
         (stacks (make-nlib-stacks))
         (point-array (make-arraypoint3d points stacks (1+ length))))
      (let ((flag (n_fitcae point-array length 1 1 3 tolerance 1 curve stacks)))
      (if (zerop flag)
          (progn (add-stacks curve stacks)
                 curve)
        (let ((error-type (e_errtyp)))
          (free-stacks stacks)
          
          (error "Failed to make curve with routine n_fitcae.~% The Nlib error was: `(~a) ~a.'~%"
                 error-type (error-string error-type)))))))


(uffi:def-function ("N_FitCrvApprox" n_fitcae)
    ((points-array point)
     (highest-index index) 
     (closed? flag) ;; 0 or 1
     (start-degree x-degree) 
     (result-degree x-degree) 
     (tolerance c-real) 
     (knot-multiplicity flag)  ;; 1 or 2
     (h nurbs-curve) 
     (i stacks))
  :returning flag)
