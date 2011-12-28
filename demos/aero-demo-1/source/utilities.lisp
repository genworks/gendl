(in-package :aero)

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
(define-object p-relaxation (base-object)
          
  :input-slots ((curve-in )
                (relaxation-paramete)
                (number-of-sample-points 9 ))
         
  :computed-slots ((%curve-in (the curve-in)) 
                   
                   (total-length (the %curve-in total-length))
                   
                   (interval (/ (the total-length) (the number-of-sample-points)))
                   
                   (pi-factor (/ (degrees-to-radians 180)
                               ;;(the relaxation-parameter) 
                               (the number-of-sample-points)))
                   
                   (sample-points-list (list-elements (the sample-points) (the-element center))))
    :objects
 
    ((sample-points :type 'sample-point
                    :hidden? nil
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
