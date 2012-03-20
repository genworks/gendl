(in-package :wind)


(define-object test-sur-gen-rot (base-object)
  :objects
  ((surface :type 'surf::test-b-spline-surface)
   
   
   (surface-1 :type 'rectangular-surface
           
            :length 10
            :width 1
            :height 0)
   
   (rotated-gen-surface :type 'sur-gen-rot
                        :hidden? nil
                        :sequence (:size 182)
                        :surface (the surface)
                        :point (the center)
                        :vector #(0.0 1.0 0.0)
                        :degree (* 1 (the-child index))) 
   
   (rotated-box :type 'boxed-surface
                :sequence (:size 182)
                :surface-in (the surface)
                :display-controls (list :color :purple)
                :orientation (alignment :left 
                                     (rotate-vector-d (the (face-normal-vector :left))
                                                      (the-child index)
                                                      (the (face-normal-vector :rear)))))))                
   

(define-object test-reversed-surface(base-object)
  :objects
  ((surface :type 'surf::test-b-spline-surface)
   
   (revers-surf :type 'reversed-surface
                     :surface (the surface)
                     :direction :u)))




(define-object sur-gen-rot (surface) 
  
  :input-slots 
  
  ((surface)(point)(vector) (degree))

  :hidden-objects ((surf-in :type 'surface 
                            :native-surface (smlib::surface-copy (the surface))))
                           ;;:native-surface (the surface fresh-copy native-surface)))

  :computed-slots
  
  ((native-surface (smlib::surface-general-rotation (the surf-in) (the point) (the vector) (the degree)))))


;;+#nil
(define-object reversed-surface(surface) 
  
  :input-slots 
  
  ((surface)(direction ))
  
  :computed-slots
  
  ((native-surface (smlib::surface-reverse (the surface ) (the direction))))) 


(define-object normalize-surface (surface) 
  
  :input-slots 
  
  ((surface)(u-min 0.0 )(u-max 1.0 ) 
  (v-min 0.0 )(v-max 1.0 ))


  :hidden-objects ((surf-in :type 'surface 
                            :native-surface (smlib::surface-copy (the surface))))
                            ;:native-surface (the surface fresh-copy native-surface)))

  :computed-slots
  
  ((native-surface (smlib::surface-knot-scale (the surf-in native-surface) 
                                              :u-min (the u-min )
                                              :u-max (the u-max ) 
                                              :v-min (the v-min )
                                              :v-max (the v-max )  
                                              ))))

(in-package :smlib)

(uffi:def-function ("N_AllocSrf" n_allocsrf) ((a stacks)) :returning (* nurbs-surface))
  
(defun surface-general-rotation (surface point vector degree)
  (let ((s (the-object surface native-surface))
        (c (make-point3d point))
        (v (make-point3d vector))
        (d (to-double-float degree)))
    (let ((flag (lisp_n_surrot s c v d)))
      (if (zerop flag)s
        (let ((error-type (e_errtyp)))
          (error "Failed to create nlib gordon surface with routine n_ascgor .~% Nlib error was: `(~a) ~a.'~%"
                 error-type  (error-string error-type)))))))

;;;#+nil
(defun surface-copy (surface)
  (let ((stacks (make-nlib-stacks))
        (surface-in (the-object surface native-surface)))
    (let ((surface-out  (make-nurbs-surface)))
      (let ((flag (n_srfcopy surface-in surface-out stacks)))
        (if (zerop flag) 
           (progn (add-stacks surface-out stacks) surface-out)
          (let ((error-type (e_errtyp)))
            (free-stacks stacks)
            (error "Failed to get isoparametric curve from surface with routine n_tooscx.~% The Nlib error was: `(~a) ~a.'~%"
                   error-type (error-string error-type))))))))


(defmethod surface-reverse (
       ;;;you have to add the geometry-kernel in the final implementation 
                            ;;(geometry-kernel geometry-kernel) 
                            surface direction )
  (let ((surface-in (the-object surface native-surface))
        (u-or-v (ecase direction (:u 1) (:v 2)))
        (reversed-surface (make-nurbs-surface))
        (stacks (make-nlib-stacks)))
    ;;(let ((reversed-surface (n_allocsrf stacks)))
      (let ((flag (n_surrev surface-in u-or-v  reversed-surface stacks)))
        (if (zerop flag)
            (progn (add-stacks reversed-surface stacks) reversed-surface)
          (let ((error-type (e_errtyp)))
            (free-stacks stacks)
            (error "Failed to make reversed curve with routine u_currev.~% The Nlib error was: `(~a) ~a.'~%"
                   error-type (error-string error-type)))))))


(defmethod surface-knot-scale (
                               ;;(geometry-kernel geometry-kernel) 
                               surface &key (u-min 0.0 )
                                            (u-max 1.0 ) 
                                            (v-min 0.0 )
                                            (v-max 1.0 ))
  
  (n_surkts surface 
            u-min u-max
            v-min v-max) surface)


;;;"N_SrfRotateAtPt"   
(uffi:def-function ("LISP_N_surrot" lisp_n_surrot) 
    ((a nurbs-surface) 
     (b (* point)) 
     (c (* x-vector)) 
     (d c-real))
  :returning flag)

;;;#+nil
(uffi:def-function ("N_SrfCopy" n_srfcopy) 
    ((a nurbs-surface) 
     (b nurbs-surface) 
     (c stacks)) 
  :returning flag)



(uffi:def-function ("N_SrfReverse" n_surrev) 
    ((a nurbs-surface) 
     (b flag) 
     (c nurbs-surface) 
     (d stacks)) :returning flag)

;;;it is not implemented
(uffi:def-function ("N_SrfReparam" n_surkts) 
((a nurbs-surface) 
 (b parameter) 
 (c parameter) 
 (d parameter) 
 (e parameter))
  :returning x-void) 
