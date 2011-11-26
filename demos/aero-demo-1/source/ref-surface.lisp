(in-package :aero)

(define-object ref-surf (base-object)
  
  :input-slots 
  ((layer-1 t :settable)
   (layer-2 nil :settable)
   (data-directory *data-pathname*)
   (chord-data  '(220  80) :settable)
   (span-data   '(0.0  320) :settable)
   (sweep-angle '(0.0  -40) :settable)
   (twist-data  '(0.0  0.0) ));;;it is not added in this model up-stream 
   
  :computed-slots 
  ((sweep-data (* (tan (degrees-to-radians 
                        (the sweep-angle))) 
                       (the span-data))))
  
    :objects  
  ((transform-points 
    :type        'proces-pints
    :hidden?     (the layer-1)
    :sequence    (:size (length (the chord-data)))
    :points      (read-points-list 
                  (merge-pathnames "naca-0012.dat"
                                   (the data-directory)))
    :scale-x     (nth (the-child index) (the chord-data))
    :scale-y     (nth (the-child index) (the chord-data))
    :translate-x (* (tan (degrees-to-radians 
                          (nth (the-child index) (the sweep-angle)))) 
                    (nth (the-child index) (the span-data)))
    :translate-z (nth (the-child index) (the span-data))
    :points-data (the-child procesed-points) 
    :rotate      (nth (the-child index)(the twist-data )))
     
   (ref-surf :type 'fitted-surface
             :hidden? (the layer-2)
             :display-controls (list :color :green-spring)
             :v-degree (- (length (the chord-data)) 1)
             :points  (mapcar #' (lambda (x) (the-object x points-data))
                                  (list-elements (the transform-points))))

   ;;this approach has a tolerance error "E^-8"
   (end-cap-curve :type 'iso-curve
                  :hidden? (the layer-1)
                  :fixed-parameter :v
                  :parameter (the ref-surf v-max)
                  :surface (the ref-surf))
   
   (ecc-trimmed-curve :type  'trimmed-curve
                      :hidden? (the layer-1)
                      :built-from (the end-cap-curve)
                      :tolerance 0.0001
                      :u1 (the end-cap-curve u-min)
                      :u2 (* (the end-cap-curve u-max) 0.5))
   
   (end-cap-surf :type 'revolved-surface 
                 :hidden? (the layer-2)
                 ;;'smlib::revolved-surface-nlib ;;the smlib revolved-surface from GDL has at the moment memory leaks
                 :display-controls (list :color :green-yellow2)
                 :curve (the ecc-trimmed-curve)
                 :arc pi
                 :axis-point (the ecc-trimmed-curve start)
                 :axis-vector  (make-vector 1.0 0.0 0.0));;add the real vector that contains the point 
   
   ))
