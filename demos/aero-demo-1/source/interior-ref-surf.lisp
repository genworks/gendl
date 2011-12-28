(in-package :aero)

 (define-object interior-ref-surf (surface-trimm) 
    
   :input-slots 
   ((layer-1 t :settable)
    (layer-2 t :settable)
    (layer-3 t :settable)
    (layer-4 t :settable)
    (layer-5 t :settable)
    (layer-6 nil :settable)
    (root-ribs-clearance 10  :settable) 
    (tip-ribs-clearance 5   :settable)
    (main-body-ribs-angle 15 :settable)
    (relaxation-parameters '(0.0 0.05 0.1 0.175 0.25 0.35 0.45 0.57 0.7 0.85 1.0)  :settable);; frome 0.0 to 1.0  
    )
   :computed-slots 
   ()
   :hidden-objects 
   ()
   :objects 
   ((spar-ref-isos :type 'iso-curve
                   :hidden? (the layer-5)
                   :sequence (:size 4)
                   :surface (the ref-surf)
                   :fixed-parameter :u
                   :parameter (nth (the-child index)
                                   (list 
                                    (* (first (the spars-position)) 0.5)
                                    (- 1 (* (first (the spars-position)) 0.5))
                                    (- 0.5 (* (- 1 (second (the spars-position))) 0.5))
                                    (+ 0.5 (* (- 1 (second (the spars-position))) 0.5)))))
    
    (te-ref-spar :type 'ruled-surface
                 :hidden? (the layer-6)
                 :curve-1 (the (spar-ref-isos 0))
                 :curve-2 (the (spar-ref-isos 1))
                 :display-controls (list :color :violet))
    
    (le-ref-spar :type 'ruled-surface
                 :hidden? (the layer-6)
                 :curve-1 (the (spar-ref-isos 2))
                 :curve-2 (the (spar-ref-isos 3))
                 :display-controls (list :color :violet))
    
    (ref-root-rib :type 'ruled-surface
                  :hidden? (the layer-6)
                  :curve-1 (first (list-elements (the (main-body 0 )(u-iso-curves ))))
                  :curve-2  (the-object (first  (list-elements (the (main-body 1)(u-iso-curves )))) reverse)
                  :display-controls (list :color :sienna))
    
    (ref-tip-rib :type 'ruled-surface
                 :hidden? (the layer-6)
                 :curve-1 (first (last (list-elements (the (main-body 2 )(u-iso-curves )))))
                 :curve-2  (the-object (first (last  (list-elements (the (main-body 3)(u-iso-curves ))))) reverse)
                 :display-controls (list :color :sienna))
    
    (root-clearance-point :type 'point 
                          :hidden? (the layer-5)
                          :center (translate-along-vector
                                  (the-object (first (list-elements (the te-ref-spar u-iso-curves ))) start)
                                  (subtract-vectors  (the-object (first (last (list-elements (the te-ref-spar u-iso-curves )))) start)
                                                     (the-object (first (list-elements (the te-ref-spar u-iso-curves ))) start))
                                                     
                                  (the root-ribs-clearance)))
    
    (tip-clearance-point :type 'point 
                         :hidden? (the layer-5)
                         :center (translate-along-vector
                                  (the-object (first (last (list-elements (the le-ref-spar u-iso-curves )))) start)
                                  (subtract-vectors  (the-object (first (list-elements (the le-ref-spar u-iso-curves ))) start)
                                                     (the-object (first (last (list-elements (the le-ref-spar u-iso-curves )))) start))
                                  (the tip-ribs-clearance)))
    (ribs-ref-axis :type 'linear-curve
                   :hidden? (the layer-5)
                   :start (the root-clearance-point center)
                   :end (the tip-clearance-point center))
    
    (plane-ref-points :type 'point 
                      :hidden? (the layer-5)
                      :sequence (:size (length (the relaxation-parameters ))) 
                      :center (the ribs-ref-axis
                                (point (nth (the-child index) 
                                            (the relaxation-parameters )))))
    
    (ribs-b-curves  :type 'planar-section-curve
                    :hidden? (the layer-5)
                        :sequence (:size (length (the relaxation-parameters ))) 
                        :surface (the ref-surf)
                        ;; It is required the *3d-tolerance-default* tolerance because 
                        ;; curve intersection-point up-stream it is based on higher tolerances. 
                        ;; I recommend for planar-intersection-cueve (s) tolerance to be set to
                        ;; This adds a lot of disadvantages like: !!! 1200 control-points !!!

                        ;;;:3d-approximation-tolerance *3d-tolerance-default*
                        :plane-normal (rotate-vector (make-vector 1.0 0.0 0.0)
                                                     (degrees-to-radians 
                                                      (- 90 (the main-body-ribs-angle)))
                                                     (make-vector 0.0 -1.0 0.0))
                        :plane-point (the ribs-ref-axis
                                       (point (nth (the-child index) 
                                                   (the relaxation-parameters ))))
                        :display-controls (list :color :red ))
    
    (ribs-bottom-b-curves :type 'trimmed-curve
                          :hidden? (the layer-5)
                       :sequence (:size (length (the relaxation-parameters ))) 
                       :built-from (the (ribs-b-curves (the-child index))) 
                       :u2 (get-parameter-of (the (ribs-b-curves (the-child index)) 
                                               (curve-intersection-point (the (spar-ref-isos 0))
                                                                         :distance-tolerance 
                                                                         *3d-approximation-tolerance-default*)))
                       :u1 (get-parameter-of (the (ribs-b-curves (the-child index)) 
                                               (curve-intersection-point (the (spar-ref-isos 2))
                                                                         :distance-tolerance 
                                                                         *3d-approximation-tolerance-default*)))
                       :display-controls (list :color :red :line-thickness 1.5))
    
    (ribs-top-curves :type 'trimmed-curve
                     :hidden? (the layer-5)
                          :sequence (:size (length (the relaxation-parameters ))) 
                          :built-from (the (ribs-b-curves (the-child index))) 
                          :u1 (get-parameter-of (the (ribs-b-curves (the-child index)) 
                                                  (curve-intersection-point (the (spar-ref-isos 1))
                                                                            :distance-tolerance 
                                                                            *3d-approximation-tolerance-default*)))
                          :u2 (get-parameter-of (the (ribs-b-curves (the-child index)) 
                                                  (curve-intersection-point (the (spar-ref-isos 3))
                                                                            :distance-tolerance 
                                                                            *3d-approximation-tolerance-default*)))
                          :display-controls (list :color :red :line-thickness 1.5))
    
    (interior-rib :type 'ruled-surface
                  :hidden? (the layer-6)
                  :sequence (:size (length (the relaxation-parameters ))) 
                  :curve-1 (the (ribs-bottom-b-curves (the-child index)))
                  :curve-2 (the (ribs-top-curves  (the-child index)) reverse )
                  :display-controls (list :color :orange))
    ))
                            
          
         
