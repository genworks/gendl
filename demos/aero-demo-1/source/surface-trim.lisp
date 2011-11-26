(in-package :aero )

 (define-object surface-trimm (ref-surf) 
    
     :input-slots 
     ((layer-1 t :settable)
      (layer-2 t :settable)
      (layer-3 t :settable)
      (layer-4 nil :settable)
      ;; !!! Flag !!!
      ;;the profile is symmetric "naca-0012" in this case it is possible to consider the 
      ;;chord parameterization 1/2 from the surface parameterization 
      (spars-position '(0.2 0.9) :settable)
      ;; !!! Flag !!
      ;;!!! Flag !!! 
      ;;at the moment GDL doesn't have the capability 
      ;;to change the number of children in the tree and update the view accordingly
      (elevator-span 300)) ;;;:settable))
     ;;!!! Flag !!! 
      
     :computed-slots 
     ((sub-surface-list (list 
                             (the (surfaces-uuu 0)) 
                             (the (surfaces-uuuu 0))
                             (the (surfaces-uuuu 1))
                             (the (surfaces-tuuu 0))
                             (the (surfaces-tuuuu 0))
                             (the (surfaces-tuuuu 1)))))
     :hidden-objects 
     ((split-1 
       :type 'split-surface
       :hidden? (the layer-3)
       :u-or-v :v 
       :display-controls (list :color :red :line-thickness 1) 
       :surface-in (the ref-surf)
       :projection-point (make-point 0.0 0.0 (the elevator-span))
       :projection-vector (make-vector 1.0 0.0 0.0))

      (surfaces-v
       :type 'surface
       :hidden? (the layer-3)
       :sequence (:size 2)
       :native-surface  (nth (the-child index)(the split-1  %native-surfaces%)))
      
         (split-2 
          :type 'split-surface
          :hidden? (the layer-3)
           :u-or-v :u 
           :display-controls (list :color :red :line-thickness 1)
           :surface-in (the (surfaces-v 0) )
           :parameter (* (first (the spars-position)) 0.5))
         
      (surfaces-u
       :type 'surface
       :sequence (:size 2)
       :hidden? (the layer-3)
       :native-surface  (nth (the-child index)(the split-2  %native-surfaces%)))
       (split-3 
           :type 'split-surface
           :u-or-v :u 
           :display-controls (list :color :red :line-thickness 1)
           :surface-in (the (surfaces-u 1) )
           :parameter (- 1 (* (first (the spars-position)) 0.5)))
      
      (surfaces-uu
       :type 'surface
       :sequence (:size 2)
       :hidden? (the layer-3)
       :native-surface  (nth (the-child index)(the split-3  %native-surfaces%)))
      
      (split-4 
       :type 'split-surface
       :hidden? (the layer-3)
           :u-or-v :u 
           :display-controls (list :color :red :line-thickness 1)
           :surface-in (the (surfaces-uu 0) )
           :parameter (- 0.5 (* (- 1 (second (the spars-position))) 0.5)))
      
      (surfaces-uuu
       :type 'surface
       :hidden? (the layer-3)
       :sequence (:size 2)
       :native-surface  (nth (the-child index)(the split-4  %native-surfaces%)))
      
      (split-5 
       :type 'split-surface
       :hidden? (the layer-3)
           :u-or-v :u 
           :display-controls (list :color :red :line-thickness 1)
           :surface-in (the (surfaces-uuu 1) )
           :parameter (+ 0.5 (* (- 1 (second (the spars-position))) 0.5)))
      
      (surfaces-uuuu
       :type 'surface
       :hidden? (the layer-3)
       :sequence (:size 2)
       :native-surface  (nth (the-child index)(the split-5  %native-surfaces%)))
      
      (split-t4 
       :type 'split-surface
       :hidden? (the layer-3)
           :u-or-v :u 
           :display-controls (list :color :red :line-thickness 1)
           :surface-in (the (surfaces-v 1) )
           :parameter (- 0.5 (* (- 1 (second (the spars-position))) 0.5)))
      (surfaces-tuuu
       :type 'surface
       :hidden? (the layer-3)
       :sequence (:size 2)
       :native-surface  (nth (the-child index)(the split-t4  %native-surfaces%)))
      
      (split-t5 
       :type 'split-surface
       :hidden? (the layer-3) 
       :u-or-v :u 
           :display-controls (list :color :red :line-thickness 1)
           :surface-in (the (surfaces-tuuu 1) )
           :parameter (+ 0.5 (* (- 1 (second (the spars-position))) 0.5)))
     
      (surfaces-tuuuu
       :type 'surface
       :hidden? (the layer-3)
       :sequence (:size 2)
       :native-surface  (nth (the-child index)(the split-t5  %native-surfaces%))))

     :objects 
     
      ((flaps-surf
      
        :type 'surface
        :hidden? (the layer-4)
       :sequence (:size 2)
        :display-controls (list :color :red )
       :built-from (nth (the-child index)
                        (list (the (surfaces-u 0)) (the (surfaces-uu 1)))))
      (main-body 
       :type 'surface
         :hidden? (the layer-4)
       :sequence (:size 4)
       :display-controls (list :color :green )
       :built-from (nth (the-child index) (list (the (surfaces-uuu 0)) 
                             
                             (the (surfaces-uuuu 1))
                             (the (surfaces-tuuu 0))
                           
                             (the (surfaces-tuuuu 1)))))
      (te-surface   
       :type 'surface
       :sequence (:size 2)
         :hidden? (the layer-4)
       :display-controls (list :color :blue)
       :built-from (nth (the-child index) (list  
                             (the (surfaces-uuuu 0))
                             (the (surfaces-tuuuu 0))
                          )))))
                            
          
         
