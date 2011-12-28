(in-package :aero)

(define-object material-design (base-object)
   ;;Note this is based on the surfaces --> control-grid-u --> global-polyline --> vertex-list.
  :input-slots 
  ((layer-1 t :settable)
   (layer-2 t :settable)
   (layer-3 t :settable)
   (layer-4 t :settable)
   (layer-5 t :settable)
   (layer-6 t :settable)
   (layer-m-1 t :settable)
   (layer-m-2 nil :settable))
   
   
  :computed-slots 
  
  ()
  
  :objects  
  
  ((m-ref-surf :type 'interior-ref-surf
               :hidden? (the layer-m-1)
               :layer-4 nil
               :layer-6 nil)
   
   (test :type 'material-panel
         :hidden? (the layer-m-2) 
         :sequence (:size (length (list-elements (the m-ref-surf))))
         :surface-in (nth (the-child index)(list-elements (the m-ref-surf)))
         :strings-for-display (the-child surface-in strings-for-display)
         :display-controls (the-child surface-in display-controls))))
          
  
   (define-object material-panel (planar-surface)

     :input-slots (surface-in)
     :computed-slots ((p00 (the surface-in (point (the surface-in u-min)(the surface-in v-min))))
                      (p01 (the surface-in (point (the surface-in u-min)(the surface-in v-max))))
                      (p10 (the surface-in (point (the surface-in u-max)(the surface-in v-min))))
                      (p11 (the surface-in (point (the surface-in u-max)(the surface-in v-max))))))
