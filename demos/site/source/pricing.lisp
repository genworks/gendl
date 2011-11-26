(in-package :genworks.com)


(define-object pricing (base-site-sheet)

  :computed-slots ((body-class "pricing"))
  
  :objects
  (

   (column-left 
    :type 'sheet-section
    :main-view nil)
   

   (column-right 
    :type 'sheet-section
    :main-view nil)
   
   
   (column-center
    :type 'sheet-section
    :main-view
    (with-cl-who-string ()
      ((:div :class "content pricing") 
       (:h2 "Pricing")
       (:h5 "GDL products offer a savings of " (:strong "up to 75% or more") " as compared with legacy KBE tools.") 
       (:h5 "We also offer an alternative 12- or 24-month Lease with Option to Purchase.")
       (:h5 "Please email us at " ((:a :href "mailto:info@genworks.com") "info@genworks.com") " for a specific quote."))))))
