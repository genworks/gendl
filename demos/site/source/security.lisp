(in-package :genworks.com)


(define-object security (base-site-sheet)

  :input-slots (pricing-footer)

  :computed-slots ((body-class "products security"))
  
  :objects
  (
   
   (column-left 
    :type 'sheet-section
    :main-view nil)
   
   (column-right 
    :type 'sheet-section
    :main-view nil)


   
   (column-center :type 'sheet-section
		  :main-view 
		  (with-cl-who-string ()
		    ((:div :class "content") 
		     (:h2 "Security") 
		     (:h5 "Source Code Escrow")
		     (:p "Genworks offers Enterprise customers the option of entering
into a Source Code Escrow agreement administered by "
			 ((:a :href "http://www.softescrow.com/") "Lincoln-Parry")
			 ", to protect your investment in GDL application 
development against all eventualities. Please contact us for details. ")
		     
		     )))))


