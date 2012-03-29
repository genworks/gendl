(in-package :genworks.com)


(define-object contact (base-site-sheet)
  
  :computed-slots ((body-class "contact"))
  
  :objects ((column-center :type 'sheet-section
			   :inner-html 
			   (with-cl-who-string ()
			     (:h2 "Contact Us")
			     (:dl (:dt "Telephone: ") (:dd "248-327-3253") 
				  :br  
				  ((:dt :class "email") "Email: ") 
				  ((:dd :class "email") "info@genworks.com"))))))

