(in-package :www.genworks.com)

(define-object downloads (base-site-sheet)

  :input-slots ((license-info *licensed-emails*))
 
  :computed-slots
  ((title "Genworks International - Downloads")
   (link-title  "Downloads")
   
   (right-section-js-to-eval "$j('#address').hide(200);$j('#tickete').show(200);")
   
   (right-section-inner-html
    (with-cl-who-string ()
      (:h2 "Downloads") ((:div :id "contact"))

      (:p "The Downloads section is not loaded into the site at this time.")))))
	
