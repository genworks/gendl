(in-package :genworks.com)


(define-object index (base-site-sheet)
  
  :computed-slots
  
  ((title "Genworks International")
   (body-class "home"))

  
  :objects
  ((column-center :type 'sheet-section
		  :inner-html
		  (with-cl-who-string ()
		    ((:p :id "genworks_title") 
		     ((:img :src "/site-static/images/icons/Genworks_title.gif" :alt "Genworks")))
		    (:p
		     "is a premier developer and vendor for a Generative Application Development system used for 
creating web-centric Knowledge-based Engineering and Business applications.")
		    (:p "Our groundbreaking software sets new standards for ease and speed of development 
and seamless web-based deployment for geometry-intensive, engineering, and business solutions.")
		    (:p "Genworks GDL software comes in a variety of configurations based on your needs. Check out our "
			((:a :href (the products url) :title "Products") "Products") " page for available packages and licenses.")
		    (:p "Our Open Architecture allows great flexibility for connecting / integrating with other software applications.")
		    ((:p :class "footnote") "Check out a demo or click on a link to learn more.")))))

