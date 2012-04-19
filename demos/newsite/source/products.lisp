(in-package :www.genworks.com)

(define-object products (base-site-sheet)

  :computed-slots
  ((title "Genworks International - Products")
   (link-title  "Products")

   (right-section-inner-html 
    (with-cl-who-string ()
      (:h2 "Our Products") ((:div :id "welcome"))
      (:ul
       (:li ((:a :href (the descriptions url)) "Descriptions"))
       (:li ((:a :href (the licensing url)) "Licensing"))
       (:li ((:a :href (the configurator url)) "Configurator"))))))

  
  :objects
  ((descriptions :type 'product-descriptions
		 :pass-down (respondent))

   (licensing :type 'product-licensing
	      :pass-down (respondent))

   (configurator :type 'configurator
		 :pass-down (respondent))))

