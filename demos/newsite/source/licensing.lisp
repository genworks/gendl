(in-package :www.genworks.com)

(define-object product-licensing (base-site-sheet)

  :computed-slots
  ((title "Genworks International - Product Descriptions")
   (link-title  "Descriptions")

   (right-section-inner-html 
    (with-cl-who-string ()
      (:h2 "Product Descriptions") ((:div :id "welcome"))
      (:ul
       (:li ((:a :href (the descriptions url)) "Descriptions"))
       (:li ((:a :href (the licensing url)) "Licensing"))
       (:li ((:a :href (the configurator url)) "Configurator")))))))

