(in-package :www.genworks.com)

(define-object product-licensing (base-site-sheet)

  :computed-slots
  ((title "Genworks International - Product Licensing")
   (link-title  "Licensing")

   (right-section-js-to-eval "$j('#all-go').hide(200);$j('#product-image').show(200);")

   (right-section-inner-html 
    (the configurator gendl-license explanation inner-html)
    #+nil
    (with-cl-who-string ()
      (:h2 "Product Descriptions") ((:div :id "welcome"))
      (:ul
       (:li ((:a :href (the descriptions url)) "Descriptions"))
       (:li ((:a :href (the licensing url)) "Licensing"))
       (:li ((:a :href (the configurator url)) "Configurator")))))))

