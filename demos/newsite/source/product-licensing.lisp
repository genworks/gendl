(in-package :www.genworks.com)

(define-object product-licensing (base-site-sheet)

  :computed-slots
  ((title "Genworks International - Product Licensing")
   (link-title  "Licensing")

   (right-section-inner-html 
    (with-cl-who-string ()
      (:h2 "Product Licensing") ((:div :id "welcome"))
      "Our products are licensed in a variety of flexible ways"))))
