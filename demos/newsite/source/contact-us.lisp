(in-package :www.genworks.com)

(define-object contact-us (base-site-sheet)

  :computed-slots
  ((title "Genworks International - Contact Us")
   (link-title  "Contact us")
   
   (right-section-inner-html
    (with-cl-who-string ()
      (:H2 "Contact us")
      ((:DIV :ID "welcome")
       ((:IMG :SRC "/newsite-static/images/pic_4.jpg" :ALT "Pic 1" :CLASS "left" :WIDTH "83"
	      :HEIGHT "129"))
       (:P "Address: Decebal 35B") (:P "City: Bistrita")
       (:P "Country: Romania") (:P "Tel: +40 36 340 5066")
       (:P "Email: info@ke-works.ro"))))))
