(in-package :www.genworks.com)

(define-object assembly (base-site-sheet)

  :computed-slots
  ((title "Genworks International - Welcome")
   (right-section-inner-html (the index-html right-section-inner-html))
   (link-title "Home")
   
   (pages (the children)))
  
  :trickle-down-slots (pages)

  :hidden-objects
  ((news :type 'news)

   (configurator :type 'configurator))

  :objects
  ((index-html :type 'index-html)
   (products :type 'products)
   (services :type 'services)
   (demos :type 'demos)
   (people :type 'people)
   (contact-us :type 'contact-us)))