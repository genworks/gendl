(in-package :www.genworks.com)

(define-object services (base-site-sheet)

  :computed-slots
  ((title "Genworks International - Services")
   (link-title  "Services")
   (right-section-inner-html
    (with-cl-who-string ()
      (:H2 "Our Services") ((:DIV :ID "welcome"))
      ((:DIV :ID "profile")
       ((:DIV :ID "peop") (:H3 "Value-added application support ")
	(:P
	 "One year Maintenance and Implementation Support (provided by KE-Works, up to 5 events), with the option to renew at any time for a 5 k&euro; fee.")
	(:H3 "Remote consultancy ")
	(:P "Service scheduled for release Q1 2012.")
	(:H3 "On-site consultancy ")
	(:P "Service scheduled for release Q1 2012.")))))))