(in-package :www.genworks.com)

(define-object news (base-site-sheet)

  :computed-slots
  ((title "Genworks International - News")
   (link-title  "News")

   (right-section-inner-html 
    (with-cl-who-string ()
      (:h2 "Prior News") 
      (:H3 "2012-03-01")
      (:p "Genworks GDL (GenDL)included as a standard dist with " ((:a :href "http://www.quicklisp.org") "Quicklisp") ".")
      ((:div :class "hr-dots")) 
      (:h3 "2012-02-01")
      (:p "Port of core Genworks GDL (GenDL) to " ((:a :href "http://www.sbcl.org") "Steel Bank Common Lisp") " is completed.")
      (:h3 "2012-01-01")
      (:p "Genworks GDL 1581 (GenDL) Stable Beta release available, available on "
	  ((:a :href "http://www.franz.com/products/allegro") "Allegro CL 8.2") " and "
	  ((:a :href "http://www.lispworks.com/news/news31.html") "LispWorks 6.1"))
      (:h3 "2011-10-24")
      (:p ((:a :href "http://en.wikipedia.org/wiki/John_McCarthy_(computer_scientist)") "John McCarthy")
	  ", father of Lisp programming language family, exits the planet.")
      (:h3 "2011-10-23")
      (:p "Open-sourcing of GDL (GenDL) under "
	  ((:a :href "http://www.gnu.org/licenses/agpl-3.0.html") "Gnu Affero General Public License")
	  " is announced at "
	  ((:a :href "http://blip.tv/eclm") "ECLM 2011") ".")))))
