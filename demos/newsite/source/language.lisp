(in-package :www.genworks.com)


(define-object language (base-site-sheet)


  :computed-slots
  ((title (locale-string :language-selection))
   (link-title (locale-string :language))

   (right-section-inner-html 
    (with-cl-who-string ()
      (:h2 (str (locale-string :language-selection)))
      ((:div :id "contact"))
      
      (:p "Please select your desired language for the site. Note that site
localization is an ongoing process, and some sections remain English-only at this moment.")
      
      (:fieldset
       (str (the choice html-string ))))))


  :objects
  ((choice :type 'radio-form-control 
	   :description-position :table-row-prepend
	   :default :english
	   :ajax-submit-on-change? t
	   :choice-plist 
	   (list :english (with-cl-who-string ()
			    ((:img :src "/newsite-static/images/british-flag.jpg" :width 150))
			    " "
			    (when (eql (the lang) :english)
			      (htm ((:img :src "/newsite-static/images/green-checkmark.jpg" :width 80)))))
		 :chinese (with-cl-who-string ()
			    ((:img :src "/newsite-static/images/china-flag.gif" :width 150))
			    " "
			    (when (eql (the lang) :chinese)
			      (htm ((:img :src "/newsite-static/images/green-checkmark.jpg" :width 80)))))))))
