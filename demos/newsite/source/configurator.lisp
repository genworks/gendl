(in-package :www.genworks.com)

(define-object configurator (base-site-sheet)

  :computed-slots
  ((title "Genworks International - Product Configurator")
   (link-title  "Product Configurator")

   (current-price (* (the quantity value)
		     (+
		      (ecase (the license value)
			(:agpl 0)
			(:proprietary 3000))
		      (ecase (the lisp-engine value)
			(:none 0)
			(:sbcl 0)
			(:ccl 0)
			(:acl 5000)
			(:lw  3000))
		      (ecase (the support-level value)
			(:none 0)
			(:install 300)
			(:comprehensive 9000))
		      (ecase (the geometry-kernel value)
			(:basic 0)
			(:smlib 9000)))))
   
   (right-section-inner-html 
    (with-cl-who-string ()
      (:h2 "Configure your Download") ((:div :id "configurator"))

      (:p (:h1 (fmt "Price: $~$" (the current-price))))
      (with-html-form (:cl-who? t)
	(:fieldset
	 (str (the quantity html-string))
	 (str (the license html-string))
	 (str (the lisp-engine html-string))
	 (str (the support-level html-string))
	 (str (the geometry-kernel html-string)))))))


  :objects
  ((quantity :type 'text-form-control 
	     :size 1
	     :default 1
	     :prompt "Quantity"
	     :ajax-submit-on-change? t)

   (license :type 'menu-form-control
	    :size 1
	    :choice-plist (list :agpl "AGPL"
				:proprietary "Proprietary")
	    :default :agpl
	    :prompt "License"
	    :ajax-submit-on-change? t)

   (lisp-engine :type 'menu-form-control
		:size 1
		:choice-plist (list :none "None"
				    :sbcl "SBCL"
				    :ccl "Clozure CL"
				    :acl "Franz&reg; Allegro CL"
				    :lw  "LispWorks")
		:default :none
		:prompt "Common Lisp Engine"
		:ajax-submit-on-change? t)

   (support-level :type 'menu-form-control
		  :size 1
		  :choice-plist (list :none "None"
				      :install "Installation"
				      :comprehensive "Comprehensive")
		  :default :none
		  :ajax-submit-on-change? t)

   (geometry-kernel :type 'menu-form-control 
		    :size 1		
		    :choice-plist (list :basic "Basic"
					:smlib "SMLib from Solid Modeling Solutions&#8482;")
		    :default :basic
		    :ajax-submit-on-change? t)))

				     
