(in-package :genworks.com)


(define-object maintenance (base-site-sheet)

  :input-slots (pricing-footer)
  
  
  :computed-slots ((body-class "products maintenance"))
  
  :objects
  (

   (column-left 
    :type 'sheet-section
    :inner-html nil)

   (column-right 
    :type 'sheet-section
    :inner-html nil)

   
   (column-center :type 'sheet-section
		  :inner-html
		  (with-cl-who-string ()
		    ((:div :class "content") 
		     (:h2 "GDL Maintenance and Support")
		     (:h5
		      "GDL/GWL Professional and Enterprise Development product customers are entitled to one year of 
Maintenance and Support, with the option to renew annually for a percentage-based fee.")
		     (:h5 "Maintenance and Support includes the following:")
		     (:p
		      "Reasonable consultation, via telephone, electronic mail or other writing on specific 
problems that arise in the installation and use of GDL. Telephone support is generally available on U.S. business 
days from 9am to 12 midnight Eastern (Michigan) Time.")
		     (:p
		      "Comprehensive, 24-hour per day, online web access to download patch updates (approximately 
monthly) and software version updates (as they are released).")
		     (:p "All major product upgrades that are released on CD-ROM during the Maintenance and Support period.")
		     (:p "Software Defect logging and tracking through Genworks' email-based bug tracking system.")
		     (:p
		      "Web access to the Genworks software updates database (e.g. detailed enhancement list for 
each new version and other on-line services as available from time to time.")
		     (:h2 "On Site Training")
		     (:p
		      "Subject to the availability of personnel and payment of Genworks' standard consulting 
and/or training fees, as well as travel and living expenses. Genworks will also offer project-specific and 
on-site technical support and/or training as reasonably requested by a customer. In addition, members of the 
Genworks VAR network are also generally available to provide similar services.")
		     (:h2 "Long-term Viability of Your GDL/GWL Investment")
		     (:p
		      "Genworks is fully dedicated to long-term support, maintenance, and enhancement of our 
products, in order to ensure our customers' continued success in using them. This focused vision and commitment 
to information longevity represents \"what we stand for\" as a company.")
		     
		     )))))
