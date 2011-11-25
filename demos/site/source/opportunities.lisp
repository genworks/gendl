(in-package :genworks.com)


(define-object opportunities (base-site-sheet)

  :computed-slots ((body-class "opportunities"))
  
  :objects
  ((column-center 
    :type 'sheet-section
    :main-view
    (with-cl-who-string ()
      
      (:h2 "Value Added Reseller (VAR) Arrangements")
      (:p
       "As an alternative to employment, consider the entrepreneurial approach of becoming a Genworks VAR. 
The start-up investment is surprisingly low. As a VAR you would:")
      (:ol (:li "be provided with one (or more) GDL/GWL development seats")
	   (:li "full access to Genworks' worldwide technical support resources") 
	   (:li "training as needed, and")
	   (:li
	    "the unimpeded license to design, sell, and support your own runtime applications using 
this ever-expanding 21st century technology."))
      (:p "Start-up cost and maintenance charges will be provided to interested parties on request.")
      ((:p :class "contact_info") "Contact us here regarding VAR opportunities, or give us a call at: +1 248-327-3253")
      (:h2 "Employment")
      (:p
       "Genworks periodically adds personnel. An applicant should have a solid academic Computer Science background. 
Relevant practical KB experience would be taken into consideration in the absence of a CS or EE degree. 
Experience with Common Lisp or another Generative KB System would be a major plus. In addition to a current 
resume, you should include an accompanying letter in which you succinctly describe your experience in a 
narrative form, together with a specific discussion of what about Genworks and GDL/GWL has piqued your 
interest.")
      ((:p :class "contact_mail") "Email your resume to info@genworks.com, or send a hardcopy to:" 
				  :br 
				  " Genworks International" 
				  :br
				  " 255 E. Brown Street, Ste 310 " 
				  :br 
				  " Birmingham, MI 48009 U.S.A.")))))
