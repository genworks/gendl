(in-package :www.genworks.com)

(defparameter *lang* :english)

(defparameter *locale-strings-hash*

  (let ((strings 
	 '(:gendl-component
	   (:english "Gendl Component"
	    :chinese "开 按")

	   :your-selection
	   (:english "Your Selection"
	    :chinese "Your Selection")

	   :price
	   (:english "Price"
	    :chinese "Price")

	   :total 
	   (:english "Total"
	    :chinese "Total")

	   
	   :previous
	   (:english "Previous"
	    :chinese "Previous")

	   :next 
	   (:english "Next"
	    :chinese "Next")

	   :gendl-licensing-level
	   (:english "Gendl Licensing Level"
	    :chinese "Gendl Licensing Level")
	   
	   :open-source-agpl
	   (:english "Open Source (AGPL)"
	    :chinese "Open Source (AGPL)")

	   :please-select-basic-geom
	   (:english "Please Select \"Basic\" Geometry Kernel to enable this option."
	    :chinese "Please Select \"Basic\" Geometry Kernel to enable this option.")

	   :evaluation
	   (:english "Evaluation"
	    :chinese "Evaluation")

	   :student
	   (:english "Student"
	    :chinese "Student")

	   :professional
	   (:english "Professional"
	    :chinese "Professional")

	   :enterprise
	   (:english "Enterprise"
	    :chinese "Enterprise")

	   :order-qualifies-for-academic
	   (:english "This Order Qualifies for Academic Pricing"
	    :chinese "This Order Qualifies for Academic Pricing")

	   :common-lisp-engine
	   (:english "Common Lisp Engine"
	    :chinese "Common Lisp Engine")

	   :to-enable-this-option
	   (:english "to enable this option"
	    :chinese "to enable this option")

	   :please-select
	   (:english "Please select"
	    :chinese "Please select")

	   :the-basic-geometry-kernel
	   (:english "the \"Basic\" geometry kernel" ""
	    :chinese "the \"Basic\" geometry kernel" "")

	   :and
	   (:english "and"
	    :chinese "and")

	   :a-non-evaluation-gendl-license
	   (:english "a non-Evaluation Gendl license"
	    :chinese "a non-Evaluation Gendl license")
	   
	   :franz-allegro-cl-32
	   (:english "Franz Allegro CL&reg; 32-bit"
	    :chinese "Franz Allegro CL&reg; 32-bit")

	   :franz-allegro-cl-64
	   (:english "Franz Allegro CL&reg; 64-bit"
	    :chinese "Franz Allegro CL&reg; 64-bit")

	   :lispworks-32-basic
	   (:english "LispWorks 32-bit Basic"
	    :chinese "LispWorks 32-bit Basic")

	   :lispworks-32-expanded
	   (:english "LispWorks 32-bit Expanded"
	    :chinese "LispWorks 32-bit Expanded")

	   :lispworks-64-expanded 
	   (:english "LispWorks 64-bit Expanded"
	    :chinese "LispWorks 64-bit Expanded")


	   :geometry-kernel
	   (:english "Geometry Kernel"
	    :chinese "Geometry Kernel")
	   
	   :basic
	   (:english "Basic"
	    :chinese "Basic")
	   
	   :smlib
	   (:english "SMLib&reg;"
	    :chinese "SMLib&reg;")
	   
	   :non-agpl-gendl 
	   (:english "a non-AGPL Gendl license"
	    :chinese "a non-AGPL Gendl license")

	   :commercial-cl-engine
	   (:english "a commercial CL engine"
	    :chinese "a commercial CL engine")
	   
	   :technical-support-level
	   (:english "Technical Support Level"
	    :chinese "Technical Support Level")

	   :select
	   (:english "Select"
	    :chinese "Select")

	   :a 
	   (:english "a"
	    :chinese "a")

	   :commercial
	   (:english "commercial"
	    :chinese "commercial")

	   :to-reduce-tech-support-prices
	   (:english " to reduce Technical Support prices."
	    :chinese " to reduce Technical Support prices.")

	   :none
	   (:english "None"
	    :chinese "None")

	   :i-e
	   (:english "i.e."
	    :chinese "i.e.")

	   :self-provided-or-third-party
	   (:english "self-provided or third-party"
	    :chinese "self-provided or third-party")

	   :installation-and-configuration
	   (:english "Installation and Configuration"
	    :chinese "Installation and Configuration")

	   :non-trial
	   (:english "non-Trial"
	    :chinese "non-Trial")

	   :non-student
	   (:english "non-Student"
	    :chinese "non-Student")
	   

	   :technical-how-to-q-a
	   (:english "Technical how-to questions/answers"
	    :chinese "Technical how-to questions/answers")

	   :application-code-nda
	   (:english "Application Code Nondisclosure"
	    :chinese "Application Code Nondisclosure")

	   :mission-critical-prod-env
	   (:english "Mission-critical Production Environment"
	    :chinese "Mission-critical Production Environment")

	   :an-open-source
	   (:english "an Open Source"
	    :chinese "an Open Source")

	   :or 
	   (:english "or"
	    :chinese "or")

	   :enterprise-class
	   (:english "Enterprise-class"
	    :chinese "Enterprise-class")


	   :gendl-license
	   (:english "Gendl license"
	    :chinese "Gendl license")


	   :training-level
	   (:english "Training Level"
	    :chinese "Training Level")

	   :self-guided-etc
	   (:english "self-guided, online videos &amp; tutorials, or third-party"
	    :chinese "self-guided, online videos &amp; tutorials, or third-party")
	    
	   :remote 
	   (:english "Remote"
	    :chinese "Remote")

	   :three-day
	   (:english "Three-day"
	    :chinese "Three-day")

	   :ten-day 
	   (:english "Ten-day"
	    :chinese "Ten-day")

	   :onsite 
	   (:english "Onsite"
	    :chinese "Onsite")

	   :excl-travel-and-exp
	   (:english "excl. travel and expenses"
	    :chinese "excl. travel and expenses")

	   :language-selection 
	   (:english "Language Selection"
	    :chinese "Language Selection")

	   :language 
	   (:english "Language"
	    :chinese "Language")
	   
	   :tip
	   (:english "TIP"
	    :chinese "TIP")

	   )))


    (let ((ht (make-hash-table :size (length strings))))
      (mapc #'(lambda(key value) (setf (gethash key ht) value))
	    (plist-keys strings) (plist-values strings)) ht)))

(defmacro locale-string (key) 
  `(let ((string (getf (gethash ,key *locale-strings-hash*) (the lang))))
     (or string 
	 (progn (warn "~s not found in *locale-strings-hash* for current *lang*: ~a"
		      ,key (the lang))
		(format nil "!! ~s !!" ,key)))))
       
       

