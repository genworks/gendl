(in-package :www.genworks.com)

(defparameter *lang* :english)

(defparameter *locale-strings-hash*

  (let ((strings 
	 '(:gendl-component
	   (:english "Gendl Component"
	    :chinese "Gendl 成份")

	   :your-selection
	   (:english "Your Selection"
	    :chinese "选项")

	   :price
	   (:english "Price"
	    :chinese "价钱")

	   :total 
	   (:english "Total"
	    :chinese "总额")

	   
	   :previous
	   (:english "Previous"
	    :chinese "先前")

	   :next 
	   (:english "Next"
	    :chinese "下一项")

	   :gendl-licensing-level
	   (:english "Gendl Licensing Level"
	    :chinese "Gendl 软件级别")
	   
	   :open-source-agpl
	   (:english "Open Source (AGPL)"
	    :chinese "开源软件 (AGPL)")

	   :please-select-basic-geom
	   (:english "Please Select \"Basic\" Geometry Kernel to enable this option."
	    :chinese "请选择几何内核选项 ")

	   :evaluation
	   (:english "Evaluation"
	    :chinese "评估")

	   :student
	   (:english "Student"
	    :chinese "学生版")

	   :professional
	   (:english "Professional"
	    :chinese "专业版")

	   :enterprise
	   (:english "Enterprise"
	    :chinese "企业版")

	   :order-qualifies-for-academic
	   (:english "This Order Qualifies for Academic Pricing"
	    :chinese "此项学术定价资格")

	   :common-lisp-engine
	   (:english "Common Lisp Engine"
	    :chinese "Common Lisp Engine")

	   :to-enable-this-option
	   (:english "to enable this option"
	    :chinese "选此项")

	   :please-select
	   (:english "Please select"
	    :chinese "请选")

	   :the-basic-geometry-kernel
	   (:english "the \"Basic\" geometry kernel" ""
	    :chinese "基本几何内核" "")

	   :and
	   (:english "and"
	    :chinese "和")

	   :a-non-evaluation-gendl-license
	   (:english "a non-Evaluation Gendl license"
	    :chinese "无评估 Gendl 软件")
	   
	   :franz-allegro-cl-32
	   (:english "Franz Allegro CL&reg; 32-bit"
	    :chinese "Franz Allegro CL&reg; 32-bit")

	   :franz-allegro-cl-64
	   (:english "Franz Allegro CL&reg; 64-bit"
	    :chinese "Franz Allegro CL&reg; 64-bit")

	   :lispworks-32-basic
	   (:english "LispWorks 32-bit Basic"
	    :chinese "基本 LispWorks 32-bit")

	   :lispworks-32-expanded
	   (:english "LispWorks 32-bit Expanded"
	    :chinese "扩展LispWorks 32-bit")

	   :lispworks-64-expanded 
	   (:english "LispWorks 64-bit Expanded"
	    :chinese "扩展 LispWorks 64-bit")


	   :geometry-kernel
	   (:english "Geometry Kernel"
	    :chinese "几何内核")
	   
	   :basic
	   (:english "Basic"
	    :chinese "基本")
	   
	   :smlib
	   (:english "SMLib&reg;"
	    :chinese "SMLib&reg;")
	   
	   :non-agpl-gendl 
	   (:english "a non-AGPL Gendl license"
	    :chinese "无 AGPL Gendl 软件")

	   :commercial-cl-engine
	   (:english "a commercial CL engine"
	    :chinese "a commercial CL engine")
	   
	   :technical-support-level
	   (:english "Technical Support Level"
	    :chinese "技术售后服务")

	   :select
	   (:english "Select"
	    :chinese "选项")

	   :a 
	   (:english "a"
	    :chinese "a")

	   :commercial
	   (:english "commercial"
	    :chinese "商业")

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
	    :chinese "三天")

	   :ten-day 
	   (:english "Ten-day"
	    :chinese "十天")

	   :onsite 
	   (:english "Onsite"
	    :chinese "现场")

	   :excl-travel-and-exp
	   (:english "excl. travel and expenses"
	    :chinese "差")

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
       
       

