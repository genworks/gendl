(in-package :www.genworks.com)

(defparameter *lang* :english)

(defmacro locale-string (key) 
  `(let ((string (getf (gethash ,key *locale-strings-hash*) (the lang))))
     (or string 
	 (progn (warn "~s not found in *locale-strings-hash* for current *lang*: ~a"
		      ,key (the lang))
		(format nil "!! ~s !!" ,key)))))

(defparameter *locale-strings-hash*

  (let ((strings 
	 `(

	   :gendl-component
	   (:english "Gendl Component"
	    :chinese "Gendl 组成部份")

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
	    :chinese "前一页")

	   :next 
	   (:english "Next"
	    :chinese "下一页")

	   :gendl-licensing-level
	   (:english "Gendl Licensing Level"
	    :chinese "Gendl 软件级别")
	   
	   :open-source-agpl
	   (:english "Open Source (AGPL)"
	    :chinese "开源软件 (AGPL)")

	   :please-select-basic-geom
	   (:english "Please Select \"Basic\" Geometry Kernel to enable this option."
	    :chinese "请选择基本几何内核选项 ")

	   :evaluation
	   (:english "Evaluation"
	    :chinese "试用软件")

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
	    :chinese "此项具备学术定价资格")

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
	   (:english "the \"Basic\" geometry kernel"
	    :chinese "基本几何内核")

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
	    :chinese "无")

	   :i-e
	   (:english "i.e."
	    :chinese "例如")

	   :self-provided-or-third-party
	   (:english "self-provided or third-party"
	    :chinese "自方或者第三方")

	   :installation-and-configuration
	   (:english "Installation and Configuration"
	    :chinese "安装和配置")

	   :non-trial
	   (:english "non-Trial"
	    :chinese "非试用")

	   :non-student
	   (:english "non-Student"
	    :chinese "非学生")
	   

	   :technical-how-to-q-a
	   (:english "Technical how-to questions/answers"
	    :chinese "技术问答")

	   :application-code-nda
	   (:english "Application Code Nondisclosure"
	    :chinese "Application Code Nondisclosure")

	   :mission-critical-prod-env
	   (:english "Mission-critical Production Environment"
	    :chinese "Mission-critical Production Environment")

	   :an-open-source
	   (:english "an Open Source"
	    :chinese "开源软件")

	   :or 
	   (:english "or"
	    :chinese "或者")

	   :enterprise-class
	   (:english "Enterprise-class"
	    :chinese "企业级")


	   :gendl-license
	   (:english "Gendl license"
	    :chinese "Gendl 软件")
	   
	   :less-than
	   (:english "less than"
	    :chinese "less than")
	   

	   :training-level
	   (:english "Training Level"
	    :chinese "培训等级")

	   :self-guided-etc
	   (:english "self-guided, online videos &amp; tutorials, or third-party"
	    :chinese "自学, 网上视频; 教程, 或者是第三方")
	    
	   :remote 
	   (:english "Remote"
	    :chinese "远程教学")

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
	    :chinese "差及其它费用")

	   :language-selection 
	   (:english "Language Selection"
	    :chinese "语言选项")

	   :language 
	   (:english "Language"
	    :chinese "语言")
	   
	   :tip
	   (:english "TIP"
	    :chinese "TIP")

	   :other-than
	   (:english "other than"
	    :chinese "other than")
	   
	   :known
	   (:english "known"
	    :chinese "known")
	   


	   :licensing-explained
	   (:english "Licensing Levels Explained"
	    :chinese "软件级别详解")

	   :genworks-gendl-is-so-called
	   (:english "Genworks Gendl is so-called"
	    :chinese "Genworks Gendl 是所称的")

	   :dual-licensed
	   (:english "dual-licensed"
	    :chinese "双轨制")

	   :software-available-under-an-open-source-license
	   (:english "software, available under an open-source license"
	    :chinese "软件，即提供 AGPL开源软件模式")


	   :and-alternatively
	   (:english "and alternatively under various proprietary
                  licenses (described below) some of which allow you
                  to distribute your application and derivative code
                  and keep it as closed-source, proprietary (Trade
                  Secret) software."
	    :chinese "与此同时用户可选择性地使用多样的私人授权方式（下面有详细叙述），用户可以发行个人的应用程序和衍生代码，保持一种非AGPL商业授权模式。保护您的专利权和商业密秘，也就是享受闭源软件的保护。")
	   
	   :open-source
	   (:english "Open Source"
	    :chinese "自由软件")

	   :open-source-allows-freedom
	   (:english "The Open Source (\"free software\") license allows
                  your application to enjoy complete freedom. This
                  license follows the philosophy of \"share and share
                  alike.\" By choosing this license (or by using Gendl
                  and distributing a compiled application without
                  making any choice of license), you agree to
                  distribute your application source code under the "
	    :chinese "(“自由软件”）的开源许可证允许您的应用程序享受完全的自由。但是您要遵循APGL许可协议。在此许可协议的授权方式下，人们可以从Genworks 网站自己获得开放源代码软件资源，并允许自由修改，自由发布")

	   :or-a-compatible-license
	   (:english "or a compatible license."
	    :chinese "或相应的版本。")

	   :no-restrictions
	   (:english "There are no restrictions against Commercial Use
                  with the Open Source license; the main requirement
                  is that you offer to share your application code at
                  no cost, just as the Gendl code has been shared with
                  you."
	    :chinese "我们没有对Gendl开源软件加以商业用途的限制; 但在向市场发布前，必须将自己修改的部分源代码及时返还回
                      社区，而且没有任何付加成本。 就象我们已与您共享的Gendl源代码一样。")

	   :code-must-remain-free
	   (:english "Gendl application or utility code developed with an
                  Open Source-licensed installation must always remain
                  as Free Software; the Proprietary licenses in
                  general do not allow for code originally developed
                  on an Open Source seat to be converted into a
                  closed-source proprietary application after the
                  fact, using a proprietary Gendl license."
	    :chinese "开源协议授权下安装的gendl应用程序或开放的实用程序代码必须始终保持为自由软件; 使用专有Gendl许可证，
                      私人授权不可以将在开源授权方式下开放的源代码转换为一个闭源授权的应用程序。")

	   :opportunity-to-overcome-limitation
	   (:english "There is an opportunity to overcome this limitation,
                 however: If you have Gendl application or utility
                 code which was developed using an Open Source
                 installation, and you wish to include or build upon
                 this code in a proprietary/closed-source application,
                 you may:"
	    :chinese "下面几点可以不束服这个限制，如果你有利用开源授权安装开发的Gendl应用程序或实用程序代码，你希望包括
                      或在源代码基础上建立一个专有/闭源应用此代码，你可以：")
	   
	   :contribute-to-gendl
	   (:english "Contribute the code to the Gendl project
		    under our standard "
	    :chinese "根据我们的标准所提供的协议，发行代码到Gendl项目")

	   :contributors-agreement
	   (:english "Contributor's Agreement"
	    :chinese "与Genworks分")
	   
	   :sharing
	   (:english "sharing with Genworks the copyright to any
		    original work. After suitable review, Genworks may
		    choose to include the code either in the mainline
		    Gendl codebase, or in a \"contrib\" directory"
	    :chinese "享任何原创作品的版权。经过适当的审查，Genworks可以选择包括在主线Gendl的codebase，或在“分享”目录")

	   :purchase-appropriate-license
	   (:english "Purchase an appropriate proprietary Gendl license"
	    :chinese "购买一个适当的的专有Gendl许证")

	   :freely-include 
	   (:english "Freely include the contributed code in your
		    distributed closed-source applications."
	    :chinese "在您的闭源应用程序中可以自由包括贡献代码。")
	   
	   :the
	   (:english "the"
	    :chinese "")
	   
	   :evaluation
	   (:english "Evaluation"
	    :chinese "评估")

	   :evaluation-license
	   (:english "Evaluation license"
	    :chinese "评估许可")

	   :provides-fully-functional
	   (:english "provides a fully-functional,
		 time-limited Common Lisp engine configured to load
		 Gendl from its source code on startup. By using the
		 Evaluation license, you agree to both the \"share and
		 share alike\" terms of the"
	    :chinese "证提供一个全功能的，有时间限制的Common Lisp的核心机制，配置成在启动时加载Gendl源代码。通过使用评估许可证，您必须同意")

	   :agpl-open-source-license
	   (:english "AGPL Open Source license"
	    :chinese "APGL开源许可")
	   
	   :as-well-as-the-no-commercial-and-other
	   (:english "as well as the \"No Commercial Use\" and other terms of the"
	    :chinese "的“分享和与他人分享”,以及“无商业用途”和评估许可的其他条款。")
	   
	   :evaluation-may-be-renewed
	   (:english "The Evaluation license may be renewed a
		 reasonable number of times while a purchase or Open
		 Source decision is being made. After a successful
		 Trial period with the Evaluation license, a request
		 may be made to add the SMLib geometry kernel to an
		 extended Evaluation period."

	    :chinese "评估许可证可在正式购买和开源决定期间合理数量地续约。完成一个成功的评估试用期后，可以请求添加SMLib的几何内核扩展的评估期。")
	   
	   :student-license
	   (:english "Student license"
	    :chinese "学生版")

	   :is-valid-for-one-year
	   (:english "is valid for one year from
	           date of initiation (renewable each year at current
		   prices), and is intended for undergraduate
		   Students as a learning tool and to peform
		   coursework.  Student-level licensing is not
		   authorized for use on University-sanctioned or
		   government/industry sponsored research
		   projects. The software package designed for the
		   Student level License provides a fully-functional
		   Common Lisp engine configured to load Gendl from
		   its source code on startup. By using the Student
		   license, you agree to both the \"share and share
		   alike\" terms of the "
	    :chinese "许可证有效期为一年，从开始之日起（按当年价格每年可续约），目的是作为一种本科生的学习工具，辅助课程。学生级许可没有被授权使用在大学认可或政府/工业界资助的研究项目。学生水平许可软件包的设计提供一个全功能的Common Lisp配置，其源代码在启动时加载Gendl。通过使用学生证，你同意")

	   :student-license-also-qualifies-for-smlib
	   (:english "The Student license also
		   qualifies to have the SMLib geometry kernel added
		   as an option."
	    :chinese "学生版 许可证也有资格把SMLib几何内核作为一个选项添加。")

	   
	   :professional-license
	   (:english "Professional License"
	    :chinese "专业版")
	   
	   :is-intended-for-use-in-the-early
	   (:english "is intended for use in the early phases of
                       proprietary application development.  It does
                       not include the capability to generate runtime
                       application distributions. The Professional
                       Edition may also be appropriate for providing
                       additional Development seats within an
                       organization or department which already has at
                       least one Enterprise seat. Gendl source code
                       developed with Professional Edition may be
                       distributed as proprietary, closed-source
                       compiled binaries (so-called \"fasl\" files)
                       for use on licensed installations of Gendl
                       Runtime."
	    :chinese "本许可证可用来开发最初阶段的私有应用程序。它不包括生成运行的应用程序分布的能力。专业版适合于已经拥有企业版的公司和部门里， 作为额外的开发版本。专业版开发gendl的源代码可以发布为专有，封闭源代码编译为二进制文件（即所谓的“FASL”文件）在持牌Gendl运行安装使用。")

	   :professional-is-perpetual
	   (:english "The Professional license is a perpetual Gendl
                   license; from date of initiation, you may continue
                   to develop and distribute closed-source compiled
                   files, with the initiated Gendl version, into
                   perpetuity. As an option you may renew maintenance
                   and support on an annual basis for 25% of the
                   initiation price for Gendl-related components and
                   support, and 50% for SMLib-related components."
	    :chinese "专业版执照是一个永久不失效的Gendl许可证;从开始之日起，你可以永远利用Gendl版本持续开发和发布闭源代码编译的文件。您可以选择每年延续Gendl的维护和售后咨旬。费用是原价25%的Gendl相关组件的支持和维护，和原价50％的SMLib相关维护和支持。")
	   
	   :enterprise-license
	   (:english "Enterprise License"
	    :chinese "企业版本")


	   :allows-all-uses-plus
	   (:english "allows all uses provided
                       by the Professional license, with the
                       additional ability to generate closed-source,
                       proprietary, self-contained Runtime
                       applications. These Runtime applications may be
                       used free of charge in"
	    :chinese "的许可证是建立在专业版的基础上，允许所有用户产生闭源的，专有的，自足运行的应用程序。这些运行时应用程序可以免费使用在：")

	   :noncommercial-testing
	   (:english "Noncommercial deployments (e.g. for testing
		    and demonstration purposes)"
	    :chinese "非商业性部署（例如用于测试和演示目的）")

	   :academic-eg-university
	   (:english "Academic (e.g. University research lab) settings."
	    :chinese "在学术（如大学的研究实验室）的设置。")

	   :for-commercial-deployments-gendl-runtime-are-available
	   (:english "For commercial closed-source runtime
                   deployments, Gendl Runtime licenses are available
                   under a flat-fee, internal model or a
                   percentage-based VAR distribution model."
	    :chinese "对于闭源的商业运行部署，我们提供Gendl运行许可证下的VAR销售模式，您只付您销售额的百分比。")

	   :enterprise-is-perpetual
	   (:english "Enterprise license is a perpetual Gendl
                   license; from date of initiation, you may continue
                   to develop and distribute closed-source compiled
                   files, with the initiated Gendl version, into
                   perpetuity. In order to generate closed-source
                   self-contained Runtime applications, your Gendl
                   Enterprise license must be paid-up with maintenance
                   on an annual basis, at a price of 25% of the
                   initiation price for Gendl-related components, and
                   50% for SMLib-related components."
	    :chinese "企业版本执照是一个永久不失效的Gendl许可证;从开始之日起，你可以永远利用Gendl版本持续开发和发布闭源代码编译的文件。但是为了生成闭源自足运行的应用程序，您必须每年延续Gendl的维护和售后咨旬并上缴每年的维修费用，原价25%的Gendl相关组件的支持和维护，和原价50％的SMLib相关维护和支持。")
	   
	   :academic-pricing
	   (:english "Academic Pricing"
	    :chinese "学术定价")

	   :as-our-investment-in-future
	   (:english "As our investment in the future, and in the
		 possible improvement of Gendl from research results,
		 Genworks offers a 50% discount for most Gendl
		 components for use in Academic institutions. Academic
		 settings which qualify for Academic pricing
		 include:"
	    :chinese "作为我们对未来的投资以及研究成果可能给Gendl带来的改进，Genworks将把Gendl的大部分组件对学术机构中提供50％的折扣。符合学术定价的学术环境包括：")

	   :academic-institutions
	   (:english "Academic Institutions of higher learning (i.e. Universities)"
	    :chinese "高校（即大学）的学术机构")

	   :government-research-labs
	   (:english "Government research laboratories"
	    :chinese "政府研究实验室")

	   :government-industry-consortia
	   (:english "Government/Industry/University collaborative research consortia"
	    :chinese "政府/工业/大学合作研究财团")

	   :furthermore-academic-runtimes-free
	   (:english "Furthermore, Runtime applications in an Academic
		 setting (generated from an Enterprise-level licensed
		 Gendl seat) are by definition non-commercial, and can
		 therefore be deployed free of charge without limit."
	    :chinese "此外，在学术环境中的运行应用程序（来自于企业版本证书支持的Gendl席位）被定义为非商业用途，因此，可以免费无限制部署。")

	   :copyright-genworks
	   (:english "Copyright &copy; 2012 Genworks International. All right reserved."
		     :chinese "生成应用程序开发的实用工具版权所有©2012 Genworks国际。保留所有权利。")


	   )))



    (let ((ht (make-hash-table :size (length strings))))
      (mapc #'(lambda(key value) (setf (gethash key ht) value))
	    (plist-keys strings) (plist-values strings)) ht)))





