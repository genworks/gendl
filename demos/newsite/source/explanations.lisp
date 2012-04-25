(in-package :www.genworks.com)

(define-object license-choice-explanation (sheet-section)

  :computed-slots
  ((inner-html (with-cl-who-string ()
		 (:h2 "Licensing Levels Explained")
		 "Genworks Gendl is so-called "
		 ((:a :href "http://en.wikipedia.org/wiki/Multi-licensing") "\"dual-licensed\"")
		 " software, under an open-source license ("
		 ((:a :href "http://www.gnu.org/licenses/agpl-3.0.txt") "AGPL")
		 ") and alternatively under various proprietary
                  licenses (described below) some of which allow you
                  to distribute your application and derivative code
                  and keep it as closed-source, proprietary (Trade
                  Secret) software."

		 ((:a :name "agpl-license"))
		 ((:a :href "#Top") (:h3 "Open Source"))
		 ((:div :class "profile")
		  ((:div :class "people")
		   (:p "The Open Source (\"free software\") license allows
                  your application to enjoy complete freedom. This
                  license follows the philosophy of \"share and share
                  alike.\" By choosing this license (or by using Gendl
                  and distributing a compiled application without
                  making any choice of license), you agree to
                  distribute your application source code under the "
		   ((:a :href "http://www.gnu.org/licenses/agpl-3.0.txt") "AGPL")

		   " or a compatible license.")

		   (:p "There are no restrictions against Commercial Use
                  with the Open Source license; the main requirement
                  is that you offer to share your application code at
                  no cost, just as the Gendl code has been shared with
                  you.")

		   (:p "Gendl application or utility code developed with an
                  Open Source-licensed installation must always remain
                  as Free Software; the Proprietary licenses in
                  general do not allow for code originally developed
                  on an Open Source seat to be converted into a
                  closed-source proprietary application after the
                  fact, using a proprietary Gendl license.")

		   (:p "There is an opportunity to overcome this limitation,
                 however: If you have Gendl application or utility
                 code which was developed using an Open Source
                 installation, and you wish to include or build upon
                 this code in a proprietary/closed-source application,
                 you may:")
		   (:ol
		    (:li "Contribute the code to the Gendl project
		    under our standard "
			 ((:a :href "/newsite-static/documents/contributor.pdf") "Contributor's Agreement")
			 ", sharing with Genworks the copyright to any
		    original work. After suitable review, Genworks may
		    choose to include the code either in the mainline
		    Gendl codebase, or in a \"contrib\" directory;")
		    (:li "Purchase an appropriate proprietary Gendl license;")
		    (:li "Freely include the contributed code in your
		    distributed closed-source applications."))))

		 ((:a :name "trial-license"))
		 ((:a :href "#Top")(:h3 "Evaluation"))
		 ((:div :class "profile")
		  ((:div :class "people")
		   "The "
		   ((:a :href "/newsite-static/documents/eval.txt") "Evaluation license")
		   " provides a fully-functional,
		 time-limited Common Lisp engine configured to load
		 Gendl from its source code on startup. By using the
		 Evaluation license, you agree to both the \"share and
		 share alike\" terms of the "
		   ((:a :href "http://www.gnu.org/licenses/agpl-3.0.txt") "AGPL Open Source license")
		   ", as well as the \"No Commercial Use\" and other terms of the"
		   ((:a :href "/newsite-static/documents/eval.txt") "Evaluation license")
		   ". The Evaluation license may be renewed a
		 reasonable number of times while a purchase or Open
		 Source decision is being made. After a successful
		 Trial period with the Evaluation license, a request
		 may be made to add the SMLib geometry kernel to an
		 extended Evaluation period."))

		 ((:a :name "student-license"))
		 ((:a :href "#Top")(:h3 "Student"))
		 ((:div :class "profile")
		  ((:div :class "people")
		   "The "
		   ((:a :href "/newsite-static/documents/student.txt") "Student license")
		   " is valid for one year from
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
		   ((:a :href "http://www.gnu.org/licenses/agpl-3.0.txt") "AGPL Open Source license")
		   ", as well as the \"No Commercial Use\" terms of the "
		   ((:a :href "/newsite-static/documents/eval.txt") "Evaluation license")
		   ". The Student license also
		   qualifies to have the SMLib geometry kernel added
		   as an option."))
		 
		 ((:a :name "professional-license"))
		 ((:a :href "#Top")(:h3 "Professional"))
		 ((:div :class "profile")
		  ((:div :class "people")
		   (:p "The "
		       ((:a :href "/newsite-static/documents/gendl-development.pdf") "Professional License")
			" is intended for use in the early phases of
proprietary application development.  It does not include the
capability to generate runtime application distributions. The
Professional Edition may also be appropriate for providing additional
Development seats within an organization or department which already
has at least one Enterprise seat. Gendl source code developed with
Professional Edition may be distributed as proprietary, closed-source
compiled binaries (so-called \"fasl\" files) for use on licensed
installations of Gendl Runtime.")

		   (:p "The Professional license is a perpetual Gendl license; from date of
initiation, you may continue to develop and distribute closed-source
compiled files, with the initiated Gendl version, into perpetuity. As
an option you may renew maintenance and support on an annual basis for
25% of the initiation price for Gendl-related components and support, and 50% for
SMLib-related components.")))

		 ((:a :name "enterprise-license"))
		 ((:a :href "#Top") (:h3 "Enterprise"))
		 ((:div :class "profile")
		  ((:div :class "people")
		   (:p "The "
		       ((:a :href "/newsite-static/documents/gendl-development.pdf") "Enterprise License")
		       " Enterprise license allows all uses provided by
the Professional license, with the additional ability to generate
closed-source, proprietary, self-contained Runtime applications. These
Runtime applications may be used free of charge in")

		   (:ol
		    (:li "Noncommercial deployments (e.g. for testing and demonstration purposes)")
		    (:li "In Academic (e.g. University research lab) settings."))
		   (:p " For commercial closed-source runtime deployments, Gendl Runtime licenses are 
available under a flat-fee, internal model or a percentage-based VAR distribution model.")
		   (:p "Professional license is a perpetual Gendl
license; from date of initiation, you may continue to develop and
distribute closed-source compiled files, with the initiated Gendl
version, into perpetuity. In order to generate closed-source
self-contained Runtime applications, your Gendl Enterprise license
must be paid-up with maintenance on an annual basis, at a price of 25%
of the initiation price for Gendl-related components, and 50% for
SMLib-related components.")))

		 ((:a :name "academic-pricing"))
		 ((:a :href "#Top") (:h2 "Academic Pricing"))
		 
		 (:p "As our investment in the future, and in the possible
		 improvement of Gendl from research results, Genworks
		 offers a 50% discount for most Gendl components for
		 use in Academic institutions. Academic settings which qualify for Academic pricing include:")
		 (:ol
		  (:li "Academic Institutions of higher learning (i.e. Universities)")
		  (:li "Government research laboratories")
		  (:li "Government/Industry/University collaborative research consortia"))
		 (:p "Furthermore, Runtime applications in an Academic
		 setting (generated from an Enterprise-level licensed
		 Gendl seat) are by definition non-commercial, and can
		 therefore be deployed free of charge without limit.")))))
