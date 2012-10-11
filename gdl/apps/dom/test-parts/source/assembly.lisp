(in-package :user)

(defparameter *assembly*
    `((:document :title   "Test Document"
                 :author  "David J. Cooper Jr."
                 :company "Genworks International"
                 :date    "January, 2001"
                 :textwidth 6.5
                 :textheight 8.5
                 :topmargin 0
                 :oddsidemargin 0
                 :evensidemargin 1.0
                 :class   :article)
      ((:section :title "Executive Summary")
       "This is a high-level "
       (:emph "executive summary")
       " of the whole paper.")
      
      ((:section :title "Sample List")
       ((:list :style :description)
	((:item :word "Scope and Requirements")
	 "We currently know only a small portion of the required functions of the
system. We will only discover the full range of requirements with some initial deployment 
and user feedback.")
	((:item :word "Technology Landscape")
	 "The IT landscape changes constantly, different solutions vendors, 
operating system platforms, programming languages, database systems, etc, constantly 
vying for advantage. Along with this, various Corporate-wide ``one size fits all'' 
initiatives promise far-reaching solutions but lack the flexibility to meet all 
specific needs in a timely and responsive manner."))
       
       ((:list :style :itemize)
	(:item "Do start building a prototype system.")
	(:item "Use only stable, well-defined technologies"))
       
       ((:list :style :enumerate)
	(:item "Do start building a prototype system.")
	(:item "Use only stable, well-defined technologies")))))
