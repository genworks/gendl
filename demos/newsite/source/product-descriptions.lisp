(in-package :www.genworks.com)

(define-object product-descriptions (base-site-sheet)

  :computed-slots
  ((title "Genworks International - Product Descriptions")
   (link-title  "Descriptions")
   
   (right-section-js-to-eval "$j('#all-go').hide(200);$j('#product-image').show(200);")

   (right-section-inner-html 
    (with-cl-who-string ()
      (:h2 "Product Descriptions") 
      (:h3 "Overview")
      ((:div :class "profile")
       ((:div :class "people")
	(:h4 "Genworks GenDL")
	(:p
	 "Our core product is Genworks GenDL. GenDL stands for
General-purpose Declarative Language. While GenDL excels
at representing complex hierarchical systems, including
three-dimensional geometric models, the language framework itself is
domain-independent and can be used to achieve a wide range of
results.")
	(:p
	 "GenDL combines the best aspects of a spreadsheet, a
dynamic object-oriented programming language, a parametric CAD system, 
a web application server, and a Knowledge-management system.")))

      ((:div :class "profile")
       ((:div :class "people")
	(:h4 "Free, Open-source Core")
	(:p
	 "Starting in October 2011, the GenDL core source code
has been released under the Affero Gnu Public License and is freely
available for any type of use (including commercial use) under the
terms of this license. GenDL is based on ANSI Common Lisp, and
as such, requires a compliant Common Lisp implementation to build and
execute. Currently GenDL is validated for Allegro CL, LispWorks, and
SBCL.")))

      (:h3 "Core Features")

      ((:div :class "profile")
       ((:div :class "people")
	(:h4 "Core Features available in the Open-Source core:")
	(:p
	 (:ul 
	  (:li "GenDL Compiler and Development Environment (seamlessly integrated with CL/CLOS compiler)")
	  (:li "Library of core GenDL Objects, Functions, and Macros")
	  (:li "Library of 3D and 2D geometric GenDL Objects, Functions, and Macros")
	  (:li "GenDL Web Application and Ajax Framework, integrated with a fully-featured, in-memory, 
open-source AllegroServe webserver")
	  (:li "GenDL-based Object Tree Browser and Inspector with 3D geometry rendering")
	  (:li "Distributed GenDL (dGenDL), allowing child objects of GenDL instance trees to exist on separate 
processes on local or remote server(s) ")
	  (:li "Access to Relational Databases")
	  (:li "GenDL integrated object-oriented relational database support, including table definition compiler 
and caching and dependency-tracking for database table and row objects")
	  (:li "GenDL relational database user interface, providing default web-based access to insert, update, 
and delete database table rows")
	  (:li "Fully cross-referenced web-based GenDL reference documentation")
	  (:li "Comprehensive PDF manual with introduction, tutorials, and examples")
	  (:li "Library of functioning sample applications")))))

      (:h3 "Commercial Builds")

      ((:div :class "profile")
       ((:div :class "people")
	(:p
	 (:i "For Proprietary (closed-source) applications, and
environments which demand commercial software components and support,
Genworks offers configured packages with proprietary distribution
rights. These packages come with industrial-strength commercial 
Common Lisp engines built-in, and optionally with the SMLib solid 
modeling kernel and expert technical support."))))

      ((:div :class "profile")
       ((:div :class "people")
	(:h4 "1. Professional Edition (also available for Academic/Research use at 50% price reduction)")
	(:p
	 "The Professional Edition is for use in the early phases of
application development.  It does not include the capability to
generate runtime application distributions. The Professional Edition
may also be appropriate for providing additional Development seats
within an organization or department which already has at least one
Enterprise seat. Applications developed with Professional Edition may
be distributed as proprietary, closed-source compiled binaries for use
on licensed installations of GenDL Runtime.")))

      
      ((:div :class "profile")
       ((:div :class "people")
	(:h4 "2. Enterprise Edition (also available for Academic/Research use at 50% price reduction)")
	(:p
	 "The Enterprise Edition is for use in developing, building,
and deploying run-time applications in a networked corporate
Enterprise environment, or on the Internet. Enterprise customers are
automatically licensed to generate and use run-time applications for
noncommercial activities (e.g. deployment testing, demonstrations,
etc). Customers who wish to generate and deploy company-internal
run-time applications for production use may purchase GenDL Runtime
Licenses for this purpose. Customers who wish to generate and deploy
commercial run-time applications may apply to Genworks to become GenDL
VARs (Value-added Resellers).")))
      

      (:h3 "Surfaces and Solids Geometry Kernel")
      (:p
       "For advanced Surface and Solids geometry modeling,
Genworks offers an integrated Geometry kernel based on the SMLib
product from Solid Modeling Solutions, Inc. The integration with SMLib
adds powerful surface and solids capabilities to GenDL's built-in 3D
wireframe facilities. SMLib provides extensible filleting, as well as
full support for non-manifold topology (e.g. edges sharing more than
two faces) for boundary-representation solids. Along with the dynamic
modeling capability, the SMLib option also provides input and output
support for the standard Iges and STEP formats as well as the
possibility of direct CAD translation.")

      (:p "The combination of the industry-standard SMLib kernel library with 
GenDL's interactive and dynamic development environment results in an unprecedented
level of power and flexibility for developing domain-specific \"intelligent\" 
domain-specific CAD solutions.")

      (:h3 "Proprietary Licensing")

      ((:div :class "profile")
       ((:div :class "people")
	"Customers who require proprietary (closed-source) application
distribution rights, but who wish to perform their own software builds
using their own Common Lisp engine, and do not require Genworks
support or the SMLib geometry kernel, may purchase Proprietary
Licensing as an unbundled product."))

      ))))



      