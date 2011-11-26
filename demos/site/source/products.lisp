(in-package :genworks.com)


(define-object products (base-site-sheet)
  
  :computed-slots
  ((body-class "products columns_two")
   )
  
  :objects
  ((column-left 
    :type 'sheet-section
    :main-view nil)
   
   (column-right 
    :type 'sheet-section
    :main-view nil)
   
   (column-center 
    :type 'sheet-section
    :main-view 
    (with-cl-who-string ()
      ((:div :class "content") 
       ((:p :class "clear") "&nbsp;")
       ((:div :class "column row_top") (:h1 "Genworks offers GDL in a package tailored to your specific needs."))
       ((:div :class "column row_top") 
	(:h5 "Genworks Products use a unique core language called GDL.")
	(:p
	 "The acronym stands for General-Purpose, Declarative, Language. By General-Purpose it is meant that the 
language may be used to create a wide spectrum of end results. It is particularly effective at representing 
complex systems, including three-dimensional geometric models.")
	(:p
	 "Examples with geometry include auto or airplane wiring or hose systems, sheet metal surfaces, 
baggage delivery carousels, storage tanks and boilers, airplane fuselage or other components, plus many others. 
Examples without geometry would be such things as a Trucking company's national delivery scheduling, 
a company's multi-national Patent Tracking system, an individuals computer Diary system."))
       ((:p :class "clear") "&nbsp;")
       ((:div :class "column row_bottom enterprise_edition") 
	(:h5 "1. Enterprise Edition Development Environment")
	(:p "The " (:strong "Enterprise Edition")
	    " is for use in developing, building, and deploying runtime applications in a networked corporate 
Enterprise environment. It. includes:")
	"
		"
	(:ul (:li "All GDL/GWL Professional Edition components")
	     (:li
	      "Ability to generate standalone runtime single-user and (web) server-based GDL/GWL applications, 
as well as shared-library (.DLL or .so) applications")
	     (:li
	      "Distributed GDL (dGDL), allowing child objects of GDL instance trees to exist on separate 
processes on local or remote server(s) ")
	     (:li "Access to Relational Databases")
	     (:li
	      "GDL integrated object-oriented relational database support, including table definition compiler 
and caching and dependency-tracking for database table and row objects")
	     (:li "GWL relational database user interface, providing default web-based access to insert, update, 
and delete database table rows")
	     (:li "Secure Socket Layer for building secure web server and client applications")
	     (:li
	      "Enterprise customers are automatically licensed to generate and use Runtime Applications for 
noncommercial activities (e.g. deployment testing, demonstrations, etc.)")))
       ((:div :class "column row_bottom professional_development") 
	(:h5 "2. Professional Edition Development Environment")
	(:p "The " (:strong "Professional Edition")
	    " is for use in the early phases of application development. It does not include the capability to 
generate runtime applications. It includes:")
	"
		"
	(:ul (:li "GDL Compiler and Development Environment (seamlessly integrated with CL/CLOS compiler)")
	     (:li "Library of GDL Objects, Functions, and Macros")
	     (:li "GWL Web Application Framework, integrated with a fully-featured web server")
	     (:li "GWL-based GDL Object Tree Browser and Inspector")
	     (:li "Fully cross-referenced web-based GDL/GWL reference documentation, tutorials, and a set of 
functioning sample applications")
	     (:li "Base CL/CLOS compiler and development system with profiler and debugger"))))))))



