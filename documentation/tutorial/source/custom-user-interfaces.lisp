;;
;; Copyright 2002, 2009, 2012 Genworks International
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;
;; This source file contains free software: you can redistribute it
;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This source file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with this source file.  If not, see
;; <http://www.gnu.org/licenses/>.
;;

(in-package :gendl-doc)

(defparameter *custom-user-interfaces*
  `((:chapter :title "Custom User Interfaces in Gendl")

    (:p "Another of the strengths of GDL is the ability to create custom
web-based user interfaces. GDL contains a built-in web server and
supports the creation of generative "
	(:emph "web-based")
	" user interfaces"
	(:footnote "GDL does not contain support for native desktop
GUI applications. Although the host Common Lisp
environment (e.g. Allegro CL or LispWorks) may contain a GUI builder
and Integrated Development Environment, and you are free to use these,
GDL does not provide specific support for them.")
	". Using the same "
	(:texttt "define-object")
	" syntax which you have already encountered, you can define
web pages, sections of web pages, and "
	(:emph "form control")
	" elements such as type-in fields, checkboxes, and choice
lists [using this capability does require a basic working knowledge of
the HTML language]."
	(:footnote "We will not cover HTML in this manual, but
plentiful resources are available online and in print.")
	".")

    (:p "Any web extensions such as custom JavaScript and JavaScript libraries
can also be used, as with any standard web application.")

    (:p "With the primitive objects and functions in its "
	(:texttt ":gwl")
	" package, GDL supports both the traditional ``Web 1.0''
interfaces (with fillout forms, page submittal, and complete page
refresh) as well as so-called ``Web 2.0'' interaction with AJAX.")


    ((:section :title "Package and Environment for Web Development")
     (:p "Similarly to " (:texttt "gdl:define-package") ", you can use "
	 (:texttt "gwl:define-package") " in order to create a working package which has
access to the symbols you will need for building a web application (in
addition to all the other GDL symbols).")
     
     (:p "The " (:texttt ":gwl-user") " package is pre-defined and may be used for practice
work. For real projects, you should define your own package using "
	 (:texttt "gwl:define-package") ".")

     (:p "The acronym ``GWL'' stands for Generative Web Language,
which is not actually a separate language from GDL itself, but rather
is a set of primitive objects and functions available with GDL for
building web applications. The YADD reference documentation for
package ``Generative Web Language'' provides detailed specifications
for all the primitive objects and functions."))

    ((:section :title "Traditional Web Pages and Applications")
     (:p "To make a GDL object presentable as a web page, the following two
steps are needed:"

	 ((:list :style :enumerate)
	  (:item "Mix " (:texttt "base-html-sheet") " into the object definition.")
	  (:item "define a GDL function called " (:texttt "main-sheet") " within the object definition."))

	 "The " (:texttt "main-sheet") " function should return valid
HTML for the page. The easiest way to produce HTML is with the use of
an HTML generating library, such as "
	 (:href "http://weitz.de/cl-who" "CL-WHO") " or "
	 (:href "http://www.franz.com/support/documentation/current/doc/aserve/htmlgen.html" "HTMLGen")
	 ", both of which are built into GDL.")

     (:p "For our examples we will use cl-who, which is currently the
standard default HTML generating library used internally by
GDL. Here we will make note of the major features of cl-who while
introducing the examples; for complete documentation on cl-who, please
visit the page at Edi Weitz' website linked above.")



     ((:subsection :title "A Simple Static Page Example")
      (:p "In Figure "
	  (:ref "fig:gwl-1")
	  ", GWL convenience macro "
	  (:texttt "with-cl-who")
	  " is used; this sets up a standard default environment for outputting HTML 
within a GWL application."
	  
	  ((:boxed-figure :caption "Simple Static Page Example" :label "fig:gwl-1")
	   (:small (:verbatim
		    (:include "~/gendl/documentation/tutorial/examples/gwl-1.gdl")))))


      (:p "The code in Figure "
	  (:ref "fig:gwl-1")
	  " produces HTML output as shown in Figure "
	  (:ref "fig:gwl-1-html")
	  " which looks similar to Figure "
	  (:ref "fig:gwl-1-image")
	  " in a web browser."

	  ((:boxed-figure :caption "Simple Static Page Example" :label "fig:gwl-1-html")
	   (:small (:verbatim
		   (:include "~/gendl/documentation/tutorial/examples/gwl-1.html"))))
	  
	  ((:image-figure :image-file "gwl-1.png" :caption "Simple Static Page Example" 
			  :width "4in" :height "3in"
			  :label "fig:gwl-1-image")))


      (:p "Several important concepts are packed into this example. Note the following:"

	  ((:list :style :itemize)

	   (:item "Our convenience macro "
	     (:texttt "with-cl-who")
	     " is used to wrap the native "
	     (:texttt "with-html-output")
	     " macro which comes with the cl-who library.")

	   (:item "We use the keyword argument "
	     (:texttt ":indent t")
	     " in order to pretty-print the generated HTML. This does
     not affect the browser display but can make the generated HTML
     easier to read and debug. This option should be left as nil (the
     default) for production deployments.")

	   (:item "The " (:texttt "fmt") " symbol has special meaning
      within the cl-who environment and works the same as a Common
      Lisp " (:texttt "(format nil ...)") ", in order to evaluate a format
      string together with matching arguments, and produce a string at
      runtime.")

	   (:item "The " (:texttt "str") " symbol has special meaning
      within the cl-who environment and works by evaluating an
      expression at runtime to return a string or other printable
      object, which is then included at that point in the HTML output.")

	   (:item "Expressions within the " 
	     (:texttt "body") " of an
      HTML tag have to be evaluated, usually by use of the "
	     (:texttt "fmt")
	     " or "
	     (:texttt "str")
	     " in cl-who.  There are three examples of this in the
      above sample: one "
	     (:texttt "fmt")
	     " and two "
	     (:texttt "str")
	     ".")

	   (:item "Expressions within a "
	     (:emph "tag attribute")
	     " are always evaluated automatically, and so do "
	     (:textbf "not")
	     " require a "
	     (:texttt "str")
	     " or other special symbol to force evaluation at
      runtime. Tag attributes in HTML (or XML) are represented as a
      plist spliced in after a tag name, wrapped in extra parentheses
      around the tag name. In the sample above, the "
	     (:texttt ":border (the table-border)")
	     " is an example of a tag attribute on the "
	     (:texttt ":table")
	     " tag. Notice that the expression "
	     (:texttt "(the table-border)")
	     " does not need "
	     (:texttt "str")
	     " in order to be evaluated - it gets evaluated automatically.")

	   (:item "In cl-who, if a tag attribute evaluates to "
	     (:texttt "nil")
	     ", then that tag attribute will be left out of the output
      completely. For example if "
	     (:texttt "(the table-border)")
	     " evaluates to nil, then the "
	     (:texttt ":table")
	     " tag will be outputted without any attributes at
      all. This is a convenient way to conditionalize tag
      attributes.")

	   (:item "The URL "
	     (:texttt "http://localhost:9000/make?object=gwl-user::president")
	     " is published automatically based on the package and
      name of the object definition. When you visit this URL, the
      response is redirected to a unique URL identified by
      a " (:emph "session ID") ". This ensures that each user to your
      application site will see their own specific instance of the
      page object. The session ID is constructed from a combination of
      the current date and time, along with a pseudo-random
      number."))))



     ((:subsection :title "A Simple Dynamic Page which Mixes HTML and Common Lisp/GDL")

      (:p "Within the cl-who environment it is possible to include any standard
Common Lisp structures such as "
	  (:texttt "let")
	  ", "
	  (:texttt "dolist")
	  " , "
	  (:texttt "dotimes")
	  ", etc, which accept a "
	  (:emph "body")
	  " of code. The requirement is that any internal code body
	  must be wrapped in a list beginning with the special symbol "
	  (:texttt "htm")
	  ", which has meaning to cl-who. ")
      
      ((:boxed-figure :caption "Mixing Static HTML and Dynamic Content" :label "fig:gwl-2")
       (:small (:verbatim
	       (:include "~/gendl/documentation/tutorial/examples/gwl-2.gdl"))))

      ((:image-figure :image-file "gwl-2.png" :caption "Mixing Static HTML and Dynamic Content" 
			  :width "4in" :height "3in"
			  :label "fig:gwl-2-image"))

      (:p "The example in Figure "
	  (:ref "fig:gwl-2")
	  "  uses this technique to output an HTML table row for each ``row'' of data in a list of lists.
The output looks similar to Figure "
	  (:ref "fig:gwl-2-image")
	  " in a web browser.")

      (:p "Note the following from this example:"
	  ((:list :style :itemize)
	   (:item (:texttt "title")
	     " is a "
	     (:texttt "let")
	     " variable, so we use "
	     (:texttt "(str title)")
	     " to evaluate it as a string. We do not use "
	     (:texttt "(str (the title))")
	     " because "
	     (:texttt "title")
	     " is a local variable and not a message (i.e. slot) in the object.")

	   (:item "Inside the "
	     (:texttt "dolist")
	     ", we ``drop back into'' HTML mode using the "
	     (:texttt "htm")  " operator.")
	   )))


     ((:subsection :title "Linking to Multiple Pages")
      (:p "The base-html-sheet mixin provides a "
	  (:texttt "self-link")
	  " message for the purpose of generating a hyperlink to that
page. Typically you will have a ``parent'' page object which links to
its ``child'' pages, but GDL pages can link to other pages anywhere
in the GDL tree"
	  (:footnote "In order for dependency-tracking to work
properly, the pages must all belong to the same tree, i.e. they must
share a common root object.")
	  "."

	  ((:boxed-figure :caption "Linking to Multiple Pages"
			  :label "fig:gwl-3")
	   (:small (:verbatim (:include "~/gendl/documentation/tutorial/examples/gwl-3.gdl"))))

	  ((:boxed-figure :caption "Linking to Multiple Pages"
			  :label "fig:gwl-3a")
	   (:small (:verbatim (:include "~/gendl/documentation/tutorial/examples/gwl-3a.gdl"))))

	  " In Figures "
	  (:ref "fig:gwl-3")
	  " and "
	  (:ref "fig:gwl-3a")
	  ", we provide links from a parent page into a child page
with detailed information on each president. The output looks similar
to Figure "
	  (:ref "fig:gwl-3-image")
	  " in a web browser."

	  ((:image-figure :image-file "gwl-3.png" :caption "Linking to Multiple Pages" 
			  :width "4in" :height "3in"
			  :label "fig:gwl-3-image")))

      (:p " Note the following from this example:"
	  ((:list :style :itemize)
	   (:item "The "
	     (:texttt "write-self-link")
	     " message is a function which can take a keyword argument
   of "
	     (:texttt ":display-string")
	     ". This string is used for the actual hyperlink text.")
	   (:item "There is a "
	     (:texttt "write-back-link")
	     " message which similarly can take a keyword argument of "
	     (:texttt ":display-string")
	     ". This generates a link back to "
	     (:texttt "(the return-object)")
	     " which, by default in base-html-sheet, is "
	     (:texttt "(the parent).")))))


     ((:subsection :title "Form Controls and Fillout-Forms")
      ((:subsubsection :title "Form Controls")
      
       (:p "GDL provides a set of primitives useful for generating the
       standard HTML form-controls"
	   (:footnote "http://www.w3.org/TR/html401/interact/forms.html")}
	   " such as text, checkbox, radio, submit, menu, etc. These
            should be instantiated as child objects in the page, then
            included in the HTML for the page
            using " (:texttt "str") " within an
            HTML " (:texttt "form")
	   " tag (see next section).")


       (:p "The form-controls provided by GDL are documented in YADD accessible with "
	   (:verbatim "http://localhost:9000/yadd")
	   " and in Chapter " (:ref "chapter:gdlreference") " of this Manual. Examples of 
available form-controls are:"
	   ((:list :style :itemize)
	    (:item (:texttt "text-form-control"))
	    (:item (:texttt "checkbox-form-control"))
	    (:item (:texttt "menu-form-control"))
	    (:item (:texttt "radio-form-control"))
	    (:item (:texttt "text-form-control"))
	    (:item (:texttt "button-form-control"))))


       (:p "These form-controls are customizable by mixing them into
	   your own specific form-controls (although this is often not
	   necessary). New form-controls such as for numbers, dates,
	   etc will soon be added to correspond to latest HTML
	   standards."))

      ((:subsubsection :title "Fillout Forms")
       
       (:p "A traditional web application must enclose form controls inside a "
	   (:texttt "form")
	   " tag and specify an " (:texttt "action") " (a web URL) to receive and respond to the 
form submission. The response will cause the entire page to refresh with
a new page. In GDL, such a form can be generated by wrapping the layout
of the form controls within the " (:texttt "with-html-form") " macro.")

       ((:boxed-figure :caption "Form Controls and Fillout Forms"
			  :label "fig:gwl-3b")
	   (:small (:verbatim (:include "~/gendl/documentation/tutorial/examples/gwl-3b.gdl"))))

       ((:image-figure :image-file "gwl-3b.png" :caption "Form Controls and Fillout Forms" 
		       :width "4in" :height "3in"
		       :label "fig:gwl-3b-image"))

       (:p "In Figure "
	   (:ref "fig:gwl-3b")
	   " is an example which allows the user to enter a year, and the 
application will respond with the revenue amount for that year. Additional
form controls are also provided to adjust the table border and cell padding.")

       (:p "This example, when instantiated in a web browser, might look as shown in Figure "
	   (:ref "fig:gwl-3b-image")
	   "."))))


    ((:section :title "Partial Page Updates with gdlAjax")
     (:p (:indexed "AJAX")
	 " stands for Asynchronous JavaScript and
XML " 
	 (:footnote "http://en.wikipedia.org/wiki/Ajax_(programming)")
	 ", and allows for more interactive web applications which
respond to user events by updating only part of the web page. The
``Asynchronous'' in Ajax refers to a web page's ability to continue
interacting while one part of the page is being updated by a server
response. Requests need not be Asynchronous, they can also be
Synchronous (``SJAX''), which would cause the web browser to block
execution of any other tasks while the request is being carried
out. The ``XML'' refers to the format of the data that is typically
returned from an AJAX request.")

     (:p "GDL contains a simple framework referred to as "
	 (:emph "gdlAjax")
	 " which supports a uniquely convenient and generative
approach to AJAX (and SJAX). With gdlAjax, you use standard GDL
object definitions and child objects in order to model the web page
and the sections of the page, and the dependency tracking engine which
is built into GDL automatically keeps track of which sections of the
page need to be updated after a request.")

     (:p "Moreover, the state of the internal GDL model which
represents the page and the page sections is kept identical to the
displayed state of the page. This means that if the user hits the
``Refresh'' button in the browser, the state of the page will remain
unchanged. This ability is not present in some other Ajax
frameworks.")

     ((:subsection :title "Steps to Create a gdlAjax Application")
      (:p "First, it is important to understand that the fundamentals from the
previous section on Standard Web Applications still apply for gdlAjax
applications --- that is, HTML generation, page linking, etc. These
techniques will all still work in a gdlAjax application.")

      (:p "To produce a gdlAjax application involves three main differences from
a standard web application:"

	  
	  ((:list :style :enumerate)
	   (:item "You mix in " (:texttt "base-ajax-sheet") " instead of " (:texttt "base-html-sheet")
	     ". " (:texttt "base-ajax-sheet") " mixes in " (:texttt "base-html-sheet")
	     ", so it will still provide all the functionality of that
   mixin. In fact, you can use "
	     (:texttt "base-ajax-sheet")
	     " in standard web applications and you won't notice any difference if you do
    everything else the same.")
	   (:item "Instead of a "
	     (:texttt "write-html-sheet")
	     " message, you specify a " (:texttt "main-sheet-body")
	     " message. The " (:texttt "main-sheet-body") "  can be a computed-slot or GDL function,
    and unlike the " (:texttt "write-html-sheet") " message, it should
    simply return a string, not send output to a stream. Also, it only
    fills in the body of the page --- everything between the <body>
    and </body> tags. The head tag of the page is filled in
    automatically and can be customized in various ways.")
	   (:item "Any sections of the page which you want to be able
      to change themselves in response to an Ajax call must be made
      into separate page sections, or ``sheet sections,'' and the HTML
      for their " (:texttt "main-div") " included in the main page's "
      (:texttt "main-sheet-body") " by use of cl-who's " (:texttt "str") " directive."))


	  ((:boxed-figure :caption "Partial Page Updates with GdlAjax"
			  :label "fig:gwl-4")
	   (:small (:verbatim (:include "~/gendl/documentation/tutorial/examples/gwl-4.gdl")))))

      (:p "Note the following from the example in Figure "
	  (:ref "fig:gwl-4") ":"
	  
	  ((:list :style :itemize)
	   (:item "We mix in " (:texttt "base-ajax-sheet") " and specify a "
		  (:texttt "main-sheet-body") " slot, which uses "
		  (:texttt "with-cl-who-string") " to compute a string
                   of HTML. This approach is also easier to debug,
                   since the " (:texttt "main-sheet-body") " string
                   can be evaluated in the tasty inspector or at the
                   command-line.")

	   (:item "We use " (:texttt "str") " to include the string for the main page
        section (called " (:texttt "main-section") " in this example) into the "
        (:texttt "main-sheet-body") ".")

	   (:item "In the " (:texttt "main-section") ", we also
        use " (:texttt "str") " to include the html-string for each of
        three form-controls. We have provided a form control for the
        table border, the table padding, and the revenue year to look
        up.")

	   (:item "The only page section in this example is "
	     (:texttt "(the main-section)")
	     ". This is defined as a child object, and has its "
	     (:texttt "inner-html")
	     " computed in the parent and passed in as an input. The "
	     (:texttt "sheet-section")
	     " will automatically compute a "
	     (:texttt "main-div")
	     " message based on the "
	     (:texttt "inner-html")
	     " that we are passing in. The " (:texttt "main-div")
	     " is simply the " (:texttt "inner-html") ", wrapped with
               an HTML DIV (i.e. ``division'') tag which contains a unique
               identifier for this section, derived from the root-path
               to the GDL object in the in tree which represents the
               sheet section.")

	   (:item "We introduce the CL function " (:texttt "gwl:publish-gwl-app")
		  ", which makes available a simplified URL for visiting an instance of this
        object in the web browser. In this case, we can access the
        instance using "
		  (:texttt "http://localhost:9000/revenue-lookup"))))


      

      (:p "Notice also the use of "
	  (:texttt ":ajax-submit-on-change? ...")
	  " in each of the form-controls. This directs the gdlAjax
	  system to ``scrape'' the values of these form controls
	  and ``bash'' them into the "
	  (:texttt "value")
	  " slot of the corresponding object on the server, whenever
they are changed in the browser. No ``Submit'' button press is
necessary.")

      (:p "It is also possible programmatically to send form-control
      values, and/or call a GDL Function, on the server, by
      using the "
	  (:texttt (:indexed "gdl-ajax-call"))
	  " GDL function. This function will emit the necessary
	  JavaScript code to use as an event handler, e.g. for an
	  ``onclick'' event. For example, you could have the following snippet somewhere in your page:"
	  (:verbatim "((:span :onclick (the (gdl-ajax-call :function-key :restore-defaults!))) \"Press Me\" )")
	  "This will produce a piece of text ``Press Me,'' which,
	  when pressed, will have the effect of calling a function named "
	  (:texttt "restore-defaults!")
	  " in the page's object on the server. If the function "
	  (:texttt "restore-defaults!")
	  " is not defined, an error will result. The "
	  (:texttt "gdl-ajax-call")
	  " GDL function can also send arbitrary form-control values
	  to the server by using the "
	  (:texttt ":form-controls")
	  " keyword argument, and listing the relevant form-control objects. The "
	  (:texttt "gdl-ajax-call")
	  " GDL function is fully documented in YADD and the reference appendix.")

      
      (:p "If for some reason you want to do more than one "
	  (:texttt "gdl-ajax-call")
	  " sequentially, then it is best to use "
	  (:texttt "gdl-sjax-call")
	  " instead. This variant will cause the browser to wait until
	  each call completes, before making the next call. To achieve
	  this, you would want to append the strings together, e.g:"
	  (:verbatim "
       ((:span :onclick (string-append (the (gdl-sjax-call ...))  
                                       (the (gdl-sjax-call ...)) 
                                       (the (gdl-sjax-call ...))) ... ))")

	  "With that said, it is rarely necessary to do these calls
	  sequentially like this, because you can use :form-controls
	  and :function-key simultaneously. As long as your logic
	  works correctly when the form-controls are set before the
	  function is called, then you can group the functions
	  together into a ``wrapper-function,'' and do the whole
	  processing with a single Ajax (or Sjax) call. Normally this
	  would be be the recommended approach whenever it it
	  possible."))


     ((:subsection :title "Including Graphics")
      (:p "The fundamental mixin or child type to make a graphics viewport is " 
	  (:texttt "base-ajax-graphics-sheet")
	  ". This object definition takes several optional input-slots, but the most essential are the " 
	  (:texttt ":display-list-objects") " and the " 
	  (:texttt ":display-list-object-roots")
	  ". As indicated by their names, you specify a list of nodes to include in
the graphics output with the " 
	  (:texttt ":display-list-objects")
	  ", and a list of nodes whose " 
	  (:texttt "leaves")
	  " you want to display in the graphics output with the " 
	  (:texttt ":display-list-object-roots")
	  ". View controls, rendering format, action to take when clicking on objects, etc, 
can be controlled with other optional input-slots.")


      ((:boxed-figure :caption "Including Graphics in a Web Page"
		      :label "fig:gwl-5")
       (:small (:verbatim (:include "~/gendl/documentation/tutorial/examples/gwl-5.gdl"))))


      ((:image-figure :image-file "gwl-5.png" :caption "Including Graphics" 
		       :width "5in" :height "4in"
		       :label "fig:gwl-5-image"))

      (:p "The example in Figure " (:ref "fig:gwl-5")
	  " contains a simple box with two graphics viewports
and ability to modify the length, height, and and with of the box:")



      (:p "This will produce a web browser output similar to what is shown in Figure "
	  (:ref "fig:gwl-5-image")
	  ".")
      
      (:p "Note the following from this example:"

	  ((:list :style :itemize)

	   (:item "The " (:texttt "(:use-raphael? t)") " enables raphael for SVG or VML output.")

	   (:item "The " (:texttt ":raphael") " image-format generates SVG or VML, depending on the browser.")

	   (:item "We conditionally include development-links for full Update and SetSelf! actions.")

	   (:item "We include two viewports in the " (:texttt "main-sheet-body") ", elements from a sequence of size 2.")

	   (:item "In the inputs-section, we use
                  the " (:texttt "html-string") " message from each
                  form-control to display the default
                  decoration (prompt, etc).")))


      



      ))))





