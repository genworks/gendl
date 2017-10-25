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
  `((:chapter :title "Custom User Interfaces in GDL")

    (:p "Another strength of GDL is the ability to create custom
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
web pages, sections of web pages, and form elements.
Your UI elements generate standard HTML, CSS, JavaScript, and can use
any external client-side libraries as with any standard web application.  AJAX support is
built-in, making it easy for server and UI elements to communicate asynchronously."
)

    (:p "In order to create a web application in GDL, you should have a working
knowledge of the semantics of HTML, for which many explanations are available
online and in print.  For syntax, you can use an HTML-generation library such as "
	 (:href "http://weitz.de/cl-who" "CL-WHO") (:footnote "http://weitz.de/cl-who")
	 " or "
	 (:href "http://www.franz.com/support/documentation/current/doc/aserve/htmlgen.html" "HTMLGen")
	 (:footnote "http://www.franz.com/support/documentation/current/doc/aserve/htmlgen.html")
         ", both of which are built into GDL.  In this tutorial we will use CL-WHO, so in what folows,
we will assume that you are already familiar with its features as documented in "
	 (:href "http://weitz.de/cl-who" "http://weitz.de/cl-who") ".")

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ((:section :title "Package and Environment for Web Development")

     (:p "The "(:texttt ":gwl")" package"(:footnote "The acronym \"GWL\" stands
for Generative Web Language")" contains a set of primitive objects and functions provided by
GDL for building web applications.")

     (:p "Similarly to "(:texttt "gdl:define-package")", you can use "(:texttt "gwl:define-package")"
to create a working package which has access to the symbols you will need for building a
web application (in addition to the other GDL symbols).")
     
     (:p "The "(:texttt ":gwl-user")" package is pre-defined and may be used for practice
work. For real projects, you should define your own package using "(:texttt "gwl:define-package")".")

     (:p "The YADD"(:footnote "YADD is accessible with "(:texttt
"http://localhost:9000/yadd")".")" reference documentation for package
GWL provides detailed specifications for all the primitive objects and functions.")
)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ((:section :title "Web Page Objects")

     (:p "To make a GDL object presentable as a web page, the following two
steps are needed:"

	 ((:list :style :enumerate)
	  (:item "Mix " (:texttt "base-ajax-sheet") " into the object definition.")
	  (:item "Within the object, define the GDL message "(:texttt "main-sheet-body")" returning an HTML string.")))

     (:p "As soon the object is defined, the object's page becomes accessible at the URL"
         (:quote (:texttt "http://localhost:9000/make?object="(:emph "classname")))
"where "(:texttt(:emph "classname"))" is the object name (including the package name).  Connecting to this
URL starts a new web server session and creates an instance of the object unique to that session"(:footnote
"This is done by redirecting the response to a unique URL identified by a "(:emph "session ID")".
The session ID is constructed from a combination of the current date and time, along with a pseudo-random number.")".
This ensures that each user of your application site will see their own specific instance of the object.")

     (:p "The "(:texttt "main-sheet-body")" message can be either a computed slot or a GDL function.  It should
produce a string of the HTML to be placed in the body of the page, i.e. between the "(:texttt "<body>")" and
"(:texttt "</body>")" tags.  The "(:texttt "<head>")" of the page is filled in automatically by GDL, and can be
customized in various ways.")

     (:p "The easiest way to produce a valid "(:texttt "main-sheet-body")" string is with the GWL convenience macro
"(:texttt "with-cl-who-string")".  This is a wrapper for the CL-WHO macro "(:texttt "with-html-output")" which
additionally establishes the default environment for outputting an HTML string within a GWL application.")

     (:p "Figure "(:ref "fig:gwl-hello-world")" is an example of a simple static web page."
     ((:boxed-figure :caption "Simple Static Page" :label "fig:gwl-hello-world")
      (:small (:verbatim (:include "~/genworks/gendl/documentation/tutorial/examples/gwl-hello-world.gdl"))))
     #+NOT-YET
     ((:image-figure :image-file "gwl-hello-world.png" :caption  "Simple Static Page"
                     :width "4in" :height "3in"
                     :label "fig:gwl-hello-world-image")))

     (:p "This simple framework can also be used to create dynamic content, as illustrated in figure "(:ref "fig:gwl-president-table")".
Note that CL-WHO symbols such as "(:texttt "htm")", "(:texttt "str")", and "(:texttt "fmt")" are available in GWL without
package qualification. See "(:href "http://weitz.de/cl-who" "http://weitz.de/cl-who")" for an explanation of
dynamic HTML generation in CL-WHO."

     ((:boxed-figure :caption "Dynamic Content Using CL-WHO" :label "fig:gwl-president-table")
        (:small (:verbatim (:include "~/genworks/gendl/documentation/tutorial/examples/gwl-president-table.gdl"))))

     #+NOT-YET
     ((:image-figure :image-file "gwl-president-table.png" :caption "Dynamic Content Using CL-WHO"
                     :width "4in" :height "3in"
                     :label "fig:gwl-president-table-image"))
     ))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ((:section :title "Page URLs")

      (:p "Objects based on "(:texttt "base-ajax-sheet")" are automatically available from the server at the
URL of the form "(:texttt "http://localhost:9000/make?object="(:emph "classname"))".  This is useful during
debugging, but for real projects, you should define a more direct URL.  You do that using the
CL function "(:texttt "(gwl:publish-gwl-app "(:emph "path")" "(:emph "classname")")")".  For example,"
      (:quote (:texttt "(gwl:publish-gwl-app \"/greeting\" \"gwl-user::hello-world\")"))
"will make the "(:texttt "gwl-user::hello-world")" object accessible at the URL "(:texttt "http://localhost:9000/greeting")"."))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ((:section :title "Page Customizations")

     ;;; TODO: Could illustrate customizations by adding CSS to make Hello World red. (:p :class "red" "Hello World!") and maybe
     ;;; javascript to change colors when click?

     (:p "In addition to specifying the body html via "(:texttt "main-sheet-body")", you can
customize the page by optionally adding certain messages to your object:"
         ((:list :style :itemize)
          (:item "Message "(:texttt "title")" can be used to specify the title of the web page")
          (:item "Message "(:texttt "doctype-string")" is the string to place at the very start
of the document (it defaults to "(:texttt "\"<!DOCTYPE HTML>\"")")")
          (:item "Message "(:texttt "additional-header-content")" can contain any additional
HTML you want to go into the page's "(:texttt "<head>")" section.")
          (:item "Message "(:texttt "body-class")" can specify the CSS "(:texttt "class")" attribute for the "(:texttt "<body>")" tag.")
          (:item "Message "(:texttt "body-onload")" can be a string of Javascript to go
into the "(:texttt "onload")" event attribute of the "(:texttt "<body>")" tag.")
          (:item "Message "(:texttt "body-onpageshow")" can be a string of Javascript to go
into the "(:texttt "onpageshow")" event attribute of the "(:texttt "<body>")" tag.")))

     (:p "We will be using some of these customizations in the examples below."))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ((:section :title "Debugging")
      ;;;TODO: what else is there to say here?
     (:p ""(:texttt "base-ajax-sheet")" provides the "(:texttt "development-links")" message with
links for functionality useful during development, currently consisting of a Refresh! link and a Break
link.  It is typically used as follows:"
          (:verbatim "(main-sheet-body
  (with-cl-who-string ()
    (when *developing?* (str (the development-links)))
    ;; Rest of page definition goes here
    ...))")))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ((:section :title "Page Links")

     (:p "In order to allow HTML links between page objects, GDL implements a scheme that assigns
URL's to individual page instances.  In order to do this efficiently, GDL maintains a table of
root-level instances, and identifies page instances relative to their root instance.  For this reason,
and in order for dependency-tracking to work properly, all pages in a GDL application must belong to
the same tree, i.e. they must share a common root page object.")

     (:p "You can generate a hyperlink (an "(:texttt "<A>")" tag) to a particular "(:texttt "base-ajax-sheet")" page
by invoking the "(:texttt "write-self-link")" GDL function on the page instance. "(:texttt "write-self-link")" accepts
a number of keyword arguments to customize the link:"
          ((:list :style :itemize)
           (:item "You specify the text to be displayed as the link with the "(:texttt ":display-string")" argument")
           (:item "You can direct the link to a specific anchor within the page with the "(:texttt ":local-anchor")" argument")
           (:item "You can specify various tag attributes of the "(:texttt "<A>")" tag:"
                  ((:list :style :itemize)
                   (:item "Argument "(:texttt ":target")" for the "(:texttt "target")" attribute")
                   (:item "Argument "(:texttt ":on-mouse-over")" for the "(:texttt "onmouseover")" attribute")
                   (:item "Argument "(:texttt ":on-mouse-out")" for the "(:texttt "onmouseout")" attribute")
                   (:item "Argument "(:texttt ":on-click")" for the "(:texttt "onclick")" attribute")
                   (:item "Argument "(:texttt ":title")" for the "(:texttt "title")" attribute")
                   (:item "Argument "(:texttt ":class")" for the "(:texttt "class")" attribute")
                   (:item "Argument "(:texttt ":id")" for the "(:texttt "id")" attribute")))))
      
     (:p "In order to help create hierarchical multi-page sites, "(:texttt "base-ajax-sheet")" also provides
a "(:texttt "write-back-link")" GDL function for the purpose of generating a link back to the parent page object.
It accepts all the same arguments as "(:texttt "write-self-link")".")

     (:p "Figure "(:ref "fig:gwl-president-links")" shows a modification of "(:ref "fig:gwl-president-table")"
where we put detailed information about each president on a separate page, and provide links to these individual
pages from a summary page.  Each child page contains a link to return back to the summary."

         ((:boxed-figure :caption "Hyperlinking" :label "fig:gwl-president-links")
          (:small (:verbatim (:include "~/genworks/gendl/documentation/tutorial/examples/gwl-president-links.gdl"))))

         #+NOT-YET
         ((:image-figure :image-file "gwl-president-links.png" :caption "Hyperlinking"
                         :width "4in" :height "3in"
                         :label "fig:gwl-president-links-image")))
      )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ((:section :title "Input Using Forms")
     (:p "GWL provides a macro "(:texttt "with-html-form")" to obtain input from the user
with an HTML form.  In the body of the form, you specify input fields using standard HTML
form controls.  When the user submits the form, GDL processes the form input values in two
ways:"
          ((:list :style :itemize)
           (:item "When the name of an form field matches the name of a "(:texttt
":settable")" computed-slot in the object, GDL will automatically infer its type, do
appropriate conversions, and set the slot the its new value"(:footnote "If the type of a slot
can vary, it is best to make its default be a string and parse the string yourself e.g. with
the "(:texttt "read-safe-string")" function.")".")
           (:item "Any input values that do not match a suitable slot are available 
from the "(:texttt "query-plist")" message, which returns a plist mapping
keywords representing form field names to the corresponding input value strings")))
     (:p "Figure "(:ref "fig:gwl-hello-world-form")" shows how we can extend the simple
Hello World application of "(:ref "fig:gwl-hello-world")" to allow the user to
customize both the name and the greeting."
          
         ((:boxed-figure :caption "Input Using Forms" :label "fig:gwl-hello-world-form")
           (:small (:verbatim (:include "~/genworks/gendl/documentation/tutorial/examples/gwl-hello-world-form.gdl"))))
          
         #+NOT-YET
          ((:image-figure :image-file "gwl-hello-world-form.png" :caption "Input Using Forms"
                          :width "4in" :height "3in"
                          :label "fig:gwl-hello-world-form-image")))

     ((:subsection :title "Form Controls")

      (:p "GDL provides a set of primitives useful for generating the
standard HTML form controls in the body of "(:texttt "with-html-form")".
These should be instantiated as child objects in the page.  They provide a
"(:texttt "html-string")" message which returns the HTML for the control.")

      (:p "The form controls provided by GDL are documented in YADD"(:footnote "YADD
is accessible with "(:texttt "http://localhost:9000/yadd")".")" and in the reference appendix
of this manual. Examples of available form controls are:"
          ((:list :style :itemize)
           (:item (:texttt "text-form-control"))
           (:item (:texttt "checkbox-form-control"))
           (:item (:texttt "menu-form-control"))
           (:item (:texttt "radio-form-control"))
           (:item (:texttt "text-form-control"))
           (:item (:texttt "button-form-control"))))


      (:p "These form controls are customizable by mixing them into
	   your own specific form controls (although this often is not
	   necessary). New form controls such as for numbers, dates,
	   etc will soon be added to correspond to latest HTML
	   standards.")

#+old (:p "Figure "(:ref "fig:gwl-3b")" is an example which allows the user to enter a year,
          and the application will respond with the revenue amount for that year. Additional
          form controls are also provided to adjust the table border and cell padding.
          This example, when instantiated in a web browser, might look as shown in Figure "(:ref
          "fig:gwl-3b-image")"."
          
          ((:boxed-figure :caption "Using Form Controls"
                          :label "fig:gwl-3b")
           (:tiny (:verbatim (:include "~/genworks/gendl/documentation/tutorial/examples/gwl-3b.gdl"))))
          
          ((:image-figure :image-file "gwl-3b.png" :caption "Using Form Controls" 
                          :width "4in" :height "3in"
                          :label "fig:gwl-3b-image")))

       (:p "Figure "(:ref "fig:gwl-hello-world-controls")" reimplements the Hello World form from the last section
using form controls.  The functionality is the same but the source is shorter and simpler to read and understand."
          ((:boxed-figure :caption "Using Form Controls" :label "fig:gwl-hello-world-controls")
           (:small (:verbatim (:include "~/genworks/gendl/documentation/tutorial/examples/gwl-hello-world-controls.gdl"))))
       )))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ((:section :title "Interactive Applications using AJAX")

     (:p "Input using traditional HTML forms requires the user to explicitly \"submit\" each update
request, and then the application responds by reloading the whole page.  The "(:href
"http://en.wikipedia.org/wiki/Ajax_(programming)" "AJAX")(:footnote
"http://en.wikipedia.org/wiki/Ajax_(programming)")" methodology allows web
applications to autonomously contact the server, and to change page content dynamically in
response, leading to a more interactive user experience.")

     (:p "GDL has built-in support for AJAX, using its usual convenient generative approach.
You use child objects to model independently updatable sections of the web pgae, and the dependency
tracking engine which is built into GDL automatically manages of which sections of the
page need to be updated after a request.")

     (:p "Moreover, the state of the internal GDL model which
represents the page and the page sections is kept identical to the
displayed state of the page. This means that if the user hits the
``Refresh'' button in the browser, the state of the page will remain
unchanged. This ability is not available in some other AJAX
frameworks.")

     ((:subsection :title "AJAX Event Handling")
      
      (:p "An AJAX application works by contacting the server
in response to "(:href "https://en.wikipedia.org/wiki/DOM_events" "HTML events")
(:footnote "https://en.wikipedia.org/wiki/DOM_events")" such as \"onclick\"
or \"onfocus\".  GDL provides the function "(:texttt "gdl-ajax-call")" to generate
the JavaScript to handle such events by invoking GDL functionality on the server.")
      (:p "For example, the following CL-WHO snippet"
	  (:verbatim "((:span :onclick (the (gdl-ajax-call :function-key :restore-defaults!)))
        \"Press Me\")")
	  "will produce a piece of text ''Press Me'' which, when pressed, will call "(:texttt "restore-defaults!")"
in the page's object on the server. If "(:texttt "restore-defaults!")" is not defined, an error will result.")
      (:p (:texttt "gdl-ajax-call")" can also update form control values on the server by using the "(:texttt
":form-controls")" keyword argument. For details, see the "(:texttt "gdl-ajax-call")" documentation in
YADD"(:footnote "YADD is accessible with "(:texttt "http://localhost:9000/yadd")".")" and the reference appendix.")

      (:p "For convenience, GDL form-control objects provide direct support for AJAX with the "(:texttt ":ajax-submit-on-change?")"
argument, which is equivalent to invoking "(:texttt "gdl-ajax-call")" for the control's \"onchange\" event."))

     ((:subsection :title "AJAX Page Updating")

      (:p "In order to have the capacity to change itself in response to AJAX calls, a page must be structured
as one or more "(:texttt "sheet-section")" child objects.  Sheet sections provide a "(:texttt "main-div")" message
which computes the HTML for the section and registers the section as subject to AJAX update handling.  The
main page should include the "(:texttt "main-div")" of each child sheet section in its "(:texttt "main-sheet-body")".")

      (:p "The sheet section definition should specify the section's HTML in the "(:texttt "inner-html")" input slot.
This plays the same role as the "(:texttt "main-sheet-body")" does in the page.")

      (:p "Figure "(:ref "fig:gwl-hello-world-ajax")" reimplements the Hello World form from the last section
using AJAX.  Note that we don't need the explicit UPDATE button any more, as the changes the user makes take effect
immediately due to the use of "(:texttt ":ajax-submit-on-change?")" argument in each of the form controls."
          ((:boxed-figure :caption "Using AJAX"
                          :label "fig:gwl-hello-world-ajax")
           (:small (:verbatim (:include "~/genworks/gendl/documentation/tutorial/examples/gwl-hello-world-ajax.gdl"))))
       )

      )


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
       (:tiny (:verbatim (:include "~/genworks/gendl/documentation/tutorial/examples/gwl-5.gdl"))))


      ((:image-figure :image-file "gwl-5.png" :caption "Including Graphics" 
		       :width "5in" :height "4in"
		       :label "fig:gwl-5-image"))

      (:p "The example in Figure " (:ref "fig:gwl-5")
	  " contains a simple box with two graphics viewports
and ability to modify the length, height, and and width of the box:")



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





