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

(in-package :gdl-tutorial)

(defparameter *gwl-syntax*
    `((:chapter :title "GWL Syntax")
      
      (:index "web user interface!creating")
      (:index "HTTP")
      (:index "HTML")
      "GWL (Generative Web Language) consists essentially of a set of mixins and
a few functions which provide a convenient mechanism to present KB objects
defined in GDL through a standard HTTP/HTML web user interface. GWL ships
as a standard component of the commercial GDL product, and is available
on the GDL Trial Edition CD as well. 

GWL is designed to operate in conjunction with AllegroServe"
      (:index "AllegroServe")
      (:index "htmlgen")
      (:footnote "AllegroServe is an open-source webserver from Franz Inc, available
at http://opensource.franz.com")
      " and its companion HTML generating facility, htmlgen. 

This chapter describes basic GWL usage and syntax. It assumes familiarity with the
underlying base language, GDL, covered in the previous chapter."
      
      ((:section :title "Testing your GWL Installation")
       "After you have installed according to "
       (:texttt "install.htm")
       ":"
       ((:list :style :enumerate)
        (:item "Make sure you have started AllegroServe with:"
               (:verbatim 
                "(net.aserve:start :port 9000)")
	       "(or any port of your choice)")
        (:item "In any standard web browser, go to:"
               (:verbatim
                "http://<hostname>:<port>/demos/robot")
	       "e.g.: "
               (:verbatim
                "http://localhost:9000/demos/robot")))
       
       "You should see a page with a simple robot assembly made from boxes. If 
the robot shows up as well as the orange graphical \"compass\" below the 
graphics viewport, your installation is likely working correctly.")
      
      
      ((:section :title "GWL:Define-Package")
       "The macro "
       (:texttt "gwl:define-package")
       " is provided for setting up new working GWL packages.

Example:"
       (:verbatim "(gwl:define-package :gwl-user)")
       "The "
       (:texttt ":gwl-user")
       " package is an empty, pre-defined package for your use if you 
do not wish to make a new package just for scratch work.

For real projects it is recommended that you make and work in your
own GWL package.")


      ((:section :title "Basic Usage")
       "To present a GDL object instance as a web page requires two simple
steps:"
       ((:list :style :enumerate)
        (:item "mix in "
               (:texttt (:indexed "base-html-sheet"))
               " or a subclass thereof")
        (:item "define a function in the object called "
               (:texttt (:indexed "write-html-sheet"))))
       
       "The "
       (:texttt "write-html-sheet")
       " function should typically make use of the htmlgen "
       (:texttt "html")
       " macro. It does not need to take any arguments.

Please see the htmlgen documentation (at "
       (:texttt "http://opensource.franz.com")
       ", with the AllegroServe distribution, or in "
       (:texttt "<gdl-home>/doc/aserve/")
       ") for full details on the use of htmlgen.

The code for a simple example object with its "
       (:texttt "write-html-sheet")
       " presentation function is shown in Figure "
       (:ref "code:basic-usage")
       ". This example contains two optional input slots with 
values for Name and Term of a president, and creates a simple HTML 
table displaying this information. As outlined above, in order to 
make an instance and display this object through a web browser, you 
would visit the URI:"
       (:verbatim "http://<host>:<port>/make?object=gwl-user::president")
       
       ((:boxed-figure :caption "Basic Usage"
                       :label "code:basic-usage")
        (:verbatim "


 (define-object president (base-html-sheet)
   :input-slots
   ((name \"Carter\")
    (term 1976))

   :functions
   ((write-html-sheet 
     ()
     (html
      (:html
       (:head (:title (format nil \"Info on President: ~a\" (the name))))
       (:body 
        (:table
         (:tr (:td \"Name\") (:td \"Term\"))
         (:tr (:td (:princ (the name))) (:td (:princ (the term)))))))))))
    ")))
      
      
      ((:section :title "Page Linking")
       "Creating hypertext links to other pages in the page hierarchy is
usually accomplished with the built-in GDL function of "
       (:texttt "base-html-sheet")
       " called "
       (:texttt "write-self-link")
       ". This GDL function, when called on a particular page instance, will write 
a hypertext link referring to that page instance.

These hypertext links are published by AllegroServe ``on the fly'' (as
a side-effect of being demanded), and are made up from the unique
root-path of the target object, as well as an "
       (:indexed "instance-id")
       " which identifies the particular object instance which is the ``root'' of
the relevant page hierarchy. This is necessary because GWL maintains a
table of root-level instances. Each root-level instance will usually
correspond to one \"user\" or session. However, in general, there can
be a many-to-many relationship between user sessions and root-level
instances.

The instance-id is generated randomly. On a publicly-accessible website,
the maximum instance-id should be set to a very large number to decrease
the likelihood of a malicious visitor being able to ``guess'' the 
instance-id of another user. The maximum is set with the parameter "
       (:texttt (:indexed "gwl:*max-id-value*"))
       ". 

Figures "
       (:ref "code:page-linking")
       " and "
       (:ref "code:link-target")
       " show the code for making a page with a list of links
to pages representing individual U.S. presidents, resulting in a 
web page which should resemble Figure "
       (:ref "fig:presidents-container")
       ". Note the call
to the "
       (:texttt "write-self-link")
       " function inside the "
       (:texttt "dolist")
       " in Figure "
       (:ref "code:page-linking")
       ". This results in an HTML list item being generated with a hyperlink
for each ``president'' child object.

Note also the use of the "
       (:texttt (:indexed "write-back-link"))
       " function in "
       (:texttt "presidents-display")
       ". This will generate a link back to the "
       (:texttt (:indexed "return-object"))
       " of the object, which defaults to the object's "
       (:texttt "parent")
       "."
       
       ((:boxed-figure :caption "Making a List of Links"
                       :label "code:page-linking")
        (:verbatim "


 (define-object presidents-container (base-html-sheet)
   :input-slots
   ((data '((:last-name \"Carter\" :term 1976)
            (:last-name \"Clinton\" :term 1992))))
   :objects
   ((presidents :type 'president-display
                :sequence (:size (length (the data)))
                :last-name (getf (nth (the-child index)
                                      (the data))
                                 :last-name)
                :term (getf (nth (the-child index)
                                 (the data))
                            :term)))
   :functions
   ((write-html-sheet 
     ()
     (html
      (:html
       (:head (:title \"Links to Presidents\"))
       (:body
        (:h1 \"Links to the Presidents\")
        (:ol
         (dolist (president (list-elements (the presidents)))
           (html
            (:li (the-object president (write-self-link))))))))))))
 "))
       
       ((:boxed-figure :caption "Link Target"
                       :label "code:link-target")
        (:verbatim "

 
 (define-object president-display (base-html-sheet)
   :input-slots
   (last-name term)

   :computed-slots
   ((strings-for-display (the last-name)))
  
   :functions
   ((write-html-sheet
     ()
     (html
      (:html
       (:head (:title (:princ (the last-name))))
       (:body
        (:h1 (:princ (the last-name)))
        \"Term: \" (:princ (the term))
        (:p (the (write-back-link)))))))))
"))

       
       ((:image-figure :image-file "presidents-container.png"
                       :caption "Presidents Container Page with Links"
                       :label "fig:presidents-container"))
       
       )
      
      ((:section :title "Form Handling")
       (:index "form handling")
       "Forms are generated using the GWL macro"
       (:texttt "with-html-form")
       ". You wrap this macro around the HTMLgen code which creates the contents of the form:"
       (:verbatim "

 (with-html-form ()
  
   ;; the body of your form goes here
  
    )")
       "The above code snippet would be included in a "
       (:texttt "write-html-sheet")
       "function of a page definition.  

By default, the same object which
generates the form will also respond to the form, and is also the
object which will have its settable slots modified based on form
fields (i.e. html ``inputs'') of the same name. You can override the
default by specifying a " 
       (:texttt "bashee") 
       " and/or " 
       (:texttt "respondent") 
       "as slots in the requestor object (i.e. the object which is generating the form), for
example:"
       
       (:verbatim "

 :computed-slots
 ((respondent (the some-other-object))
  (bashee (the yet-another-object)))

")
       "Any "
       (:texttt ":settable")
       " computed-slots in the object may be specified as input values (i.e.\\ with "
       (:texttt ":input")
       " tags) in the form. GWL will automatically infer their types
and do appropriate conversion. If the type of a slot can vary, it is best
to make its default be a string, then have your application read from
the string (with the "
       (:texttt (:indexed "read-safe-string"))
       " function).

Note that only those input values which have actually changed
 (according to "
       (:texttt "equalp")
       " ) will be set into the corresponding computed-slot upon form submission. 
Ones which remain the same will be left alone (to avoid unnecessary dependency 
updating in the model).

Any "
       (:texttt ":input")
       " values in the form whose name does not match one of the "
       (:texttt ":settable")
       " computed-slots in the object will still be collected, but rather than being set into
 its own named slot, it will be returned as part of the special "
       (:texttt "query-plist")
       " message when the response page's "
       (:texttt "write-html-sheet")
       " method is invoked. "
       (:texttt "Query-plist")
       " is a plist containing keywords representing the form field names, and 
values which will be strings representing the submitted values.


If you want to do additional processing, the following functions are provided
for " 
       (:texttt "base-html-sheet")  ":"
       
       ((:list :style :description)
	((:item :word "before-set!")
	 "This is invoked before the ``bashee'' is modified with any new form values.")
	((:item :word "after-set!")
	 "This is invoked after the ``bashee'' is modified with any new form values.")
	((:item :word "before-present!")
	 "This is invoked after the ``bashee'' is modified with any new form values, but 
before the page content is returned to the web client.")
	((:item :word "after-present!")
	 "This is invoked after the page content is returned to the web client."))

       " By default, these functions are empty, but you can override them to do whatever 
extra processing you wish.

Figure "
       (:ref "code:hello-form")
       " shows an object which both generates and responds to a simple form,
with the corresponding web page shown in Figure "
       (:ref "fig:hello-form")
       ". The form allows the user to type a name to override the default ``Jack,'' and 
reflects the submitted name in the form page upon response.

To instantiate this object in a web browser, you would visit the URI:"
       (:verbatim "http://<host>:<port>/make?object=gwl-user::hello-form")
       ((:boxed-figure :caption "Hello Form"
                       :label "code:hello-form")
        (:verbatim "


 (define-object hello-form (base-html-sheet)

   :computed-slots ((username \"Jack\" :settable))

   :functions
   ((write-html-sheet 
     ()
     (html
      (:html 
       (:head (:title \"Sample Form\"))
       (:body 
        (:p \"Hello there, \" (:princ (the username)) \"!\")
        (:p (with-html-form ()
             ((:input :type :text :name :username 
                      :value (the username)))
             ((:input :type :submit :name :submit 
                      :value \" Change Name! \"))))))))))
"))

       ((:image-figure :image-file "hello-form.png"
                       :caption "Hello Form"
                       :label "fig:hello-form")))
      
      
      ((:section :title "Publishing URIs for GWL Objects")
       (:index "publishing!of GWL URIs")
       "You can publish a URI for a given object, to avoid having to type the
``make?'' expression, using the AllegroServe "
       (:texttt "publish")
       " function and the GWL "
       (:texttt (:indexed "gwl-make-object"))
       " function, as per the following example:"
       (:verbatim "
  (publish :path \"/demos/bus\"
           :function #'(lambda(req ent)
                         (gwl-make-object req ent \"bus:assembly\")))")
       "In this example, the ``bus'' object would now be instantiated simply by
visiting the URI:"
       (:verbatim "http://<host>:<port>/bus"))
      
      ((:section :title "Higher-level Apps and Graphics")
       "GDL/GWL also has the ability to generate and display standard wireframe geometric
entities. The complete list of currently available primitive geometric objects is available
in the GDL documentation set. Currently the only documented way to display geometry in
a GWL application is by using the higher-level mixin "
       (:texttt (:indexed "application-mixin"))
       ". Two complete examples of the use of this mixin, the Robot and the School Bus, are
given in Chapters "
       (:ref "chap:example2:simplifiedandroidrobot")
       " and "
       (:ref "chap:example3:schoolbus")
       ". Here we will just touch on the basics of how to use this
mixin."
       ((:list :style :enumerate)
        (:item "Instead of "
               (:texttt "base-html-sheet")
               ", mix in "
               (:texttt "application-mixin")
               " into the object definition you wish to publish via the web.")
        (:item "Collect the objects whose leaves you wish to display as geometry in a computed-slot named"
               (:texttt (:indexed "ui-display-list-objects"))
	       "."))
       
       "Following the above steps will result in a page with a default user interface which 
will display your graphics in the center. This page, and each of its components, are highly
customizable, and we will look at some of the available customizations in the examples in
Chapters "
       (:ref "chap:example2:simplifiedandroidrobot")
       " and "
       (:ref "chap:example3:schoolbus")
       ".")))

