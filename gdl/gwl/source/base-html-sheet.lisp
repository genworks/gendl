;;
;; Copyright 2002-2011 Genworks International 
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


(in-package :gwl)



(defmethod encode-for-ajax ((item t)) item)

(defmethod encode-for-ajax ((item list))
  (when item
    (cons (encode-for-ajax (first item))
          (encode-for-ajax (rest item)))))

(defmethod encode-for-ajax ((self gdl::root-path-container))
  (list :%container-rp% (the root-path)))

(defmethod encode-for-ajax ((self gdl::gdl-basis))
  (list :%rp% (the root-path)))


(defmethod decode-from-ajax ((item t) self) 
  (declare (ignore self)) item)

(defmethod decode-from-ajax ((item list) self)
  (when item
    (cond ((eql (first item) :%rp%)
           (the (follow-root-path (getf item :%rp%))))
	  ((eql (first item) :%container-rp%)
           (make-object 'gdl::root-path-container :root-path (getf item :%container-rp%)))
          (t (cons (decode-from-ajax (first item) self)
                   (decode-from-ajax (rest item) self))))))
        

(defmacro with-cl-who ((&rest args) &body body)

    "Form. Sets up body to be evaluated with cl-who and output the resulting string to the default *stream*

Note that the args are spliced into cl-who:with-html-output after *stream* nil, so for example you can do <pre>

    (with-cl-who (:indent t) ...)

</pre>

and it will expand into:

<pre>

    (with-html-output (*stream* nil :indent t) ...)

</pre>

."

    `(with-format (html-format *stream*)

        (with-html-output (*stream* nil ,@args)

            ,@body)))


(defmacro with-cl-who-string ((&rest args) &body body)

    "Form. Sets up body to be evaluated with our with-cl-who return the resulting string instead

of side-effecting anything at all to the default *stream*."

    `(with-output-to-string (*stream*)

        (with-cl-who (,@args) ,@body)))

(defmacro with-htm (&body body)
  `(with-cl-who-string () ,@body))



(define-object base-html-sheet (sheet-section)

  :documentation
  (:description "This mixin allows a part to be displayed as a web page in GWL. 
The main output can be specified either in a <tt>write-html-sheet</tt> function in the object which
mixes  this in, or in a <tt>main-sheet</tt> output-function in an html-format view of the 
object.")

  :input-slots
  (("GDL object. Default object to which control will return with the write-back-link method"
    return-object (the :parent))
   ("String. Name of a browser frame or window to display this page. Default of NIL indicates to use the same window."
    target nil)
   
   (instance-id nil :defaulting)
   
   (cookies-to-send nil)
   
   (cookies-received nil :settable)
   
   (fixed-url-prefix nil :defaulting)
   
   (tree-root (the root) :defaulting)
   
   ;;
   ;; Stuff to support dashboard
   ;;
   (time-last-touched nil :settable)
   (time-instantiated nil :settable)

   (last-visited-root-path nil :settable)

   (remote-host-original nil :settable)
   (remote-host nil :settable)

   
   
   ("List of keyword symbols. Messages corresponding to form fields which should not be retained
against Updates to the model (e.g. calls to the update! function or hitting the Update button or link in 
the browser in development mode). Defaults to NIL (the empty list)."
    transitory-slots nil)
   
   ("Boolean. Determines whether a a sanity check is done (with the <tt>check-sanity</tt> function) before
 presenting the response page if this page is a respondent. Default is NIL."
    check-sanity? nil)
   
   (home-page nil :defaulting)
   
   
   ($$tatu-object (the $$tatu) :settable)
   ($$ta2-object (the $$ta2) :settable)
   
   (plain-url? nil :defaulting)
   
   (host nil :defaulting)
   
   (query-toplevel nil :settable)
   
   )

  :computed-slots
  (
   
   
   ("Plist. Extra http headers to be published with the URI for this page."
    header-plist nil)

   ;;
   ;; FLAG -- replace localhost with computed hostname from running machine.
   ;;
   (full-url (when *wserver*
               (format nil "http://localhost:~a~a"
                       (socket:local-port (net.aserve:wserver-socket  net.aserve:*wserver*))
                       (the url))))
   
   ("String. The web address in the current session which points at this page. Published on demand."
    url (let ((url
               (if (the plain-url?) (format nil "/~a" (the (compute-url)))
                 (format nil "~a/sessions/~a/~a" (if (the fixed-url-prefix) (format nil "/~a" (the fixed-url-prefix)) "")
                         (the :instance-id) (the (compute-url))))))
          (publish
           :path url
           :content-type "text/html"
           :host (the host)
           :function #'(lambda (req ent) 
                         (the before-response!)
                         (present-part req ent url :instance-id (when (the plain-url?) 
                                                                  (the instance-id)) 
                                       :header-plist (the header-plist)
                                       :fixed-prefix (the fixed-url-prefix))))
          (pushnew url (gethash (make-keyword (the instance-id)) *url-hash-table*) :test #'string-equal)
          
          (setf (gethash url *descriptive-url-hash*) (the root-path))
          (when *debug?* (print-variables url))
          
          url))
   
   ("Plist. Contains submitted form field names and values for which no corresponding settable 
computed-slots exist. Where corresponding settable computed-slots exist, their values are set from 
the submitted form fields automatically."
    query-plist nil :settable)
   
   (%internal-hidden-object-keywords% (append (list :$$update :$$update-full :$$break :$$tatu :$$ta2 :color-palette
                                                    :security-check-failed)
                                              (call-next-method))))


  :trickle-down-slots
  (tree-root instance-id plain-url? host home-page query-toplevel fixed-url-prefix)

  :hidden-objects
  (($$update :type 'update
             :return-to self)
   ($$update-full :type 'update-full
                  :return-to self)
   ($$break :type 'break-on
            :break-on self)
   ($$tatu :type (read-safe-string "tatu:assembly")
           :root-object self)
   
   ($$ta2 :type (if (find-package :ta2) (read-safe-string "ta2:assembly") 'null-part)
          ;;:root (the-child)
          :root-object self)
   
   
   (security-check-failed :type 'security-check-failed)
   
   (color-palette :type 'color-palette))
  
  :functions
  (
   (do-security-check () 
     (or *bypass-security-check?*
         (the plain-url?)
         (eql (the remote-host) (the remote-host-original))))
   
   (descriptive-url () (string-append (the descriptive-url-base) ".html"))
   
   (descriptive-url-base
    ()
    
    (let ((url
           (let ((escaped-strings-for-display (html-escape (the strings-for-display))))
             (if (the parent)
                 (string-append (the parent descriptive-url-base)
                                "/"
                                escaped-strings-for-display)
               escaped-strings-for-display))))
      url))
   
   
   (compute-url 
    ()
    (compute-url (the root-path)))
   
   (set-self () 
             (if *break-on-set-self?*
                 
                 (progn (set-self self)
                        (let ((*package* (symbol-package (the type))))
                          (break)))
               (progn
                 (set-self self))))
   
   ("Void. Calls restore-defaults! on all the form-controls in this sheet."
    restore-form-controls!
    ()
    (dolist (form-control (the form-controls))
      (the-object form-control restore-defaults!)))
   
   
   (set-remote-host!
    (req &key original?)
    (if original?
        (the (set-slot! :remote-host-original (glisp:remote-host (request-socket req))))
      (the (set-slot! :remote-host (glisp:remote-host (request-socket req))))))
   
   (set-time-last-touched!
    ()
    (the (set-slot! :time-last-touched (get-universal-time))))
   
   (set-instantiation-time!
    ()
    (the (set-slot! :time-instantiated (get-universal-time))))
   
   ;;
   ;; FLAG -- make sure the instance-urls are in fact being
   ;; unpublished whenever we do an update! of a base-html-sheet.
   ;;
   
   ("NIL or error object. This function checks the \"sanity\" of this object. By 
default, it checks that following the object's root-path from the root resolves
to this object. If the act of following the root-path throws an error, this error
will be returned. Otherwise, if the result of following the root-path does not
match the identity of this object, an error is thrown indicating this. Otherwise,
NIL is returned and no error is thrown. You can override this function to do what 
you wish. It should return NIL if the object is found to be \"sane\" and an throw
an error otherwise.

If check-sanity? is set to T in this object, this function will be invoked automatically
within an ignore-errors by the function handling the GWL \"/answer\" form action URI 
when this object is a respondent, before the main-sheet is presented."
    check-sanity
    ()
    (let ((sanity (the root (follow-root-path (the root-path)))))
      (when (not (eql self sanity))
        (error "Object identity does not match its root-path (failed sanity check)."))))
   
   ("Void. Emits a page explaining the sanity error. This will be invoked instead of the write-main-sheet
if check-sanity? is set to T and the check-sanity throws an error. You may override this function to
do what you wish. By default a minimal error message is displayed and a link to the root object
is presented.

:arguments (error \"an error object, presumably from the <tt>check-sanity</tt> function.\")"
    sanity-error 
    (error)
    (html (:html (:head (:title "Error computing response page"))
                 (:body (:p "The expected Respondent object could not be presented. 

The error was: "
                            (:pre (:princ-safe error)))
                        (when (ignore-errors (the root-path))
                          (html (:p "The reference chain to the expected Respondent was: "
                                    (:pre (:prin1-safe (cons 'the (reverse (the root-path))))))))
                        (:p "Click " 
                            (:b (the root (write-self-link :display-string "Here")))
                            " to visit the root of the site hierarchy.")))))
   
   ("Void. This is an empty function by default, but can be overridden in
a user specialization of base-html-sheet, to do some processing before the 
header-plist is evaluated and before the HTTP response is actually initiated, but after
the cookies-received have been set."
    process-cookies! ())
   
   ("Void. This is an empty function by default, but can be overridden in
a user specialization of base-html-sheet, to do some processing before the 
header-plist is evaluated and before the HTTP response is actually initiated."
    before-response! ())
   
   
   ("Void. This is an empty function by default, but can be overridden in
the respondent of a form, to do some processing before the respondent's 
<tt>write-html-sheet</tt> function runs to present the object. This can be
useful especially for objects which are subclasses of higher-level mixins such as 
<tt>application-mixin</tt> and <tt>node-mixin</tt>, where you do not have
direct access to the <tt>write-html-sheet</tt> function and typically only define
the <tt>model-inputs</tt> function. It is not always reliable to do processing
in the <tt>model-inputs</tt> function, since some slots which depend on your
intended modifications may already have been evaluated by the time the
<tt>model-inputs</tt> function runs."
    before-present! ())

   
   
   
   ("Void. This is an empty function by default, but can be overridden in
the respondent of a form, to do some processing after the respondent's 
<tt>write-html-sheet</tt> function runs to present the object."
    after-present! ())
   
   
   ("Void. This is an empty function by default, but can be overridden in
the requestor of a form, to do some processing before the requestor's form
values are set into the specified bashee."
    before-set! ())   
   
   
   ("Void. This is an empty function by default, but can be overridden in
the requestor of a form, to do some processing after the requestor's form
values are set into the specified bashee."
    after-set! ())

   
   
   (self-link
    (&key (display-string (the strings-for-display))
          (display-color nil)
          (target nil)
          (title nil)
          class id
          on-click
          on-mouse-over on-mouse-out
          local-anchor)
    (with-cl-who-string ()
      ((:a :href 
           (if local-anchor (format nil "~a#~a" 
                                    (the url) local-anchor) (the :url))
           :target target
           :onmouseover on-mouse-over 
           :onmouseout on-mouse-out
           :title title
           :class class
           :onclick on-click
           :id id)
       (if display-color
           (htm ((:font :color display-color) (str display-string)))
         (str display-string)))))
   
      
   ("Void. Emits a hyperlink pointing to self. Note that if you need extra customization
on the display-string (e.g. to include an image tag or other arbitrary markup), use with-output-to-string
in conjunction with the html-stream macro.

:&key ((display-string (the :strings-for-display)) \"String. String to be displayed.\"
       (display-color nil) \"Keyword symbol or HTML color string. Determines the color of the displayed link text. Default of NIL indicates web browser default (usually blue).\"
       (target (the :target)) \"String. Names a frame or window to open the link when clicked.\"
       (class nil) \"String. Names a stylesheet class.\"
       (id nil) \"String. Names a stylesheet id.\"
       (on-mouse-over nil) \"String. Javascript code to run on mouse over.\"
       (on-mouse-out nil) \"String. Javascript code to run on mouse out.\")"
    write-self-link
    (&key (display-string (the :strings-for-display)) 
          (display-color nil)
          (title nil)
          (target (the :target))
          class id
          local-anchor
          on-click
          on-mouse-over 
          on-mouse-out)
    (with-format (html-format *stream*)
      (write-the (self-link :display-string display-string
                            :display-color (when display-color (lookup-color display-color :format :hex))
                            :target target
                            :title title
                            :class class :id id
                            :local-anchor local-anchor
                            :on-click on-click
                            :on-mouse-over on-mouse-over :on-mouse-out on-mouse-out))))

   
   (write-back-link
    (&key (display-string (if (and (the return-object) (the return-object strings-for-display))
                              (the return-object strings-for-display)
                            "&lt;-Back"))
          (display-color nil)
          (title nil)
          (target (the :target))
          class id
          local-anchor
          on-mouse-over on-mouse-out)
    (when (the return-object)
      (the return-object (write-self-link
                          :display-string display-string
                          :display-color (when display-color (lookup-color display-color :format :hex))
                          :target target
                          :title title
                          :class class :id id
                          :local-anchor local-anchor
                          :on-mouse-over on-mouse-over :on-mouse-out on-mouse-out))))
   

   ("Void. Creates a default unordered list with links to each child part of self.
The text of the links will come from each child's strings-for-display."
    write-child-links
    nil
    (html (:ul
           (mapc #'(lambda (child)
                     (html (:li (the-object child :write-self-link))))
                 (the :children)))))


   
   (devo-links-string
    ()
    (with-output-to-string (*html-stream*)
      (with-format (html-format *html-stream*)
        (write-the local-development-links))))
      
   
   ("Void. Writes links for access to the standard developer views of the object, currently consisting 
of an update (Refresh!) link, a Break link, and a ta2 link."
    write-development-links
    ()
    (let ((*stream* (or *html-stream* *stream*)))
      (with-format (html-format *stream*) (write-the development-links))))

   
   ;;
   ;; FLAG -- superseded by form-controls
   ;;      -- but still used in bus demo, need to rewrite that interface!!
   ;;
   ("Void. Writes an HTML Select field with Options.
:&key ((size 1) \"Integer. determines size of selection list. Default of 1 is a pulldown menu.\"
       name \"Keyword symbol or string. Determines the name of the field, which should probably match a settable computed-slot.\"
       keys \"List of strings, numbers, or symbols. Values, the selected one of which will be returned as the value of the field.\"
       (values keys) \"List of strings. Keys to display in the selection-list.\"
       tabindex \"Integer. If given, this will generate the tabindex tag for this HTML input field.\")"
    select-choices
    (&key (size 1) name keys (values keys) tabindex (use-default? t))
    (html ((:select :name name :size size :if* tabindex :tabindex tabindex)
           (mapc #'(lambda (key value)
                     (html ((:option :value key :if*
                                     (and use-default? (equalp key (the (evaluate name)))) 
                                     :selected :selected)
                            (:princ value))))
                 keys values))))


   ("Void. Writes some standard footer information. Defaults to writing Genworks and Franz 
copyright and product links. Note that VAR agreements often require that you include a ``powered by'' 
link to the vendor on public web pages."
    write-standard-footer
    (&key (include-copyright? t) (copyright-only? nil))
    (html ((:p :class "copyrightFooter")
           (when (or include-copyright? copyright-only?)
             (html "Copyright &copy; "
                   (multiple-value-bind (seconds
                                         minutes
                                         hours
                                         date
                                         month
                                         year)
                       (get-decoded-time)
                     (declare (ignore seconds minutes hours date month))
                     (html (:princ year)))
                   " "
                   ((:a :href "http://www.genworks.com") "Genworks")
                   (:sup "&reg;") " "
                   ((:a :href "http://www.genworks.com")
                    "International") ". "))
           (when copyright-only? (html "All rights reserved."))
           (when (not copyright-only?)
             (html "This site is powered by "
                   ((:a :href "http://www.franz.com")
                    "Allegro CL")
                   " and "
                   ((:a :href "http://www.genworks.com")
                    "Genworks")
                   " GDL.")))))

   ("Void. This GDL function should be redefined to generate the HTML page corresponding to this object.
It can be specified here, or as the <tt>main-sheet</tt> output-function in an html-format lens for this
object's type. This <tt>write-html-sheet</tt> function, if defined,  will override any <tt>main-sheet</tt>
function defined in the lens. Typically a <tt>write-html-sheet</tt> function would look as follows:

:example
<pre>
 (write-html-sheet
  ()
  (html (:html (:head (:title (:princ (the :page-title))))
               (:body ;;; fill in your body here

               ))))
</pre>"
    write-html-sheet
    ()
    (with-format (html-format *html-stream*) (write-the (main-sheet)))
    )))




