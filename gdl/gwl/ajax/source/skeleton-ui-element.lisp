;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
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

(define-object sheet-section (skeleton-ui-element)
  :documentation (:description "Basic mixin to support an object 
representing a section of an HTML sheet (i.e. web page). Currently 
this simply mixes in skeleton-ui-element, and the functionality is not 
extended. Sheet-section is also mixed into base-html-sheet, so it and 
any of its subclasses will be considered as sheet-sections if they 
are the child of a base-ajax-sheet.

"
                  
                  
                  :examples "<pre>

 FLAG -- fill in!!!


</pre>"))



(define-object skeleton-ui-element ()
  
  :documentation (:description "Basic mixin to support constructing a gdl ajax call 
relative to this node. Note that in order for a node to represent a section of a 
web page, you should use sheet-section (which mixes this in), rather than this raw 
primitive. 

This is a mixin into base-html-sheet, and some of the previous base-html-sheet 
functionality has been factored out into this mixin. 

Of special note in this object is thefunction <tt>gdl-ajax-call</tt> which generates 
Javascript appropriate for attaching with a UI event, e.g. onclick, onchange, 
onblur, etc. In this Javascript you can specify a GDL function (on this object, self) 
to be run, and/or specify a list of form-control objects which are rendered on 
the current page, whose values should be submitted and processed (\"bashed\") into the 
server."
                  
                  :examples "<pre>

 FLAG -- Fill in!!!

</pre>")
  
  :input-slots
  ((orientation nil)
   (center #(0.0 0.0 0.0))

   (div-class nil)


   ("List of GDL objects of type form-control. The validation-function will be forced 
on these objects when a form is submitted, even if the object's html form-control does 
not happen to be included in the values submitted with the form. Defaults to nil."
    force-validation-for nil)
   
   
   ("GDL Object. Object to have its settable computed-slots and/or query-plist set 
from the fields on the form upon submission. Defaults to self."
    bashee self)
   
   ("GDL Object. Object to respond to the form submission. Defaults to self."
    respondent self :defaulting)
   

   ("String. This can be used with (str ...) [in cl-who] or (:princ ...) [in htmlGen] 
to output this section of the page, without the wrapping :div tag [so if you use this, 
your code would be responsible for wrapping the :div tag with :id (the dom-id).]"
    inner-html (progn (the view-toggle) (with-cl-who-string () 
					  (let ((*html-stream* *stream*))
					    (write-the inner-html)))))
   
   
   ("String of valid Javascript. This Javascript will be send with the Ajax response,
and evaluated after the innerHTML for this section has been replaced." 
    js-to-eval nil)
   
   
   (js-always-to-eval nil) 
   

   ("String. This is the auto-computed dom-id which should be used for rendering 
this section. If you use the main-div HTML string for rendering this object as a 
page section, then you do not have to generate the :div tag yourself - the main-div 
will be a string of HTML which is wrapped in the correct :div tag already."
    dom-id (the base64-encoded-root-path))

   )
  
  
  :trickle-down-slots (respondent)

  
  :computed-slots 
  ((view-toggle nil :settable)
   
   (base64-encoded-root-path 
    (base64-encode-safe 
     (format nil "~s" (remove :root-object-object (the root-path)))))
   
   
   
   
   (main-div (progn (when *debug?* (print-variables (the respondent)))
                    (the respondent (register-html-section! self))
                    (the main-div%))
             :uncached)
   
   ("String. This should be used with (str ...) [in cl-who] or (:princ ...) 
[in htmlGen] to output this section of the page, including the wrapping :div tag."
    main-div% (with-cl-who-string ()
               (write-the main-div)))
   
   (url-encoded-root-path (root-path-to-query-arg (the :root-path)))
   
   
   (%html-section-root-paths% nil :settable)
   
   
   ("List of HTML sections to be scanned and possibly replaced in response to 
GDL Ajax calls. Override this slot at your own risk. The default is all 
sections who are most recently laid out on the respondent sheet, and 
this is set programmatically every time the sheet section's main-div 
is demanded."
    html-sections 
    (mapcar #'(lambda(section-root-path)
                (the root (follow-root-path section-root-path)))
            (the %html-section-root-paths%)))
   
   
   ("List of GDL objects. All the children or hidden-children 
of type base-form-control." 
    form-controls 
    (remove-duplicates 
     (remove-if-not 
      #'(lambda(child) (typep child 'base-form-control))
      (append (the ordered-form-controls)
              (the children)
                                          
              (with-error-handling ()
                (apply #'append
                       (mapcar 
                        #'(lambda(child)
                            (when (typep child 'skeleton-form-control)
                              (the-object child form-controls)))
                        (the children))))
                                          
              (the hidden-children)
                                          
              (with-error-handling ()
                (apply #'append
                       (mapcar #'(lambda(child)
                                   (when (typep child 'skeleton-form-control)
                                     (the-object child form-controls)))
                               (the hidden-children))))))
     :from-end t))
   
   ("List of GDL objects. All the form-controls which do not pass validation."
    failed-form-controls 
    (remove-if-not #'(lambda(form-control) (the-object form-control error))
                   (the form-controls)))
   
   
   
   ("List of GDL objects, which should be of type 'base-form-control. 

<p>
[Note -- this slot is not really necessary for protecting out-of-bounds sequence references 
anymore, the form-control processor protects against this by itself now].
</p>

These objects are validated and bashed first, in the order given. If the cardinality 
of one form-control depends on another as in the example below, then you should list 
those dependent objects first. Default is nil.


 :examples
<pre>
 
...
  
  :computed-slots ((number-of-nozzles (the number-of-nozzles-form value))
	    (ordered-form-controls 
	     (append (list-elements (the inner-flange-form))
		     (list (the number-of-nozzles-form)))))
  
  :objects
  ((inner-flange-form
    :type 'menu-form-control 
    :choice-plist (list :hey \"hey\" :now \"now\")
    :default :hey
    :sequence (:size (the number-of-nozzles)))

   (number-of-nozzles-form
    :type 'text-form-control 
    :prompt \"Number of Shell Nozzles Required: \"
    :domain :number     
    :default 0)


</pre>

"
    ordered-form-controls nil)
   
   ("Boolean. This switch determines whether all form-controls should be preset 
before the final setting, in order to allow any interdependencies to be detected 
for validation or detecting changed values. If this is specified as a non-nil 
value, then any nil values of (the preset?) on individual form controls will be 
ignored. If this is specified as nil, then (the preset?) of individual 
form-controls (default of these is also nil) will be respected. Default is nil."
    preset-all? nil)
   
   ("List of keyword symbols. Messages corresponding to form fields which could 
be missing from form submission (e.g. checkbox fields). Defaults to the names 
of any children or hidden-children of type  menu-form-control or 
checkbox-form-control."
    possible-nils (mapcar #'(lambda(object) (the-object object field-name))
                          (remove-if-not #'(lambda(child)
                                             (or (and (typep child 'menu-form-control)
                                                      (the-object child possible-nil?))
                                                 (and (typep child 'checkbox-form-control)
                                                      (the-object child possible-nil?))))
                                         (append (the children) (the hidden-children))))))
   

  
  :functions
  ((relative-url-to 
    (url)
    (glisp:replace-regexp (namestring (relativize-pathname url (the url))) "\\" "/"))

   (register-html-section!
    (section)
    
    (when *debug?*
      (format *trace-output* "~&Before:~%")
      (print-messages %html-section-root-paths%))
    
    (let ((html-sections (copy-list (the %html-section-root-paths%))))
      (the (set-slot! :%html-section-root-paths% (pushnew (the-object section root-path)
                                                html-sections 
                                                :test #'equalp)
                      :remember? nil)))
    
    (when *debug?*
      (format *trace-output* "~&After:~%")
      (print-messages %html-section-root-paths%)))

    
   
   (gdl-sjax-call 
    (&rest args)
    (the (gdl-ajax-call (:apply (append (list :asynch? nil) args)))))

   

   
   ("String. 

This function returns a string of Javascript, appropriate to use for events 
such as :onclick, :onchange, etc, which will invoke an Ajax request to the 
server, which will respond by replacing the innerHTML of affected :div's, and 
running the Javascript interpreter to evaluate (the js-to-eval), if any.

:examples \"<pre>

 FLAG -- Fill in!!!

</pre>\"
 
:&key ((bashee (the bashee)) \"GDL Object. This object will have the function-key 
                               called on it, if any.\"
       (respondent (the respondent)) \"GDL Object. This must be the object which 
                                       represents the actual web page being used.\"
       (function-key nil) \"Keyword symbol. This keyword symbol must name a 
                            GDL function or method which is to be invoked with 
                            the Ajax call.\"
       (arguments nil) \"List of values. This is the argument list on which the 
                         function named by function-key will be applied.\"
       (form-controls nil) \"List of GDL objects of type base-form-control. 
                             Each of the objects in this list will have its 
                             current value (as entered by the user) scraped 
                             from the web page and its value in the 
                             model \"bashed\" to reflect what has been 
                             entered on the page.\")"
    gdl-ajax-call 
    (&key (bashee (the bashee)) 
          (respondent (the respondent)) 
          function-key 
          arguments
          form-controls
          (asynch? t))
    


    (format 
     nil "gdlAjax(event, 'args=~a~a, ~a);"
     (the (encode-ajax-args :bashee bashee
                            :respondent respondent
                            :function-key function-key
                            :arguments arguments))
     (let ((string
            (if form-controls
                (string-append 
                 "&fields=' + "
                 (format 
                  nil "encode64('(' + ~{~a~^ + ' ' + ~} + ')').replace(/=/g,'')"
                  (mapcar 
                   #'(lambda(form-control) 
                       (when *debug?* (print-variables form-control))
                       (string-append
                        
                        (if (not (or (typep form-control 'radio-form-control)
                                     (typep form-control 'menu-form-control)))
                            (format nil "'~s ' + "
                                    (the-object form-control field-name)) "")

                        (or
                         (typecase form-control 
                           (radio-form-control
                            (format nil "~{~a~^ + ~}"
                                    (mapcar 
                                     #'(lambda(n)
                                         (format nil "' :|radio-~a-~a| (' + doublequote + document.getElementById('~a-~a').value + doublequote + ' ' + doublequote + document.getElementById('~a-~a').checked + doublequote + ')' "
                                                 (the-object form-control field-name) n
                                                 (the-object form-control id) n
                                                 (the-object form-control id) n))
                                     (list-of-numbers 
                                      0 
                                      (1- (length 
                                           (plist-keys 
                                            (the-object form-control 
                                                        effective-choice-plist))))))))
                          
                           (checkbox-form-control 
                            (format nil "doublequote +  encode64(document.getElementById('~a').value).replace(/=/g,'') + doublequote + ' ~s-checkedp ' + doublequote + document.getElementById('~a').checked + doublequote"
                                    (the-object form-control id)
                                    (the-object form-control field-name)
                                    (the-object form-control id)))
                          
                          
                           (menu-form-control 
                            (format nil "collectMenuSelections(document.getElementById('~a'))"
                                    (the-object form-control id))))
                         
                         (format nil "doublequote + encode64(document.getElementById('~a').value).replace(/=/g,'') + doublequote"
                                    (the-object form-control id)))))
                                            
                   (ensure-list form-controls))))
              "'"
              )))
       string)
     
     (if asynch? "true" "false")))
   
   
   #|
   
    selected = new Array();
    for (var i = 0; i < ob.options.length; i++)
    if (ob.options[ i ].selected)
    selected.push(ob.options[ i ].value);

    |#
   
   (encode-ajax-args
    (&key (bashee (the bashee)) (respondent (the respondent)) function-key arguments)
    (base64-encode-safe 
     
     (with-output-to-string (ss)
       (pprint 
        (encode-for-ajax (append (list :|iid| (the-object respondent instance-id)
                                       :|bashee| bashee)
                                 (unless (eql respondent bashee)
                                   (list :|respondent| respondent))
                                 (when function-key
                                   (list :|function| function-key))
                                 (when arguments 
                                   (list :|arguments| arguments)))) ss))))
   
   (toggle-view-toggle! () (the (set-slot! :view-toggle (not (the view-toggle)))))))


(define-lens (html-format skeleton-ui-element)()
  :output-functions
  (
   (inner-html
    ()
    (with-cl-who ()
      (fmt "~a Has no inner-html defined." (the strings-for-display))))
   

   
   (main-div 
    ()
    (with-cl-who ()
      ((:div :id (the dom-id) :class (the div-class))
       (str (the  inner-html)))))))



;;
;; from http://paste.lisp.org/display/125460
;;
(defun relativize-pathname (target-pathname relative-to-pathname)
  "Return a relative pathname for TARGET-PATHNAME that can be reached                                                                    
  from the directory that TARGET-PATHNAME refers to."
  (let* ((target-directory (pathname-directory (merge-pathnames target-pathname)))
         (relative-to-directory (pathname-directory (merge-pathnames relative-to-pathname)))
         (root-length (mismatch target-directory relative-to-directory :test #'equal)))
    (if root-length
        (merge-pathnames (enough-namestring target-pathname
					    (make-pathname :name nil :type nil
							   :directory (subseq target-directory 0 root-length)))
                         (make-pathname :name nil :type nil
                                        :directory `(:relative ,@(loop for i below (- (length relative-to-directory) root-length)
                                                                       collect :up))))
        (make-pathname :directory nil :defaults target-pathname))))

