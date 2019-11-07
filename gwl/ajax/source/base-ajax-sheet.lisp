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



(defmacro w-c-w-s ((&rest args) &rest body) `(with-cl-who-string ,args ,@body))

(defun prepend-url-depth (string depth)
  (string-append (format nil "~v@{~A~:*~}" depth "../") string))


(define-object base-ajax-sheet (base-html-sheet)
  :documentation (:description "(Note: this documentation will be moved
to the specific docs for the html-format/base-ajax-sheet lens, when 
we have lens documentation working properly)

Produces a standard main-sheet for html-format which includes the
standard GDL Javascript to enable code produced with gdl-ajax-call to
work, and optionally to include the standard JQuery library.

If you want to define your own main-sheet, then there is no use for
base-ajax-sheet, you can just use base-html-sheet. But then you have
to include any needed Javascript yourself, e.g. for gdl-ajax-call
support or jQuery.

The html-format lens for base-ajax-sheet also defines a user hook function,
main-sheet-body, which produces a \"No Body has been defined\" message by default, 
but which you can fill in your own specific lens to do something useful for the body."

                  
                               :examples "<pre>
 (in-package :gdl-user)

 (gwl:define-package :ajax-test (:export #:assembly))

 (in-package :ajax-test)

 (define-object assembly (base-ajax-sheet)

   :objects
   ((inputs-section :type 'inputs-section)

    (outputs-section :type 'outputs-section
                     :box (the viewport box)
                     :color (the inputs-section color))
   
    (viewport :type 'viewport
              :box-color (the inputs-section color))))

 (define-lens (html-format assembly)()
   :output-functions
   ((main-sheet-body
     ()
     (with-cl-who ()
       (:table 
        (:tr
         (:td (str (the inputs-section main-div)))
         (:td (str (the outputs-section main-div)))
         (:td (str (the viewport main-div)))))))))

 (define-object inputs-section (sheet-section)

   :computed-slots ((color (the menu-control value)))
  
   :objects
   ((menu-control :type 'menu-form-control
                  :choice-list (list :red :green :blue)
                  :default :red
                  :onchange (the (gdl-ajax-call 
                                  :form-controls (list (the-child)))))
   
    (little-grid :type 'grid-form-control
                 :form-control-types '(text-form-control 
                                       text-form-control 
                                       button-form-control)
                 :form-control-attributes '((:ajax-submit-on-change? t)
                                            (:ajax-submit-on-change? t))
                 :form-control-inputs 
                 (mapcar #'(lambda(row)
                             (list nil nil 
                                   (list :onclick 
                                         (the (gdl-ajax-call 
                                               :function-key :do-something!
                                               :arguments 
                                               (list (the-object row index)))))))
                         (list-elements (the-child rows)))
                 :default '((:color :number :press-me)
                            (:red 42 \"OK\")
                            (:blue 50 \"OK\"))))
   
   :computed-slots 
   ((inner-html (with-cl-who-string ()
                 (str (the little-grid form-control-string))
                 (str (the menu-control html-string)))))
   
   :functions
   ((do-something! (index)
      (format t \"Processing row ~a...~%\" index))))




 (define-object outputs-section (sheet-section)
   
   :input-slots (color box)
  
   :computed-slots 
   ((inner-html (with-cl-who-string ()
                 (:p \"The box volume is: \" (fmt \"~a\" (the box volume)))
                 (:p \"The box color is: \" 
                     ((:span :style (format nil \"color: ~a\" (the color)))
                      (str (the color))))))))



 (define-object viewport (base-ajax-graphics-sheet)
  
   :input-slots (box-color)

   :computed-slots ((length 300)
                    (width 300)
                    (display-list-objects (list (the box)))
                    (projection-vector (getf *standard-views* 
                                             (the view-selector value)))
                    (inner-html
                     (with-cl-who-string ()
                       (str (the view-selector html-string))
                       (str (the reset-zoom-button form-control-string))
                       (str (the raster-graphics)))))
  
   :objects ((box :type 'box 
                  :length 20 :width 25 :height 30
                  :display-controls (list :color (the box-color)))))
   
 (publish-gwl-app \"/ajax-test\" \"ajax-test:assembly\")

</pre>")

  :input-slots ((local-assets? t)

		(url-depth 0)


		("String of HTML. The main body of the page. 
This can be specified as input or overridden in subclass, otherwise it defaults
to the content produced by the :output-function of the same name 
in the applicable lens for  html-format."
                 main-sheet-body (with-output-to-string (*stream*)
                                   (with-format (html-format *stream*)
                                     (write-the main-sheet-body))))
                
                ("String or nil. Names the value of class attribute for the body tag. Default is nil."
                 body-class nil)

                ("String of Javascript or nil. This Javascript will go into the :onload event of the body.
Default is nil."
		 body-onload nil)

		("String of Javascript or nil. This Javascript will go into the :onpageshow event of the body.
Default is nil."
		 body-onpageshow nil)

		("String of Javascript or nil. This Javascript will go into the :onresize event of the body.
Default is nil."
		 body-onresize nil)
                
                ("String or nil. Contains the string for the doctype at the top of the document. 
Default is the standard doctype for HTML5 and later."

                 doctype-string "<!DOCTYPE HTML>")

		(use-ajax? t ) ;; of course because this is base-ajax-sheet. But we can override if needed. 
		
                ("Boolean. Include jquery javascript libraries in the page header? 
Default nil."  
                 use-jquery? nil :settable)
                
                (use-raphael? nil)
		(use-svgpanzoom? nil)
		(use-x3dom? nil)
		(use-fontawesome? nil)
		(use-anyresize? nil)

		
		(include-default-favicon? t)

                
                ("String. The title of the web page. Defaults to \"Genworks GDL -\"
.followed by the strings-for-display."
                 title (format nil "Genworks GDL - ~a" (the strings-for-display)))
                
                ("String of valid HTML. Additional tag content to go into the page header,
if you use the default main-sheet message and just fill in your own main-sheet-body, as 
is the intended use of the base-ajax-sheet primitive."
                 additional-header-content nil :settable)

                ;; FLAG JB-100203 added an input slot to the base-ajax-sheet in order for the end user
                ;; to supply an additional set of javascript JUST BEFORE THE BODY
                ;; this is enable the user to add jquery code '$(document).ready(function () { /// };'
                ;; and execute parts of this. This is complementary of the ui-specific-layout-js, which can
                ;; only be an external js file. This is inline javascript
                ("valid javascript. This javascript is added to the head of the page, just before the body."

                 additional-header-js-content nil :settable)
                

                ("Absolute URI in the browser.
This is additional JavaScript that needs to be loaded in order to initiate the layout of a user 
interface. Defaults to nil."
                 ui-specific-layout-js nil :settable)
                
                
                ;;(respondent (the bashee) :defaulting) 
		(respondent (the bashee))
                
                )

  
  :trickle-down-slots (respondent)
  
  
  :computed-slots (

                   
                   
                   ("String of HTML. Provides the developer control links for current sheet."
		    development-links
                    (with-cl-who-string () (write-the development-links))))

  :hidden-objects
  ((standard-javascript-section
    :type 'sheet-section
    :js-to-eval :parse
    :inner-html
    (with-cl-who-string (:indent t)
      (when (the use-jquery?)
	(htm 
	 ((:script :src "/static/3rdpty/jquery/js/jquery.min.js"))
	 ((:script :src "/static/3rdpty/jquery/js/jquery-ui.min.js"))
	 ((:script :src "/static/3rdpty/jquery/js/jquery.layout.min.js"))
	 ((:script :src "/static/3rdpty/jquery/js/superfish.min.js"))
	 ((:script :src "/static/3rdpty/jquery/js/jquery.bgiframe.js"))))
      
      
      (when (the use-x3dom?)
	(htm ((:script :src (if (the local-assets?)
				"/static/3rdpty/x3dom/x3dom.js"
				"https://www.x3dom.org/download/1.7.2/x3dom.js")
		       :id "x3dom_script"))
	     #+nil
	     ((:link :href (if (the local-assets?)
			       "/static/3rdpty/x3dom/x3dom.css"
			       "https://www.x3dom.org/download/1.7.2/x3dom.css")
		     :rel "stylesheet"))))


      (when (the use-raphael?)
	(htm ((:script
	       :id "raphael-script"
	       :src (if (the local-assets?)
			"/static/3rdpty/raphael/js/raphael-min.js"
			"https://cdnjs.cloudflare.com/ajax/libs/raphael/2.2.7/raphael.min.js")))))


      (when (the use-svgpanzoom?)
	(htm ((:script
	       :id "svg-panzoom"
	       :src (if (the local-assets?)
			"/static/3rdpty/svgpanzoom/svg-pan-zoom.min.js"
			"https://cdn.jsdelivr.net/npm/svg-pan-zoom@3.5.0/dist/svg-pan-zoom.min.js")))))
			   

      (when (the use-fontawesome?)
	(htm ((:link :id "fontawesome-css"
		     :rel "stylesheet"
		     :href (if (the local-assets?)
			       "/static/3rdpty/fa/css/all.min.css"
			       "https://use.fontawesome.com/releases/v5.3.1/css/all.css")
		     :integrity (unless (the local-assets?)
				  "sha384-mzrmE5qonljUremFsqc01SB46JvROS7bZs3IO2EmfFsd15uHvIt+Y8vEf7N7fWAU")
		     :crossorigin "anonymous"))))

      (when (the use-anyresize?)
	(htm ((:script :id "anyresize-script"
		       :src (if (the local-assets?)
				"/static/3rdpty/resize/any-resize-event.js"
				"https://is.gd/sAeEPt")))))

      (when (the use-ajax?)
	(htm
	 ((:script) (fmt "~%var gdliid = '~a';" (the instance-id)))
	 
	 ((:script :src (if (the local-assets?)
			    "/static/gwl/js/gdlajax1593g.js"
			    "https://genworks.com/static/gwl/js/gdlajax1593g.js")))))
      
      (when (the ui-specific-layout-js)
	(htm
	 ((:script :type "text/javascript"
		   :src (the ui-specific-layout-js))))))))
  

   :functions ((prepend-url-depth
		(string)
		(prepend-url-depth string (the url-depth)))

	       (back-link 
		(&key (display-string "&lt;-Back"))
		(w-c-w-s 
                 () 
                 ((:a :href (the return-object url)) (str display-string))))
              
               (update-root! () 
                             (unpublish-instance-urls (the instance-id) (the url))
                             (the root update!)
                             (the url)
                             )

               (reset-html-sections! 
		()
		;;(the (restore-slot-default! :%html-sections%))
		)
              
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
                  (write-the (self-link 
                              :display-string display-string
                              :display-color display-color
                              :target target 
                              :title title
                              :class class
                              :id id
                              :on-click on-click
                              :on-mouse-over on-mouse-over
                              :on-mouse-out on-mouse-out
                              :local-anchor local-anchor))))
              
               (set-self () 
                         (if *break-on-set-self?*
                             (progn (set-self self)
                                    (let ((*package* (symbol-package (the type)))) (break)))
                             (progn
                               (set-self self))))

	       ("Void. This is a hook function which applications can use to restore automatically 
from a saved snapshot file."
		custom-snap-restore!
		()

		)

	       ))


(define-lens (html-format base-ajax-sheet)()
  
  :output-functions
  ((main-sheet
    ()
    (with-cl-who ()
      (str (the doctype-string))
      ((:html :lang "en")
       (:head (:title (str (the title)))
              (:meta :charset "UTF-8")
	      (when (the include-default-favicon?)
		(htm (:link :rel "icon" :type "image/x-icon" :href "/static/gwl/images/favicon.ico")))
              (when (the additional-header-content) (str (the additional-header-content)))
              (str (the standard-javascript-section main-div))
              (when (the additional-header-js-content)
                (str (the additional-header-js-content))))
       
       ((:body :class (the body-class)
	       :onpageshow (the body-onpageshow)
               :onload (the body-onload)
	       :onresize (the body-onresize))
        (the reset-html-sections!)

        (str (the main-sheet-body))
        
        #+nil
        ((:div :id (the dom-id))
         (str (the main-sheet-body))
         )
        ))))

   
   (development-links 
    ()
    (with-cl-who ()
      (:p (write-the update-full-link)
          " | "
          (write-the break-link)
          
          )))
   
   (self-link
    (&key (display-string (the strings-for-display))
          (display-color nil)
          (target nil)
          (title nil)
          class id
          on-click
          on-mouse-over on-mouse-out
          local-anchor)
    (with-cl-who ()
      ((:a :href (if local-anchor (format nil "~a#~a" (the url) local-anchor) (the :url))
           :target target 
           :onmouseover on-mouse-over 
           :onmouseout on-mouse-out
           :title title
           :class class
           :onclick on-click
           :id id)
       (if display-color
           (htm ((:font :color display-color) (str display-string)))
         (htm (str display-string))))))
   
   
   (break-link
    (&key (display-string "SetSelf"))
    (with-cl-who ()
      ((:span :style "color: blue; cursor: pointer;" :onclick (the (gdl-ajax-call :function-key :set-self)))
       (str display-string))))
   

   (update-full-link 
    ()
    (with-cl-who ()
      ((:span :style "color: blue; cursor: pointer;"
              :onclick (string-append (the (gdl-sjax-call :function-key :update-root!))
                                      " location.reload(true);"
                                      ))
       "Update!")))


   
   (main-sheet-body
    ()
    (with-html-output (*stream*)
      "No Body has been defined."))))




