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

(define-object base-ajax-graphics-sheet 
    (base-ajax-sheet base-html-graphics-sheet)
  
  
  :documentation (:description "This mixes together base-ajax-sheet 
with base-html-graphics-sheet, and adds html-format output-functions 
for several of the new formats such as ajax-enabled png/jpeg and 
Raphael vector graphics."
                  
                  :examples "FLAG -- Fill in!!!")
  
  :input-slots 
  (("Number. Thickness of default border around graphics viewport. 
Default is 1." viewport-border-default 1)
   
   (vector-graphics-onclick? t)
   
   ("Plist of keys and strings. The default formats for graphics display. Defaults to:

 <pre>
 (list :png \"PNG image\"
       :jpeg \"jpeg image\"
       :raphael \"SVG/VML\")
</pre>"
    image-format-plist (list :png "PNG image"
                             :jpeg "jpeg image"
                             :web3d "VRML/X3D"
                             :raphael "SVG/VML"))

   
   ("Keyword symbol, one of the keys from (the image-format-plist). 
Default for the image-format-selector. Defaults to :png."
    image-format-default :png)
   
   
   ("Default view initially in the view-selector which is 
automatically included in the view-controls."
    view-direction-default :trimetric)
   
   (zoom-factor-renderer 1)
   
   ("Boolean. Include raphael javascript library in the page header? 
Default nil. " 
    use-raphael? nil)
   
   ("Boolean. Include raphael graphing library in the page header? 
Default nil. " 
    use-raphael-graf? nil)
   
   ("List of GDL objects containing geometry. These are the 
actual objects themselves, not nodes which have children or 
other descendants that you want to display. If you want to 
display the leaves of certain nodes, include the objects for 
those nodes in the display-list-object-roots, not here. 
Defaults to nil." 
    display-list-objects nil)
   
   
   ("List of GDL objects. The leaves of each of these objects will 
be included in the geometry display. Defaults to nil." 
    display-list-object-roots nil)
   
   ("3D vector. This is the normal vector of the view plane onto 
which to project the 3D objects. Defaults to 
  (getf *standard-views* (the view-selector value)),
and (the view-selector value) defaults to :top."
    projection-vector (getf *standard-views* (the view-selector value)))
   
   
   ("List of GDL objects. These objects are not used in 
computing the scale or centering for the display list. 
Defaults to nil."
    immune-objects nil)
   
   
   ("Array of three numbers between 0 and 1. RGB Color in decimal 
format. Color to be used for the background of the viewport. 
Defaults to the 
 <tt>:background</tt> from the global <tt>*colors-default*</tt> parameter."
    background-color (lookup-color (getf *colors-default* :background) :format :decimal))

   ("Number in angular degrees. The maximum angle of the view frustrum 
for perspective views. Defaults to 45 which is natural human eye field of view. "
    field-of-view-default 45)
   
   
   ("Keyword symbol. Determines the default image format. Defaults to the currently selected
value of the image-format-selector, which itself defaults to :raphael."
    image-format (the image-format-selector value))
   
   
   ("Boolean. Indicates whether standard view-controls panel should be included with the graphics."
    include-view-controls? t)
   
   (x3dom-view-controls? t)

   ;;
   ;; FLAG -- probably not needed, inherited from skeleton-ui-mixin via base-ajax-sheet, base-html-sheet, sheet-section. 
   ;;
   (inner-html (with-cl-who-string () (write-the inner-html)))


   )

  
  :functions 
  ((set-js-vals! (js-vals) (declare (ignore js-vals))))

  :computed-slots
  (

   (js-to-eval :parse :settable)
   
   (js-always-to-eval nil)
                     
   
   (raphael-canvas-id (format nil "raphael-~a" (the base64-encoded-root-path)))
   
   (raphael-string  (unless (the no-graphics?)
		      (with-error-handling ()
			(with-output-to-string (ss)
			  (with-format (raphael ss 
						:page-width (the view-object page-width)
						:page-length (the view-object page-length)
						:background-color (the background-color)
						:foreground-color (the foreground-color))
			    (write-the view-object cad-output))))))
   
   
   ("String of valid HTML. This can be used to 
include the PNG or JPG raster-graphics of the geometry."
    raster-graphics 
    (with-cl-who-string () (write-the raster-graphics)))
   
   
   ("String of valid HTML. This can be used to 
include the SVG or VML vector-graphics of the geometry."
    vector-graphics 
    (with-cl-who-string () (write-the vector-graphics)))
   
   ("String of valid HTML. This can be used to 
include the VRML or X3D graphics of the geometry."
    web3d-graphics 
    (with-cl-who-string () (write-the web3d-graphics)))
   
   
   ("String of valid HTML. This includes the image-format-selector, the reset-zoom-button, 
and the view-selector, in a simple table layout. You can override this to make the view-controls
appear any way you want and include different and/or additional form-controls."
    view-controls
    (with-cl-who-string ()
      ((:div (:class "gdlFormControls"))
       (:table (:tr (:td (str (the  view-selector html-string)))
                    (:td (str (the image-format-selector html-string)))
                    (:td (str (the reset-zoom-button form-control-string))))))))
   
   
   ("String of valid HTML. This can be used to 
include the geometry, in the format currently selected by the image-format-selector.
If the include-view-controls? is non-nil, the view-controls will be appended at the 
bottom of the graphics inside a table."
    graphics 
    (with-cl-who-string ()
      (:table (:tr ((:td :align :center)
                    (str (ecase (the image-format-selector value)
                               ((:web3d :vrml) (the web3d-graphics))
                               ((:png :jpeg :jpg) (the raster-graphics))
                               (:raphael (the vector-graphics))))))
              (when (and (member (the image-format) (list :jpeg :jpg :png :raphael))
                         (the include-view-controls?))
                (htm (:tr (:td (str (the view-controls))))))
              (when (and (member (the image-format) (list :vrml :web3d))
                         (the include-view-controls?))
                (htm (:tr (:td (str (the image-format-selector html-string))))))


              ))))

  
  :hidden-objects
  ((view-object :type 'web-drawing
                :pass-down (projection-vector immune-objects background-color field-of-view-default raphael-canvas-id
					      zoom-factor-renderer)
                :page-length (the length)
                :page-width (the width)
                :objects (the display-list-objects)
                :object-roots (the display-list-object-roots))

   ("Object of type menu-form-control. Its value slot can be used to determine the format of image displayed."
    image-format-selector
    :type 'menu-form-control
    :size 1
    :prompt "Format" 
    :onchange (the (gdl-ajax-call :form-controls (list (the-child))))
    :choice-plist (the image-format-plist)
    :default (the image-format-default))

   
   (reset-zoom-button :type 'button-form-control
                      :label "Fit"
                      :onclick (the (gdl-ajax-call :function-key :reset-zoom!)))
   

   (view-selector :type 'menu-form-control
                  :size 1
                  :prompt "View"
                  :onchange (the (gdl-ajax-call :form-controls (list (the-child))))
                  :choice-list (plist-keys (the standard-views))
                  ;;:default :top
                  :default (the view-direction-default)
                  ))

  
  :functions
  (
   ;;
   ;; FLAG -- copied from base-html-graphics-sheet's logic for dig-point and report-point -- 
   ;;         factor out the repeated code!
   ;;
   ;; FLAG -- standardize on length instead of height for Y coord.
   ;;
   
   (model-x-y (local-x-y)
	      (when local-x-y
		(destructuring-bind (&key x y) local-x-y
		  (let ((x (- x (half (the view-object width))))
			(y (let ((y (- (the view-object length) y)))
			     (- y (half (the view-object length))))))
		    (let ((adjusted 
			   (scalar*vector 
			    (the view-object user-scale)
			    (add-vectors (make-point (get-x (the view-object user-center) )
						     (get-y (the view-object user-center)) 0)
					 (scalar*vector (/ (the view-object user-scale))
							(make-point x y 0))))))
		      (let ((model-point (the view-object main-view (model-point adjusted))))
			model-point))))))



   (reset-zoom!
    ()
    (the view-object (restore-slot-defaults! (list :user-center :user-scale))))

   ("Void. Writes an embedded X3D tag with content for the <tt>view-object</tt> child of this object. 
The <tt>view-object</tt> child should exist and be of type <tt>web-drawing</tt>."

    write-embedded-x3dom-world
    (&key (include-view-controls? nil))
    (write-the (embedded-x3dom-world :include-view-controls? include-view-controls?)))))


(define-lens (html-format base-ajax-graphics-sheet)()
  :output-functions
  ((main-view
    ()
    (with-cl-who ()
      ((:table :border (the viewport-border-default))
       (:tr (:td (str (the graphics)))))))
   
   (vector-graphics
    ()
    (with-cl-who ()
      (when (typep (the :view-object) 'null-part)
        (error "A valid :view-object of type web-drawing is 
required in the sheet to call the :raphael-canvas method."))
     
      (let ((no-graphics? (the no-graphics?)))
        
        (htm ((:div :id (the raphael-canvas-id)
                    :style 
                    (format nil "cursor: ~a; 
height: ~apx;
width: ~apx;
overflow:hidden; 
clip: rect(0px ~apx ~apx 0px); 
position: relative;
"
                            (if (the vector-graphics-onclick?)
                                "crosshair" "arrow")

                            (the view-object length)
                            (the view-object width)
                            (the view-object width)
                            (the view-object length))

                    :onclick (unless (or no-graphics?
                                         (not (the vector-graphics-onclick?)))
                               (the (gdl-ajax-call :function-key :dig-point))))
              (if no-graphics?
                  (htm ((:table :border 1 :cellspacing 0 :cellpadding 0 :bgcolor :white)
                        (:tr
                         ((:td :width (the :view-object :width) :height (the :view-object :length) 
                               :align :center :valign :center)
                          (:big (:b "No Graphics Object Specified"))))))
                (let ((raphael-string (the raphael-string)))
                  (htm ((:script :type "text/javascript")
                        (str raphael-string))))))))))
   
   
   ("Void. Writes an embedded X3D tag and included content for the <tt>view-object</tt> child of this object. 
The <tt>view-object</tt> child should exist and be of type <tt>web-drawing</tt>."

    embedded-x3dom-world
    (&key (include-view-controls? nil))
    
    (declare (ignore include-view-controls?))
    
    ;; (the (restore-slot-default! :js-to-eval))
    
    (cl-who:with-html-output (*stream*)
      
    (when (typep (the :view-object) 'null-part)
      (error "A valid :view-object of type web-drawing is required in the sheet 
to call the :write-embedded-x3d-world function."))
    
    (cond ((the no-graphics?)
	   #+nil
	   (and (null (the :view-object :object-roots))
                (null (the :view-object :objects)))
           (html-stream *stream* 
                        ((:table :cellspacing 0 :cellpadding 0 :bgcolor :white)
                         (:tr
                          ((:td :width (the :view-object :width) :height
                                (the :view-object :height) :align :center :valign :center)
                           (:big (:b "No Graphics Object Specified")))))))
          (t


	   (with-cl-who ()
	     (:p
	      ((:span :style "cursor: pointer;")
	       ((:|X3D| :id "the_element"
		  :swfpath "/static/3rdpty/x3dom/x3dom.swf"
		  ;;:width  "100%"
		  ;;:height  "100%"
		  :width (the view-object page-width)
		  :height (the view-object page-length)
		  )
		(:|Scene|
		  (with-format (x3d *stream*) 
		    (let ((*onclick-function* (the onclick-function)))
		      (write-the view-object cad-output)))))))
		
	     ((:script :type "text/javascript" 
		       :src "/static/3rdpty/x3dom/x3dom.js" :id "xdom_script"))
	     
	     (when (the x3dom-view-controls?)
	       (htm (:tr (:td ((:span :style "color: blue; cursor: pointer;" 
				      :onclick "document.getElementById('the_element').runtime.showAll();")
			       "Show All")))))))



	   #+nil
           (with-cl-who ()
             ((:table :cellspacing 0 :cellpadding 0)
              (:tr
               (:td
		(:p
		 ((:|X3D| :id "the_element"
		    :swfpath "/static/3rdpty/x3dom/x3dom.swf"
		    :width (the view-object page-width)
		    :height (the view-object page-length))
		  
		  (:|Scene|
		    (with-format (x3d *stream*) (write-the view-object cad-output)))))
		
		((:script :type "text/javascript" 
			  :src "/static/3rdpty/x3dom/x3dom.js" :id "xdom_script"))))

	      (when (the x3dom-view-controls?)
		(htm (:tr (:td ((:span :style "color: blue; cursor: pointer;" 
				       :onclick "document.getElementById('the_element').runtime.showAll();")
				"Show All")))))))

	   )))

   (web3d-graphics
    ()
    
    (with-cl-who ()
      (cond ((and (null (the :view-object :object-roots)) (null (the :view-object :objects)))
             (htm ((:table :border 1 :cellspacing 0 :cellpadding 0 :bgcolor :white)
                   (:tr
                    ((:td :width (the :view-object :width) :height (the :view-object :length) 
                          :align :center :valign :center)
                     (:big (:b "No Graphics Object Specified")))))))
            
            (t (let ((vrml-url (the vrml-url)))
                 (htm ((:table :border 0 :cellspacing 0 :cellpadding 0)
                       (:tr
                        ((:td) ;;:bgcolor :yellow) --FLAG! -- SvdE @ 13-08-09 -- removed bgcolor to blend in with viewport
                         ((:embed :src vrml-url :width (the view-object page-width) 
                                  :vrml_dashboard "false"
                                  :height (the view-object page-length))))))))))))
   
   
   (raster-graphics
    ()
    (when (typep (the :view-object) 'null-part)
      (error "A valid :view-object of type web-drawing is 
required in the sheet to call the :write-geometry method."))
    (with-cl-who ()
      (cond ((and (null (the :view-object :object-roots)) (null (the :view-object :objects)))
             (htm ((:table :border 1 :cellspacing 0 :cellpadding 0 :bgcolor :white)
                   (:tr
                    ((:td :width (the :view-object :width) :height (the :view-object :length) 
                          :align :center :valign :center)
                     (:big (:b "No Graphics Object Specified")))))))
          
            ((typep (the image-url) 'error)
             (the (set-slot! :view-toggle nil))
             (write-the geometry-error))
          
            (t
             (let ((image-url (the image-url)))
               (htm ((:table :border 0 :cellspacing 0 :cellpadding 0)
                     (:tr
                      ((:td) ;;:bgcolor :yellow) --FLAG! -- SvdE @ 13-08-09 -- removed bgcolor to blend in with viewport
                       ((:img :id "myimage"
                              :style "cursor: crosshair;"
                              :src image-url 
                              :onclick (the (gdl-ajax-call :function-key :dig-point))
                              :border 0 :width (the :view-object :page-width) 
                              :height (the :view-object :page-length))))))))))))))
