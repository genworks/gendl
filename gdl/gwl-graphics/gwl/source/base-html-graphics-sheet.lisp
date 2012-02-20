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


(defparameter *onclick-function* nil)


(define-object geometry-view-mixin ()
  
  :input-slots
  ((display-controls-hash nil)

   ("Number. Length (\"height\" of screen window) of the graphics viewport. Default is 300." length 300)

   ("Number. Width of the graphics viewport. Default is 300." width 300)

   
   (onclick-function nil)
   
   (use-bsplines? nil)
   
   )
  
  :computed-slots
  (
   
   (view-toggle nil :settable)
   
   (pdf-url (let ((url 
                   (format nil "~a~a"
                           (let ((url (the url)))(subseq url 0 (- (length url) (length "index.html"))))
                           (make-pathname :name "image" :type "pdf"))))
              ;;
              ;; Pre-demand the output in order to set up dependency tracking.
              ;;
              #-lispworks5.1(with-format (pdf nil ;;(merge-pathnames "snap.pdf" (glisp:temporary-folder))
                                              :page-width (the view-object page-width)
                                              :page-length (the view-object page-length))
                              (write-the view-object cad-output))
              
              (publish :path url :content-type "application/pdf"
                       :format :binary 
                       :function 
                       #'(lambda (req ent)
                           (let ((*display-controls* (the display-controls-hash)))
                             (with-http-response (req ent)
                               (setf (reply-header-slot-value req :cache-control) "no-cache")
                               (with-http-body (req ent)
                                 (with-format (pdf (request-reply-stream req)
                                                   :page-width (the view-object page-width)
                                                   :page-length (the view-object page-length))
                                   (write-the view-object cad-output)))))))
              (push url (gethash (make-keyword (the instance-id)) *url-hash-table*)) url))
   
   
   
   (x3d-url (let ((url 
                   (format nil "~a~a"
                           (let ((url (the url)))(subseq url 0 (- (length url) (length "index.html"))))
                           (make-pathname :name "model" :type "x3d"))))

              (the view-toggle)

               ;;
               ;; Pre-demand the output in order to set up dependency tracking.
               ;;
               #-lispworks5.1(with-format (x3d nil) (write-the view-object cad-output))
               
               (print-variables (the image-format))
               
               (publish :path url :content-type "model/x3d-xml"
                        :function 
                        (lambda (req ent)
                          (let ((*display-controls* (the display-controls-hash)))
                            (with-http-response (req ent)
                              (setf (reply-header-slot-value req :cache-control) "max-age=1")
                              (with-http-body (req ent)
                                (let ((reply-stream (request-reply-stream req))
                                      (*req* req) (*ent* ent))
                                  (when *debug?* 
                                    (let ((*onclick-function* (the onclick-function)))
                                      (with-format (x3d "/tmp/try.x3d") 
                                        (write-the view-object cad-output))))
                                  (let ((*onclick-function* (the onclick-function)))
                                    (with-format (x3d reply-stream :use-bsplines? (the use-bsplines?))
                                      (write-the view-object cad-output)))))))))

               (push url (gethash (make-keyword (the instance-id)) *url-hash-table*)) url) :uncached)
   
   
   (vrml-url (let ((url 
                    (format nil "~a~a"
                            (let ((url (the url)))(subseq url 0 (- (length url) (length "index.html"))))
                            (make-pathname :name "model" :type "wrl"))))
               
               ;;
               ;; Pre-demand the output in order to set up dependency tracking.
               ;;
               #-lispworks5.1(with-format (vrml nil) (write-the view-object cad-output))
               
               (publish :path url :content-type "x-world/x-vrml"
                        :function 
                        (lambda (req ent)
                          (let ((*display-controls* (the display-controls-hash)))
                            (with-http-response (req ent)
                              ;;(setf (reply-header-slot-value req :cache-control) "no-cache")
                              ;;(setf (reply-header-slot-value req :cache-control) "max-age=2")
                              
                              (setf (reply-header-slot-value req :cache-control) "max-age=1")

                              (with-http-body (req ent)
                                (let ((reply-stream (request-reply-stream req))
                                      (*req* req) (*ent* ent))
                                  (when *debug?* 
                                    (let ((*onclick-function* (the onclick-function)))
                                      (with-format (vrml "/tmp/try.wrl") 
                                        (write-the view-object cad-output))))
                                  (let ((*onclick-function* (the onclick-function)))
                                    (with-format (vrml reply-stream :use-bsplines? (the use-bsplines?))
                                      (write-the view-object cad-output)))))))))

               (push url (gethash (make-keyword (the instance-id)) *url-hash-table*)) url))
   
   
   
   
   (2d-boxes? (the view-object main-view 2d-boxes))
                         
   
   (image-url (if (the 2d-boxes?)
                  (let ((url 
                         (format nil "~a~a"
                                 (let ((url (the url)))(subseq url 0 (- (length url) (length "index.html"))))
                                 (make-pathname :name (format nil "image~a" (gensym)) 
                                                :type (ecase (the image-format)
                                                        (:png "png")
                                                        (:jpeg "jpg"))))))
                    ;;
                    ;; Pre-demand the output in order to set up dependency tracking.
                    ;;
                
                    (the view-toggle)
                
                    (multiple-value-bind (result error)
                        (;;ignore-errors
                         progn
                         (the view-object objects)
                         (the view-object object-roots)
                     
                         (ecase (the image-format)
                           (:png
                            #-lispworks5.1(with-format (pdf 
                                                        nil ;;(merge-pathnames "snap.pdf" (glisp:temporary-folder))
                                                        :page-width (the view-object page-width)
                                                        :page-length (the view-object page-length)
                                                        :background-color (the background-color)
                                                        :foreground-color (the foreground-color))
                                            (write-the view-object cad-output)))
                           (:jpeg 
                            #-lispworks5.1(with-format (jpeg nil
                                                             :page-width (the view-object page-width)
                                                             :page-length (the view-object page-length)
                                                             :background-color (the background-color)
                                                             :foreground-color (the foreground-color)
                                                             )
                                            (write-the view-object cad-output)))))
                  
                      (declare (ignore result))
                  
                      (if (the view-object image-file)
                          (publish-file :path url :file (the view-object image-file))
                        (unless (typep error 'error)
                          (publish :path url :content-type "image/png"
                                   :format :binary 
                                   :function 
                                   #'(lambda (req ent)
                                       (let ((*display-controls* (the display-controls-hash)))
                                         (with-http-response (req ent)
                                           (setf (reply-header-slot-value req :cache-control) "no-cache")
                                           (with-http-body (req ent)

                                             (ecase (the image-format)
                                               (:png 
                                                (with-format (png (request-reply-stream req)
                                                                  :page-width (the view-object page-width)
                                                                  :page-length (the view-object page-length)
                                                                  :background-color (the background-color)
                                                                  :foreground-color (the foreground-color))
                                                  (write-the view-object cad-output)))
                                               (:jpeg 
                                                (with-format (jpeg (request-reply-stream req)
                                                                   :page-width (the view-object page-width)
                                                                   :page-length (the view-object page-length)
                                                                   :background-color (the background-color)
                                                                   :foreground-color (the foreground-color))
                                                  (write-the view-object cad-output)))))))))
                          ;;
                          ;; FLAG -- we should unpublish all the previous ones here.
                          ;;
                          (push url (gethash (make-keyword (the instance-id)) *url-hash-table*))))

                      (if (typep error 'error) error url)))
                
                "/static/gwl/images/slack_in.gif"
                
                )))

  
  :hidden-objects
  ((view-object :type 'web-drawing)))


(define-object base-html-graphics-sheet (base-html-sheet geometry-view-mixin base-object)
  :documentation
  (:description "This mixin allows a part to be displayed as a web page in GWL, and
to contain one graphics area. It requires the geom-base module to be loaded. This will 
probably be extended to allow more than one graphics area. This mixin inherits from 
base-html-sheet, so just like with <tt>base-html-sheet</tt> you can prepare the output 
with the <tt>write-html-sheet</tt> function  in a the object which mixes  this in, or 
in a <tt>main-sheet</tt> output-function in an html-format view of the object.


")
  
  
  :input-slots
  (("Plist of keywords and 3D vectors. 
Indicates the views to show in the graphics controls."
    standard-views *standard-views*)
   
   (available-image-formats (list :png :jpeg :vrml))
   
   ("Keyword symbol. Determines the default image format. Defaults to :png"
    image-format :png :settable)
   
   ("Boolean. Determines whether to use native bspline data in the vrml"
    use-bsplines? nil )
   
   
   ("Keyword symbol. Determines the default view from the <tt>standard-views</tt>. Defaults to :trimetric."
    view :trimetric :settable)

   ("Keyword symbol, one of :in, :out, or :none, or nil. If :in, then clicks
in the graphics area will increase the zoom factor by (the zoom-factor). If :out,
then clicks will decrease the factor by that amount. If :none or nil, then clicks
will have no effect."

    zoom-mode :in :settable)
   
   
   ("Number. The factor used for zooming in or out." zoom-factor 2 :settable)
   
   
   ("Keyword symbol, one of <tt>:zoom-and-center</tt>, <tt>:report-point</tt>, or <tt>:measure-distance</tt>.
<ul>
<li>If <tt>:zoom-and-center</tt>, sets the user-center and user-scale accordingly when graphics
    area is clicked.</li>

<li>If <tt>:report-point</tt>, the slot <tt>digitized-point</tt> is set with the x y value. </li>

<li>If <tt>measure-distance</tt>, the slot <tt>:digitized-distance</tt> is set with the resultant distance.</li>

</ul>

Default is <tt>:zoom-and-center</tt>"
    digitation-mode :zoom-and-center :settable)
   
   (zoom-factor-renderer 1 :settable)
   
   
   (field-of-view-default 1 :settable :defaulting))
  
  
  :trickle-down-slots (field-of-view-default)
  
  :computed-slots
  (
   
   (digitation-modes (list :zoom-and-center :report-point :measure-distance))
   
   (digitized-point nil :settable)
   
   (digitized-distance nil :settable)
   
   
   
   
   (pan-distance 50 :settable)
   
   
   
   (view-depth 0 :settable)
   (view-toggle nil :settable)
   
   
   (no-graphics? (and (null (the :view-object :object-roots)) 
                      (null (the :view-object :objects))))
   
   )

  
  :functions
  (

   (dig-point
    (&key (x  *clicked-x*)
          (y *clicked-y*))
    
    (when *debug?* (print-variables x y))
    
    (when (and x y)
    
      (let ((local-point (make-point (- x (half (the view-object width)))
                                     (let ((y (- (the view-object length) y)))
                                       (- y (half (the view-object length)))))))
      
        (when *debug?* (print-variables local-point))
      
        (let ((digitized-point 
               (add-vectors (the view-object user-center)
                            (scalar*vector (/ (the view-object user-scale))
                                           local-point))))

          (when *debug?* (print-messages digitation-mode))
          
          (ecase (the digitation-mode)
          
            (:measure-distance 
             (let ((new-point (the view-object main-view 
                                   (model-point (make-point (get-x local-point)
                                                            (get-y local-point)
                                                            0))))
                   (old-point (the digitized-point)))

               (if (null old-point)
                   (progn
                     (the (set-slot! :digitized-point new-point))
                     (the (restore-slot-default! :digitized-distance)))
                 (progn
                   (the (set-slot! :digitized-distance 
                                   (list :total (3d-distance old-point new-point)
                                         :x (- (get-x new-point) (get-x old-point))
                                         :y (- (get-y new-point) (get-y old-point))
                                         :z (- (get-z new-point) (get-z old-point)))))
                   (the (restore-slot-default! :digitized-point))))))
          
          
            (:report-point 
             (the (report-point (get-x local-point) (get-y local-point))))
          
            (:zoom-and-center

               (when *debug?* (format t "we are on zoom-and-center...~%"))
               
               (when (member (the zoom-mode) (list :in :out))

                 (when *debug?* (format t "Zooming in the graphics...~%"))
               
                 (let ((zoom-factor (if (eql (the zoom-mode) :in)
                                        (the zoom-factor)
                                        (/ (the zoom-factor)))))
                   (the view-object 
                     (set-slot! :user-center (make-point (get-x digitized-point)
                                                         (get-y digitized-point))))
                   (the view-object 
                     (set-slot! :user-scale 
                                (* (the view-object user-scale) zoom-factor)))))))))))
   
   
   ("Void. Process the points selected by digitizing in the graphics. You can override this 
function to do your own processing. By default, it prints the information to the console.

:arguments (x \"Number. The X Coordinate of the digitized point.\"
            y \"Number. The Y Coordinate of the digitized point.\")"

    report-point
    (x y)
    
    (print-variables x y)
    
    (let ((adjusted 
           (scalar*vector 
            (the view-object user-scale)
            (add-vectors (make-point (get-x (the view-object user-center) )
                                     (get-y (the view-object user-center)) 0)
                         (scalar*vector (/ (the view-object user-scale))
                                        (make-point x y 0))))))
      (let ((model-point (the view-object main-view (model-point adjusted))))
        (the (set-slot! :digitized-point model-point)))))
   
   
   ("Keyword symbol, string, list, or vector. Default background for the graphics viewport. Can be specified 
as a name (keyword or string) in *color-table*, an html-style hex string (starting with #), or a decimal RGB
triplet in a list or vector. The default comes from the :background entry in <tt>*colors-default*</tt>."
    background-color 
    ()
    (lookup-color (getf *colors-default* :background) :format :decimal))

   ("Keyword symbol, string, list, or vector. Default foreground for the graphics viewport. Can be specified 
as a name (keyword or string) in *color-table*, an html-style hex string (starting with #), or a decimal RGB
triplet in a list or vector. The default comes from the :foreground entry in <tt>*colors-default*</tt>."
    foreground-color 
    ()
    (lookup-color (getf *colors-default* :foreground) :format :decimal))
   
   (toggle-view-toggle! () (the (set-slot! :view-toggle (not (the view-toggle)))))
   
   (write-geometry-links
    (&key (include-view-controls? t))
    (write-the (geometry-links :include-view-controls? include-view-controls?)))
   
   
   ("Void. Writes an image tag and publishes an image for the <tt>view-object</tt> child of this object. 
The <tt>view-object</tt> child should exist and be of type <tt>web-drawing</tt>.

For objects of type <tt>gwl:application-mixin</tt> or <tt>gwl:node-mixin</tt>, this is done automatically.

For the time being, we recommend that you use <tt>gwl:application-mixin</tt> or <tt>gwl:node-mixin</tt> if you want to
display geometric parts in a GWL application.

:&key ((include-view-controls? t) \"Boolean. Determines whether the standard view controls are displayed below the image.\")"
    write-geometry
    (&key (include-view-controls? t) use-ajax?)
    (write-the (geometry :include-view-controls? include-view-controls? :use-ajax? use-ajax?)))

   (write-view-controls
    (&key width)
    (write-the (view-controls :width width)))
   

   
   ("Void. Writes an embedded X3D tag with content for the <tt>view-object</tt> child of this object. 
The <tt>view-object</tt> child should exist and be of type <tt>web-drawing</tt>."

    write-embedded-x3dom-world
    (&key (include-view-controls? nil))
    (write-the (embedded-x3dom-world :include-view-controls? include-view-controls?)))
      
   ("Void. Writes an OBJECT tag and publishes an X3D world for the <tt>view-object</tt> child of this object. 
The <tt>view-object</tt> child should exist and be of type <tt>web-drawing</tt>."

    write-embedded-x3d-world
    (&key (include-view-controls? nil))
    (write-the (embedded-x3d-world :include-view-controls? include-view-controls?)))

   
   
   ("Void. Writes an EMBED tag and publishes a VRML world for the <tt>view-object</tt> child of this object. 
The <tt>view-object</tt> child should exist and be of type <tt>web-drawing</tt>."

    write-embedded-vrml-world
    (&key (include-view-controls? t))
    (write-the (embedded-vrml-world :include-view-controls? include-view-controls?)))

   
   (write-image-format-selector
    ()
    (write-the image-format-selector))

   (write-vrml-view-controls
    (&key width)
    (write-the (vrml-view-controls :width width)))))
   
   

(define-lens (html-format base-html-graphics-sheet)()
  
  :output-functions
  (

   
   
   (geometry-error
    ()
    (html ((:table :border 1 :cellspacing 0 :cellpadding 0 :bgcolor :white)
           (:tr
            ((:td :width (the :view-object :width) :height (the :view-object :length) 
                  :align :center :valign :center)
             (:big (:b "Graphics Threw Error: " (:princ-safe (the image-url)))))))))
   

   

   ("Void. Writes an image tag and publishes an image for the <tt>view-object</tt> child of this object. 
The <tt>view-object</tt> child should exist and be of type <tt>web-drawing</tt>.

For objects of type <tt>gwl:application-mixin</tt> or <tt>gwl:node-mixin</tt>, this is done automatically.

For the time being, we recommend that you use <tt>gwl:application-mixin</tt> or <tt>gwl:node-mixin</tt> if you want to
display geometric parts in a GWL application.

:&key ((include-view-controls? t) \"Boolean. Determines whether the standard view controls are displayed below the image.\")"
    geometry
    (&key (include-view-controls? t) use-ajax?)
    
    (when (typep (the :view-object) 'null-part)
      (error "A valid :view-object of type web-drawing is required in the sheet to call the :write-geometry method."))
    (cond ((and (null (the :view-object :object-roots)) (null (the :view-object :objects)))
           (html ((:table :border 1 :cellspacing 0 :cellpadding 0 :bgcolor :white)
                  (:tr
                   ((:td :width (the :view-object :width) :height (the :view-object :length) 
                         :align :center :valign :center)
                    (:big (:b "No Graphics Object Specified"))))
                  )))
          
          ((typep (the image-url) 'error)
           (the (set-slot! :view-toggle nil))
           (write-the geometry-error))
          
          (t
           (let ((image-url (the image-url)))
             (html ((:table :border 0 :cellspacing 0 :cellpadding 0)
                    (:tr
                     ((:td :bgcolor :yellow)
                      
                      
                      (if use-ajax?
                          (html ((:img :id "myimage"
                                       :style "cursor: pointer;"
                                       :src image-url 
                                       :onclick (format nil "return digitizePoint(event, '~a', '~s');" 
                                                        (the instance-id) (the root-path-local))
                                       :border 0 :width (the :view-object :page-width) 
                                       :height (the :view-object :page-length))))
                        
                        (html ((:input :type :image :src image-url
                                       :alt "Loading Graphics..." :name "image"
                                       :border 0 :width (the :view-object :page-width) 
                                       :height (the :view-object :page-length)))))))
                                    
                    ;; FLAG!: the following is untested but working in the  vrml-world part of the code
                    (if include-view-controls?
                        (html 
                         (:tr (:td
                               (write-the (view-controls :width (the :view-object :width)
                                                         :use-ajax? use-ajax?))
                               (html :br)))))))))))
   
   
   (geometry-links
    (&key (include-view-controls? t))
       
    (when (typep (the :view-object) 'null-part)
      (error "A valid :view-object of type web-drawing is required in the sheet to call the :write-geometry method."))
    
    (html ((:table :cellspacing 0 :cellpadding 0 :bgcolor :white)
           (:tr
            ((:td :width (the :view-object :width) :height (the :view-object :length) 
                  :align :center :valign :center)
             
             (:ul
              (:li ((:a :href (the pdf-url)) "View PDF File of Geometry"))
              (:li ((:a :href (the vrml-url)) "View VRML File of Geometry")))))

           ;; FLAG!: the following is untested but working in the  vrml-world part of the code
           (if include-view-controls? 
               (html (:tr
                      ((:td :width (the :view-object :width) :height 100)
                       
                       (write-the (:view-controls :width (the :view-object :width))) 
                       (html :br))))))))           
   
   
   (view-controls
    (&key width use-ajax?)
    
    (if use-ajax?
        (html ((:table :if* width :width width :cellpadding 1 :cellspacing 0)
               (:tr
                ((:td :align :center :bgcolor
                      (gethash :goldenrod-medium *color-table*))
                 "View from "
               
                 ((:select :size 1 :onchange (format nil "gdlsetcontrol ('~a', '~s', ':view', this.value)" 
                                                     (the instance-id) (the root-path-local)))
                  (dolist (view-key (plist-keys (the :standard-views)))
                    (html ((:option :value view-key :if* (eql view-key (the :view))
                                    :selected :selected)
                           (:princ (string-capitalize view-key)))))))

                (:newline)
              
                ((:td :align :center :valign :center :bgcolor
                      (gethash :red-violet-medium *color-table*))
               
                 (:newline)
               
                 ((:select :size 1 :onchange (format nil "gdlsetcontrol ('~a', '~s', ':digitation-mode', this.value)" 
                                                     (the instance-id) (the root-path-local)))
                  (dolist (dig-mode (the digitation-modes))
                    (html ((:option :value dig-mode :if* (eql dig-mode (the digitation-mode)) :selected :selected)
                           (:princ (string-capitalize dig-mode))))))
                 
                 
                 ((:input :onclick (format nil "gdlsetcontrol ('~a', '(:view-object :viewport)', '(:user-center :user-scale)', 'gdl::restore-default')"
                                           (the instance-id))
                          :type :submit 
                          :value "Restore" 
                          ))
                 
                 (case (the digitation-mode) 
                   
                   (:report-point
                    (html :br
                          (if (null (the digitized-point))
                              (html "Please Select a Point")
                            (html (:princ (format nil "Selected: (~4,2f, ~4,2f, ~4,2f)"
                                                  (get-x (the digitized-point))
                                                  (get-y (the digitized-point))
                                                  (get-z (the digitized-point))))))))
                   
                   (:measure-distance
                    (html :br
                          (cond 
                           ((the digitized-distance)
                            (html
                             "total: " (:princ (format nil "~4,2f," (getf (the digitized-distance) :total)))
                             " x:" (:princ (format nil "~4,2f," (getf (the digitized-distance) :x)))
                             " y:" (:princ (format nil "~4,2f," (getf (the digitized-distance) :y)))
                             " z:" (:princ (format nil "~4,2f" (getf (the digitized-distance) :z)))))
                           ((null (the digitized-point))
                            (html "Please Select the First Point"))
                           (t (html "Please Select the Second Point"))
                           ))))))
           
               (:tr
                ((:td :bgcolor (gethash :blue-steel-light *color-table*))
                 (:table (:tr (:td "Zoom Factor: "
                                   ((:input :type :string :size 1 
                                            :value (the zoom-factor)
                                            :onchange (format nil "gdlsetcontrol ('~a', '~s', ':zoom-factor', this.value)"
                                                              (the instance-id) 
                                                              (the root-path-local))))))))
                (the write-image-format-selector))))
      
      
      (html ((:table :if* width :width width :cellpadding 1 :cellspacing 0)
             (:tr
              ((:td :align :center :bgcolor
                    (gethash :goldenrod-medium *color-table*))
               ((:input :type :submit :name :change-view :value "View")) " from "
               ((:select :name :view :size 1 :onchange "this.form.submit()")
                (dolist (view-key (plist-keys (the :standard-views)))
                  (html ((:option :value view-key :if* (eql view-key (the :view))
                                  :selected :selected)
                         (:princ (string-capitalize view-key)))))))
            
              ((:td :align :center :valign :center :bgcolor
                    (gethash :red-violet-medium *color-table*))
               ((:input :type :submit :name :restore-view :value "Restore"))
               " the Default View"))
           
             (:tr
              ((:td :bgcolor (gethash :blue-steel-light *color-table*))
               (:table (:tr (:td "Click in Graphics to Recenter and Zoom In by a factor of "
                                 ((:input :type :string :size 1 :name :zoom-factor :value
                                          (the :zoom-factor))))
                            (:td ((:select :name :digitation-mode :size 1 :onchange "this.form.submit()")
                                  (dolist (view-key (the digitation-modes) ;;(the (list :zoom-and-center :report-point)
                                           )
                                    (html ((:option :value view-key :if* (eql view-key (the :digitation-mode)) :selected :selected)
                                           (:princ (string-capitalize view-key))))))


                                 (case (the digitation-mode) 
                   
                                   (:report-point
                                      (html :br
                                            (if (null (the digitized-point))
                                                (html "Please Select a Point")
                                                (html (:princ (format nil "Selected: (~4,2f, ~4,2f, ~4,2f)"
                                                                      (get-x (the digitized-point))
                                                                      (get-y (the digitized-point))
                                                                      (get-z (the digitized-point))))))))
                   
                                   (:measure-distance
                                      (html :br
                                            (cond 
                                              ((the digitized-distance)
                                               (html
                                                "total: " (:princ (format nil "~4,2f," (getf (the digitized-distance) :total)))
                                                " x:" (:princ (format nil "~4,2f," (getf (the digitized-distance) :x)))
                                                " y:" (:princ (format nil "~4,2f," (getf (the digitized-distance) :y)))
                                                " z:" (:princ (format nil "~4,2f" (getf (the digitized-distance) :z)))))
                                              ((null (the digitized-point))
                                               (html "Please Select the First Point"))
                                              (t (html "Please Select the Second Point"))
                                              ))))


                                 )))
               ".")
              (the write-image-format-selector))))))
   
   
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
    
    (cond ((and (null (the :view-object :object-roots))
                (null (the :view-object :objects)))
           (html-stream *stream* 
                        ((:table :cellspacing 0 :cellpadding 0 :bgcolor :white)
                         (:tr
                          ((:td :width (the :view-object :width) :height
                                (the :view-object :height) :align :center :valign :center)
                           (:big (:b "No Graphics Object Specified")))))))
          (t
           (with-cl-who ()
             ((:table :cellspacing 0 :cellpadding 0)
              (:tr
               (:td
                ((:x3d :id "the_element"
                       :width (the view-object page-width)
                       :height (the view-object page-length))
                 
                 (:scene 
                  (with-format (x3d *stream*) (write-the view-object cad-output))
                  
                  ))
                ((:script :type "text/javascript" 
			  :src "/static/3rdpty/x3dom/x3dom.js" :id "xdom_script"))
                
                
                
                ))

              (:tr (:td ((:span :style "color: blue; cursor: pointer;" 
                                :onclick "document.getElementById('the_element').runtime.showAll();")
                         "Show All")
                        
                        )))
             
             #+nil
             (:script 
              "
        var $element;
        var debug = false;
        var pick_mode_info;
        var nav_mode_info;
        var ab_info;
 
        function init() {
                $element = document.getElementById('the_element');
                updateAbInfo('Viewpoint');
                updateNavInfo();
        }
        
        function updateNavInfo() {
                nav_mode_info = document.getElementById('nav_mode_info');
                nav_mode_info.innerHTML = $element.runtime.navigationType();
        }
 
        function updateAbInfo(typeName) {
                var bindable = $element.runtime.getActiveBindable(typeName);
                ab_info = document.getElementById('ab_info');
                ab_info.innerHTML = bindable.tagName + \" / \" + bindable.getAttribute('description');
        }
        
        function toggleStats(link) {
                stats = $element.runtime.statistics();
                if (stats) {
                        $element.runtime.statistics(false);
                        link.innerHTML = 'Show statistics';
                } else {
                        $element.runtime.statistics(true);
                        link.innerHTML = 'Hide statistics';
                }
        }
        
        function toggleDebug(link) {
                if (debug) {
                        $element.runtime.debug(false);
                        link.innerHTML = 'Show debug';
                } else {
                        $element.runtime.debug(true);
                        link.innerHTML = 'Hide debug';
                }
                debug = !debug

            init();

")

             
             
             )))))


   #+nil
   ("Void. Writes an embedded X3D tag and included content for the <tt>view-object</tt> child of this object. 
The <tt>view-object</tt> child should exist and be of type <tt>web-drawing</tt>."

    embedded-x3dom-world
    (&key (include-view-controls? nil))
    
    (declare (ignore include-view-controls?))
    
    (the (restore-slot-default! :js-to-eval))
    
    (when (typep (the :view-object) 'null-part)
      (error "A valid :view-object of type web-drawing is required in the sheet 
to call the :write-embedded-x3d-world function."))
    (cond ((and (null (the :view-object :object-roots))
                (null (the :view-object :objects)))
           (html-stream *stream* 
                        ((:table :cellspacing 0 :cellpadding 0 :bgcolor :white)
                         (:tr
                          ((:td :width (the :view-object :width) :height
                                (the :view-object :height) :align :center :valign :center)
                           (:big (:b "No Graphics Object Specified")))))))
          (t
           (with-cl-who ()
             ((:table :cellspacing 0 :cellpadding 0)
              (:tr
               (:td
                ((:x3d :width (the view-object page-width)
                       :height (the view-object page-length))
                 (:scene (:shape (:appearance ((:material :diffuseColor "red")))
                                 (:box))))
                ((:script :type "text/javascript" :src "http://www.x3dom.org/x3dom/release/x3dom.js" :id "xdom_script"))
                
                )))))))
   
   
   ("Void. Writes an OBJECT tag and publishes an X3D world for the <tt>view-object</tt> child of this object. 
The <tt>view-object</tt> child should exist and be of type <tt>web-drawing</tt>."

    embedded-x3d-world
    (&key (include-view-controls? nil))
    
    (when (typep (the :view-object) 'null-part)
      (error "A valid :view-object of type web-drawing is required in the sheet 
to call the :write-embedded-x3d-world function."))
    (cond ((and (null (the :view-object :object-roots))
                (null (the :view-object :objects)))
           (html-stream *stream* 
                        ((:table :cellspacing 0 :cellpadding 0 :bgcolor :white)
                         (:tr
                          ((:td :width (the :view-object :width) :height
                                (the :view-object :height) :align :center :valign :center)
                           (:big (:b "No Graphics Object Specified")))))))
          (t
           (let ((x3d-url (the x3d-url)))
             (html-stream *stream* 
                          ((:table :cellspacing 0 :cellpadding 0)
                           (:tr
                            (:td
                      
                      
                             ((:object :id "x3d" :type "model/x3d+xml"
                                       :src x3d-url
                                       :width (the view-object page-width)
                                       :height (the view-object page-length)))))
                           
                           (when include-view-controls?
                             (html (:tr
                                    ((:td :width (the :view-object :width) :height 100)
                                     (the (write-vrml-view-controls :width (the :view-object :width))) 
                                     (html :br)))))))))))

   
   
   ("Void. Writes an EMBED tag and publishes a VRML world for the <tt>view-object</tt> child of this object. 
The <tt>view-object</tt> child should exist and be of type <tt>web-drawing</tt>."

    embedded-vrml-world
    (&key (include-view-controls? t))
    (when (typep (the :view-object) 'null-part)
      (error "A valid :view-object of type web-drawing is required in the sheet 
to call the :write-embedded-vrml-world function."))
    (cond ((and (null (the :view-object :object-roots))
                (null (the :view-object :objects)))
           (html-stream *stream* 
                        ((:table :cellspacing 0 :cellpadding 0 :bgcolor :white)
                         (:tr
                          ((:td :width (the :view-object :width) :height
                                (the :view-object :height) :align :center :valign :center)
                           (:big (:b "No Graphics Object Specified")))))))
          (t
           (let ((vrml-url (the vrml-url)))
             (html-stream *stream* 
                          ((:table :cellspacing 0 :cellpadding 0)
                           (:tr
                            (:td
                             ((:embed :type "model/vrml"
                                      :src vrml-url :width (the view-object page-width) 
                                      :vrml_dashboard "false"
                                      :height (the view-object page-length)))
                      
                             ))
                           (if include-view-controls?
                               (html (:tr
                                      ((:td :width (the :view-object :width) :height 100)
                                       (the (write-vrml-view-controls :width (the :view-object :width))) 
                                       (html :br)))))))))))
   
   
   (image-format-selector
    ()
    (html
     ((:td :align :center :bgcolor (gethash :green-lime *color-table*))
      ((:input :type :submit :name :change-view-format :value "Format")) 
      ((:select :name :image-format :size 1 :onchange "this.form.submit()")
       (dolist (view-key (the available-image-formats))
         (html ((:option :value view-key :if* (eql view-key (the :image-format))
                         :selected :selected)
                (:princ (string-capitalize view-key)))))))))
   
   
   (vrml-view-controls
    (&key width)
    (html ((:table :if* width :width width :cellpadding 1 :cellspacing 0)
           (:tr
            (the write-image-format-selector)
            ((:td :align :center :bgcolor (gethash :goldenrod-medium *color-table*))
             ((:input :type :submit :name :change-view :value "View")) " from "
             ((:select :name :view :size 1 :onchange "this.form.submit()")
              (dolist (view-key (plist-keys (the :standard-views)))
                (html ((:option :value view-key :if* (eql view-key (the :view))
                                :selected :selected)
                       (:princ (string-capitalize view-key)))))))

            ((:td :align :center :bgcolor (gethash :blue-steel *color-table*))
             "Zoom Factor: " ((:input :type :text :size 5 :name :zoom-factor-renderer 
                                      :value (the zoom-factor-renderer))))

            
            ((:td :align :center :bgcolor (gethash :lime *color-table*))
             "Field of View: " ((:input :type :text :size 5 :name 
                                        :field-of-view-default :value (the field-of-view-default))))
            
            ((:td :align :center :bgcolor (gethash :red-orange *color-table*))
             ((:input :type :submit :name :apply :value " Apply")))
            
            ((:td :align :center :valign :center :bgcolor
                  (gethash :goldenrod-medium *color-table*)))))))))
  
  
  





