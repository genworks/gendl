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

(define-object web-drawing (geom-base::renderer-mixin base-drawing )
  
  :documentation (:description "Container object for displaying a view of geometric 
or text-based entities in a web application. This is supposed to be the type of the
view-object hidden-child of base-html-graphics-sheet. Also, in a GWL application using 
application-mixin, you can include one object of this type in the ui-display-list-leaves.
"
                  
                  :examples "<pre>

 (in-package :gwl-user)

 (define-object test-html-graphics-sheet (base-html-graphics-sheet)
    
   :objects 

   ((b-splines :type 'test-b-spline-curves)
   
    (boxed-spline :type 'surf:boxed-curve
                  :curve-in (the b-splines (curves 0))
                  :orientation (alignment :top (the (face-normal-vector :rear)))
                  :show-box? t)

    (view-object :type 'web-drawing
                 :page-length (the graphics-height value)
                 :page-width (the graphics-width value)
                 :projection-vector (getf *standard-views* (the view))
                 :object-roots (the ui-display-roots))
   
    (graphics-height :type 'text-form-control
                     :default 350)
   
    (graphics-width :type 'text-form-control
                    :default 500)
   
    (bg-color :type 'text-form-control
              :default :black)
   
    (fg-color :type 'text-form-control
              :default :white))
     
   :computed-slots
   ((background-color (lookup-color (the :bg-color value) :format :decimal))
    (foreground-color (lookup-color (the :fg-color value) :format :decimal))
     
    (view :trimetric :settable)
   
    (\"list of gdl objects. Objects to be displayed in the graphics window.\"
     ui-display-roots (list (the b-splines) (the boxed-spline)))))

 (define-lens (html-format test-html-graphics-sheet)()
   
   :output-functions

   ((main-sheet
     ()
     (with-html-output (*html-stream* nil :indent t)
       (:html (:head (:title \"Test HTML Graphics Sheet\"))
              (:body (when gwl:*developing?* (the write-development-links))
                     (:h2 (:center \"Test HTML Graphics Sheet\"))
                     (with-html-form (:cl-who? t)
                       (:table (:tr (:td (:ul 
                                         (:li (str (the graphics-height html-string)))
                                         (:li (str (the graphics-width html-string)))
                                              (:li (str (the bg-color html-string)))
                                              (:li (str (the fg-color html-string))))
                                         (:p (:input :type :submit :value \" OK \")))
                                    (:td (write-the geometry)))))))))))

 (publish :path \"/t-h-g-s\"
          :function #'(lambda(req ent)
                        (gwl-make-object req ent \"gwl-user::test-html-graphics-sheet\")))

 (define-object test-b-spline-curves (base-object)

   :input-slots

   ((control-points (list (make-point 0 0 0)
                          (make-point 2 3.0 0.0) 
                          (make-point 4 2.0 0.0) 
                          (make-point 5 0.0 0.0) 
                          (make-point 4 -2.0 0.0) 
                          (make-point 2 -3.0 0.0) 
                          (make-point 0 0 0))))
  
   :objects

   ((curves :type 'surf:b-spline-curve
            :sequence (:size 6)
            :control-points (the control-points)
            :degree (1+ (the-child :index))
            :display-controls (list :line-thickness (* 0.3 (the-child index))
                                    :color (ecase (the-child index)
                                             (0 :red) (1 :orange) (2 :yellow) (3 :green)
                                             (4 :blue) (5 :red-violet))))

    (points :type 'point 
            :sequence (:size (length (rest (the control-points))))
            :center (nth (the-child index) (rest (the control-points)))
            :display-controls (list :color :green))))
</pre>
"
                  )
  
  
  :input-slots
  (("String. Unique ID on the page for the raphael canvas div. By default this is passed in 
from the base-ajax-graphics-sheet and based on its root-path, but can be specified manually
if you are making a web-drawing on your own. Defaults (in the standalone case) to \"RaphaelCanvas\"" 
    raphael-canvas-id "RaphaelCanvas")

   ("3D vector. This is the normal vector of the view plane onto which to project the 3D objects. Defaults to (getf *standard-views* :top)."
    projection-vector (getf *standard-views* :top)) 
   
   ("List of GDL objects. The leaves of each of these objects will be included in the geometry display. Defaults to nil." 
    object-roots nil) 
   
   ("List of GDL objects. These nodes (not their leaves but the actual objects) will be included in the geometry display. Defaults to nil."
    objects nil)
   
   (view-scale (getf (the main-view view-contents-data) :view-scale))
   
   (view-center (getf (the main-view view-contents-data) :view-center))
   
   ("List of GDL objects. These objects are not used in computing the scale or centering for the display list. Defaults to nil."
    immune-objects nil))

  :computed-slots
  ((user-center (make-vector 0 0) :settable)
   (user-scale 1 :settable)
   (center (make-point 0 0 0))
   
   (image-file (dolist (object (the objects))
                 (when (and (typep object 'gdl::gdl-basis) (the-object object image-file))
                   (return (the-object object image-file))))))

  
  :trickle-down-slots (user-scale)
  
  :objects
  (("GDL object of type geom-base:base-view. This is the actual drawing view which is used to present the geometry. Defaults to an 
internally-computed object, this should not be overridden in user code."
    main-view :type 'base-view
    ;;:center (make-point 0 0 0)
    :pass-down (projection-vector object-roots objects view-scale view-center 
                                  user-scale 
                                  ;;user-center  -- FLAG -- probably not needed? 
                                  immune-objects))))

