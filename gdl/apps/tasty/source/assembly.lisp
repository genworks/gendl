;;
;; Copyright 2002, 2009 Genworks International and Genworks BV 
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


#|

To do:

o Add direct buttons back to viewport

o support for multiple viewports

o filter inspector slots to local slots

|#


(in-package :tasty)

(define-object assembly (base-ajax-sheet)
  
  :input-slots
  ((root-object (the root-object-object) :settable)
   
   (initial-tree-depth 1)
   (package-default :gdl-user)
   (inspector-relative-font-size "-1")
   (viewport-dimensions (list :width 800 :length 550) :settable)
   
   (uri-static-gwl "/static/gwl/")

   
   (uri-static-gwl-styles (when (the use-jquery?) (string-append (the uri-static-gwl) "style/")))

   
   (uri-static-gwl-images (when (the use-jquery?) (string-append (the uri-static-gwl) "images/")))
   
   (uri-static-gwl-unpix (when (the use-jquery?) (string-append (the uri-static-gwl) "tasty-unpix/")))
   
   (uri-style-sheet (when (the use-jquery?) (string-append (the uri-static-gwl-styles) "tasty-layout.css")))
   
   
   (tatu-update-function 
    #'(lambda() 
        (the (set-slot! :root-object (the root-object)))
        (the tree update!)))
   
   (use-jquery? (the have-valid-instance?))
   (use-raphael? (the have-valid-instance?)))

  :computed-slots
  (
   (ui-specific-layout-js (when (the use-jquery?) "/static/gwl/js/tasty-initlayout-3.js"))
   
   (have-valid-instance? (not (typep (the root-object) 'null-part)))

   (additional-header-content (when (the have-valid-instance?)
                                (the ta2-style-view)))
   
   (ta2-style-view (with-cl-who-string ()
                     ((:link :type "text/css" :rel "stylesheet" 
                             :href (the uri-style-sheet)))))
   
   (html-sections (remove nil
			  (list (the tree) 
				(the tree-status-object) 
				(the viewport) 
				(when (and (eql (the viewport image-format) :x3dom)
					   (not (the viewport no-graphics?)))
				  (the viewport x3dom-div))
				(the inspector)
				(the viewport-status-object) 
				(the image-status-object)
				(the menu-section)
				)))
   
   (root-object-type 'null-part :settable)
   
   (at-true-root? (typep (the root-object parent) 'assembly))

   ;;
   ;; FLAG - this should come from color palette.
   ;;
   ;;(color-selected nil :settable)
   
   (color-selected (the menu-section color))

   ;;(color-selected :red)
   

   )

  :hidden-objects
  (
   (click-mode :type 'menu-form-control
               :size 1
               :default :inspect
               :onchange (the (gdl-ajax-call :form-controls (list (the-child))))
               :choice-plist (list :inspect "Inspect object (I)"
                                   :add-leaves "Add Leaves (AL)"
                                   :add-node "Add Node (AN)"
                                   :draw-node "Draw Node (DN)"
                                   :add-leaves* "Add Leaves indiv. (AL*)"
                                   :draw-leaves "Draw Leaves (DL)"
                                   :delete-leaves "Clear Leaves (CL)"
                                   :break "Set self to Object (B)"
                                   :set-root "Set root to Object (SR)"
                                   ))
   
   (root-object-object :type (the root-object-type)
                       :tree-root (the-child)
                       :root (the-child)
                       :$$ta2-object self
                       :strings-for-display (format nil "~s" (the root-object-type)))

   
   (part-type-form :type 'part-type-form
                   :pass-down (package-default)
                   :tatu-root self
                   :set-root-function #'(lambda(object) (the (set-root! object)))
                   :make-instance-function #'(lambda(type) 
                                               (the (make-root-instance type))))
   
   
   (tree
    :type 'tree:newertree
    ;;:type 'tree:tree
    :display-controls-hash (the viewport display-controls-hash)
    :target-object (the root-object)
    ;;:show-onmouseover-buttons? nil
    :onclick-function 
    #'(lambda(object)
        (the (gdl-ajax-call 
              :function-key :perform-action!
              :arguments (list object)))))
   
   
   (viewport :type 'viewport
             :pass-down (root-object)
             :line-thickness-selected (the menu-section line-thickness-selected)
             :color (the menu-section color)
             :inspected-object (the inspector node)
             :length (getf (the viewport-dimensions) 
                           :length (getf (the viewport-dimensions) :height))
             :width (getf (the viewport-dimensions) :width)
             :onclick-function #'(lambda(object)
                                   (the (gdl-ajax-call 
                                         :function-key :perform-action!
                                         :arguments (list object))))
             :tatu-root self)
   
   (inspector :type 'inspector
              :pass-down (root-object)
              :click-mode (the click-mode value)
              :relative-font-size (the inspector-relative-font-size)
              :display-controls-hash (the viewport display-controls-hash)
              :tatu-root self)

   (viewport-status-object :type 'status-object
                           :status-message (the viewport digitation-mode))
   
   (image-status-object :type 'status-object
                        :status-message (the viewport image-format))
   
   (tree-status-object :type 'status-object
                       :status-message (getf (the click-mode choice-plist) (the click-mode :value)))
   
   (menu-section :type 'menu-section
                 :tatu-root self
                 :pass-down (uri-static-gwl-images uri-static-gwl-unpix at-true-root?))
   
   
   
   (footer-ui-widget-header :type 'sheet-section
                            :main-view (with-cl-who-string ()
                                         (:span "Click-mode: " (str (the viewport-status-object main-div))
                                                " | Image format: [" 
                                                
                                                (if (eql (the viewport image-format) :raphael)
                                                    (str (the image-status-object main-div))
                                                  (htm ((:a :href (ecase (the viewport image-format)
                                                                    ((:jpeg :png) (the viewport image-url))
                                                                    (:vrml (the viewport vrml-url))
                                                                    (:x3d (the viewport x3d-url))
                                                                    (:x3dom (the viewport x3d-url))
                                                                    )) 
                                                        (str (the image-status-object main-div))))) 
                                                "]" 
                                                
                                                
                                                (when (eql (the viewport digitation-mode) :report-point)
                                                  (if (the viewport digitized-point)
                                                      (str (format nil " | Clicked: [~{~a~^,~}]" (let ((x (get-x (the viewport digitized-point)))
                                                                                                 (y (get-y (the viewport digitized-point)))
                                                                                                 (z (get-z (the viewport digitized-point))))
                                                                                            (list (number-round x 2)
                                                                                                  (number-round y 2)
                                                                                                  (number-round z 2)))))
                                                    (htm " | Please Select a Point")))
                                                
                                                (when (eql (the viewport digitation-mode) :measure-distance)
                                                  (cond 
                                                   ((the viewport digitized-distance)
                                                    (htm
                                                     " | total: " (str (format nil "~4,2f," (getf (the viewport digitized-distance) :total)))
                                                     " x:" (str (format nil "~4,2f," (getf (the viewport digitized-distance) :x)))
                                                     " y:" (str (format nil "~4,2f," (getf (the viewport digitized-distance) :y)))
                                                     " z:" (str (format nil "~4,2f" (getf (the viewport digitized-distance) :z)))))
                                                   ((null (the viewport digitized-point))
                                                    (htm " | Please Select the First Point"))
                                                   (t (htm " | Please Select the Second Point"))))
                                                
                                                
                                                (when (eql (the viewport digitation-mode) :select-object)
                                                  (htm " | Mouse Over to Highlight and Click to Select Object to Inspect"))))))
                                                  

  
  :functions
  (
   (set-self! (object) (set-self object))
   
   (update-object! (object) 
                   (let ((root-object (the root-object)))
                     (the-object object update!)
                     (the-object object (set-slot! :root-object root-object))))
   
   (update-root-object!
    ()
    (the root-object update!)
    (the tree update!)
    )
   
   (make-root-instance
    (type)
    (the (set-slot! :root-object-type
                    (let ((*package* (find-package (the package-default))))
                      (read-safe-string type)))))

   (set-root!
    (object)
    (when (typep object 'base-html-sheet) 
      (the-object object (set-slot! :$$ta2-object self)))
    (the (set-slot! :root-object object)))
   
   (update-node! 
    (object)
    ;;
    ;; FLAG -- factor out repeated code between here and click-mode.lisp.
    ;;
    (let ((standalone? (not (eql (the root) (the root-object root)))))
      (if standalone?
          (progn
            (when object  (the-object object update!))
            (funcall (the tatu-update-function))
            (the viewport (toggle-view-toggle!)))
        (let ((root (the root))
              (tatu-root-path (the root-path)))
          (when object (the-object object update!))
          (the-object root (follow-root-path tatu-root-path) 
                      viewport toggle-view-toggle!)
          (list :go-to (the-object root (follow-root-path tatu-root-path)))))))
   
   (up-root!
    ()
    (unless (the at-true-root?)
      (the (set-slot! :root-object (the root-object parent)))))
   
   (reset-root!
    ()
    (unless (the at-true-root?)
      (the (set-slot! :root-object (the root-object-object)))))
   
   (perform-action!
    (object &key kids-error (click-mode (the click-mode value)))
    (when *debug?* (print-variables (the-object object root-path) click-mode))
    (ecase click-mode
      (:inspect (the inspector (set-object! object)))
      (:reset-root (the (reset-root! object)))
      (:set-root (the (set-root! object)))
      (:update-node (the (update-node! object)))
      (:draw-leaves (the viewport (draw-leaves! object)))
      (:draw-node (the viewport (draw-node! object)))
      (:add-leaves (the viewport (add-leaves! object)))
      (:add-leaves* (the viewport (add-leaves*! object)))
      (:delete-leaves (the viewport (delete-leaves! object)))
      (:add-node (the viewport (add-node! object)))
      
      (:break 
       (set-self object)
       (let ((*package* (symbol-package (the-object object root type))))
                
         (format t "~&
~aSelf is now set to to ~s, you may use command-line interaction....~%~%" 
                 (if kids-error
                     (format nil "~&
NOTE: Children cannot expand -- evaluate (the children) to reproduce the error. 
The error was: ~a

" 
                             kids-error) "") object)))
      
      ;;(:uh (the viewport (set-sheet-object! object)))
      
      ))))




(define-lens (html-format assembly)()
  
  :output-functions
  ((main-sheet-body 
    ()
    
    (print-messages have-valid-instance? use-jquery?)
    
    (if (the have-valid-instance?)
        (write-the ui-sheet)
      (write-the part-type-form main-sheet)))
   
   (ui-sheet
    ()
    (with-cl-who (:indent t) 
      ;; This is the content of the Body of the basic
      ;; tasty layout Outer layout: center (has a Layout), west (has
      ;; additional Layout), north (menu), south (footer) Inner layout
      ;; in outer-center: center (main viewport), east (auxilary
      ;; views+help), north (contexthelp), west (inspector in 3 panes
      ;; side by side) Inner layout in outer-west: center (main tree),
      ;; south (inspector when traditional ta layout selected)
      
      ((:div :id "loader-message")
        ((:span :class "ui-icon ui-icon info" style"float: left; margin: 0.3em;")
       (:span "Application is Loading")))
      
      ((:div :id "tabs" :class "page-layout-center")
       
       
       ((:ul :id "TabButtons" :class "pane")
        (:li ((:a :href "#ApplicationLayout")(:span "Application")))
        (:li ((:a :href "#Documentation")(:span "Documentation")))
        )
       
       ;; wrap tab-panels in ui-layout-content div 
       ((:div :id "TabPanelsContainer" :class "pane")
        
        ;; TAB #1 
        ((:div :id "ApplicationLayout")
         
         ((:div :id "InnerLayout" :class "outer-center pane")
          
          ((:div :class "inner-center ui-widget pane")
           ((:div :class "header ui-widget-header")"Viewport")
           ((:div :class "ui-widget-content")(str (the viewport main-div)))
           ((:div :class "footer ui-widget-header")
                    
            (str (the footer-ui-widget-header main-div))))
          
          
          ((:div :class "inner-west ui-widget pane")
           ((:div :class "header ui-widget-header")"Inspector")
           ((:div :class "ui-widget-content")
            (str (the inspector main-div))))
          
          ((:div :class "inner-north ui-widget pane")
           ((:div :class "header ui-widget-header")"Inner North")
           ((:div :class "ui-widget-content")(:span "Inner North"))))
         
         ((:div :class "outer-west ui-widget pane")
          ((:div :class "header ui-widget-header")"Tree")
          ((:div :class "ui-widget-content")(str (the tree main-div)))
          ((:div :class "footer ui-widget-header") 
           "Click-mode:" (str (the tree-status-object main-div)))
          )
         
         ((:div :class "outer-north ui-widget pane")
	  ((:div :onmouseover "InnerLayout.allowOverflow(this)")
	   (str (the menu-section main-div))))
         
         ((:div :class "outer-south ui-widget pane")
          ((:div :class "footer ui-widget-header")
           ((:span :class "fltrt") 
            "powered by " ((:a :href "http://www.genworks.com" :target "_new")"Genworks GDL")
            "- empowered by " ((:a :href "http://www.ke-works.com" :target "_new")"KE-works"))
           ((:span) "GDL status: ")
           ((:span :id "gdlStatus")"Done.")
           (when *tasty-developing?*
             (htm            
              (:span " | ")
              (:span

               ((:span :onclick (the (gdl-ajax-call :function-key :update-object!
                                                    :arguments (list self)))
                       :title "Full Update of Tasty Browser and Object in Tree (for tasty development)"
                       :style "color: blue; cursor: pointer;") "Update!")
               " | "
               ((:span :onclick (the (gdl-ajax-call :function-key :set-self!
                                                    :arguments (list self)))
                       :title "Set self to this tasty object (for tasty development)"
                       :style "color: blue; cursor: pointer;") "Set Self!"))

              
              ;;(write-the development-links)
              
              ;;(write-the break-link) (htm " | ")(write-the update-full-link)

              ))
           
           #+nil
           (when (typep (the root-object) 'base-html-sheet)
             (htm (:span " | ")
                  ((:a :href (the root-object url)) "Visit UI")))))

         
         ((:div :class "outer-east ui-widget pane")
          ((:div :class "header ui-widget-header")"Auxiliary")
          ((:div :class "ui-widget-content")(:span "Auxiliary Pane Content"))
          
          )
         
         );; #ApplicationLayout 
        
        ;; TAB #2 
        ((:div :id "Documentation" :class "ui-tabs-hide pane")
         ((:iframe :class "ui-widget-content" :src "/yadd" :width "100%" :height "500px"))
         )
        
        );; END TabPanelsContainer 
       
       )
      
      ))))
