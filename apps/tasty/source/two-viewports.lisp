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


;;
;; FLAG -- experimental, needs a lot of cleaning up to truly support two viewports.
;; 
#+nil
(in-package :tasty)

#+nil
(define-object two-viewports (assembly)

  :computed-slots
  ((html-sections (list (the tree) 
			(the tree-status-object) 
			(the viewport) 
			;;(the inspector)
			(the viewport2)
			(the viewport-status-object) 
			(the image-status-object)
			)))
  
  :objects
  ((part-type-form :type 'part-type-form
		   :pass-down (package-default)
		   :object-type "sppa::test-node"
		   :tatu-root self
		   :set-root-function #'(lambda(object) (the (set-root! object)))
		   :make-instance-function #'(lambda(type) 
					       (the (make-root-instance type))))
   
   (viewport2 :type 'viewport
	      :pass-down (root-object color-selected line-thickness-selected)
	      :image-format-default :png
	      ;;:display-list-objects (list (the-child inspected-object))
	      :display-list-object-roots 
	      (append (with-error-handling() (the-child inspected-object input-objects))
		      (with-error-handling() (the-child inspected-object explode-lines)))
						
	      :inspected-object (the inspector node)
	      :projection-vector (getf *standard-views* :trimetric)
	      
	      :length 400
	      :width 500
	      :tatu-root self)

   
   (viewport :type 'viewport
	     :pass-down (root-object color-selected line-thickness-selected)
	     :image-format-default :png
	     ;;:display-list-objects (list (the-child inspected-object))
	     :display-list-object-roots 
	     (when (typep (the-child inspected-object) (read-from-string "sppa::process-object"))
	       (list (the-child inspected-object output-object)))
						
	     :inspected-object (the inspector node)
	     
	     :projection-vector (getf *standard-views* :trimetric)
	     
	     :length 400
	     :width 500
	     :tatu-root self)
   )
  )

#+nil
(define-lens (html-format two-viewports)()
  
  :output-functions
  ((main-sheet-body 
    ()
    (print-messages have-valid-instance?)
    (if (the have-valid-instance?)
	(write-the ui-sheet)
	(write-the part-type-form main-sheet)))
   
   (ui-sheet
    ()
    (with-cl-who () 
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
	   ((:div :class "header ui-widget-header")"After")
	   ((:div :class "ui-widget-content")(str (the viewport main-div)))
	   
	   ((:div :class "footer ui-widget-header") 
	    (:span "Click-mode: " (str (the viewport-status-object main-div))
		   " | Image format: " (str (the image-status-object main-div)))))
	  
	  
	  ((:div :class "inner-west ui-widget pane")
	   ((:div :class "header ui-widget-header")"Before")
	   ((:div :class "ui-widget-content")
	    (str (the viewport2 main-div))
	    ))
	  
	  
	  #+nil
	  ((:div :class "inner-west ui-widget pane")
	   ((:div :class "header ui-widget-header")"Inspector")
	   ((:div :class "ui-widget-content")
	    (str (the inspector main-div))
	    ))
	  
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
	    (str (the menu-section main-div))
	  )
	 
	 ((:div :class "outer-south ui-widget pane")
	  ((:div :class "footer ui-widget-header")
	   ((:span :class "fltrt") 
	    "powered by " ((:a :href "http://www.genworks.com" :target "_new")"Genworks GDL")
	    "- empowered by " ((:a :href "http://www.ke-works.com" :target "_new")"KE-works"))
	   ((:span) "GDL status: ")
	   ((:span :id "gdlStatus")"Done.")
	   (:span " | ")
	   (:span
	    (when *developing?*
	      (write-the development-links)
	      
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


;;(publish-gwl-app "/t2v" "tasty::two-viewports" :make-object-args (list :title "Genworks tasty 2vp"))
