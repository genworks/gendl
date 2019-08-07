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


(in-package :tasty)


(defun publish-tutorial (server)
  (let ((tutorial-file 
	 (or (probe-file (merge-pathnames "../doc/tutorial.pdf" (glisp:executable-homedir-pathname)))
	     (probe-file (merge-pathnames "gendl/documentation/tutorial/pdf/tutorial.pdf" glisp:*genworks-source-home*))
	     (warn "tutorial.pdf not found in distribution doc/ directory or source documentation directory for
publishing in tasty Help menu.~%"))))
    (when tutorial-file (publish-file :path "/tutorial.pdf"
				      :server server
				      :file (namestring (truename tutorial-file))))))

(pushnew 'publish-tutorial *publishers*)

(define-object menu-section (sheet-section)

  :input-slots (tatu-root
                uri-static-gwl-images
                uri-static-gwl-unpix
                at-true-root?
                (line-thickness-selected nil :settable)
                
                (color nil :settable)
                
                (show-line-thicknesses? nil :settable)
                
                (show-colors? nil :settable)
                
                (blue-arrow
                 (with-cl-who-string ()
                   ((:span :style "font-weight: 900.0; color: blue;") "&rArr;")))
                
                (green-arrow
                 (with-cl-who-string ()
                   ((:span :style "font-weight: 900.0; color: green;") "&rArr;")))
                
                (purple-arrow
                 (with-cl-who-string ()
                   ((:span :style "font-weight: 900.0; color: purple;") "&rArr;")))
                
                (selected-character-spaces "&nbsp;&nbsp;&nbsp;")
                )


  :computed-slots
  (
   (tree-expand-mode (the tree expand-mode))
   
   (js-to-eval "initMenus();")
   
   (inner-html
    (with-cl-who-string ()

      (str (the menu-view))
      
      (str (the toolbar-view))
      
      ))


   (menu-view
    (with-cl-who-string ()
      ((:ul :id "menu" :class "ui-state-active navbar ui-widget-header")
       
       
       
       #+nil ;; --FLAG -- SvdE @ 12-08-09 -- no functions from the "file" menu are currently in use
       ((:li :id "menuFile" :class "top")
        "File"
        (:ul
         (:li ((:a :href "#")"New"))
         (:li ((:a :href "#")"Open"))
         (:li ((:a :href "#")"Close"))
         ((:li :class "separator"):nbsp)
         (:li ((:a :href "#")"Save"))
         (:li ((:a :href "#")"Save as"))
         ((:li :class "separator"):nbsp)
         (:li ((:a :href "#")"Export"))
         ((:li :class "separator"):nbsp)
         (:li ((:a :href "#")"Exit"))))
       
       
       
       ((:li :id "menuEdit" :class "top")
        "Edit"
        (:ul
         
         (:li ((:a :href "#")"Color"))
         (:li ((:a :href "#")"Stroke"))
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-click-mode!
                                                          :arguments (list :update-node))))
               "Update Object (U!)"))))
       
       
       
       ((:li :id "menuTree" :class "top")
        "Tree"
        (:ul
         
         ((:li :class "separator"):nbsp)
         
         
         ((:li :class "heading")"Object Click Mode:")
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-click-mode!
                                                          :arguments (list :add-node))))
               (fmt "~a&nbsp;Add Node... (AN)"
                    (if (eql (the click-mode value) :add-node) 
                        (the blue-arrow) 
                      (the selected-character-spaces)))))
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-click-mode!
                                                          :arguments (list :add-leaves))))
               (fmt "~a&nbsp;Add Leaves... (AL)"
                    (if (eql (the click-mode value) :add-leaves) 
                        (the blue-arrow) 
                      (the selected-character-spaces)))))
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-click-mode!
                                                          :arguments (list :add-leaves*))))
               (fmt "~a&nbsp;Add Leaves indiv... (AL*)"
                    (if (eql (the click-mode value) :add-leaves*) 
                        (the blue-arrow) 
                      (the selected-character-spaces)))))
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-click-mode!
                                                          :arguments (list :draw-node))))
               (fmt "~a&nbsp;Draw Node (DN)"
                    (if (eql (the click-mode value) :draw-node) 
                        (the blue-arrow) 
                      (the selected-character-spaces)))))
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-click-mode!
                                                          :arguments (list :draw-leaves))))
               (fmt "~a&nbsp;Draw Leaves (DL)"
                    (if (eql (the click-mode value) :draw-leaves) 
                        (the blue-arrow) 
                      (the selected-character-spaces)))))
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-click-mode!
                                                          :arguments (list :delete-leaves))))
               (fmt "~a&nbsp;Clear Leaves (CL)"
                    (if (eql (the click-mode value) :delete-leaves) 
                        (the blue-arrow) 
                      (the selected-character-spaces)))))
         

         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-click-mode!
                                                          :arguments (list :inspect))))
               (fmt "~a&nbsp;Inspect Object (I)"
                    (if (eql (the click-mode value) :inspect) 
                        (the blue-arrow) 
                      (the selected-character-spaces)))))
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-click-mode!
                                                          :arguments (list :break))))
               (fmt "~a&nbsp;Set Self to Object (SS)"
                    (if (eql (the click-mode value) :break) 
                        (the blue-arrow) 
                      (the selected-character-spaces)))))
         
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-click-mode!
                                                          :arguments (list :set-root))))
               (fmt "~a&nbsp;Set Root to Object (SR)"
                    (if (eql (the click-mode value) :set-root) 
                        (the blue-arrow) 
                      (the selected-character-spaces)))))

         
         ((:li :class "heading") "Tree Actions")
         
         
         #+nil
         ((:li :class (when (the at-true-root?) "heading"))
          "Up Root! (UR!)")
         
         #+nil
         ((:li :class (when (the at-true-root?) "heading"))
          "Reset Root! (RR!)")

         
         ((:li :class (when (the at-true-root?) "disabled"))
          (if (the at-true-root?)
              (htm "Up Root! (UR!)")
            (htm ((:a :href "#" :onclick (unless (the at-true-root?)
                                           (the (gdl-ajax-call :function-key :up-root!))))
                  "Up Root! (UR!)"))))
         
         ((:li :class (when (the at-true-root?) "disabled"))
          (if (the at-true-root?)
              (htm "Reset Root! (RR!)")
            (htm ((:a :href "#" :onclick (unless (the at-true-root?)
                                           (the (gdl-ajax-call :function-key :reset-root!))))
                  "Reset Root! (RR!)"))))
         
         ((:li :class "heading") "Tree Navigation ")
         
         
         ;;
         ;; FLAG -- SvdE @ 12-08-09 -- Link functions in tree navigation sub-menu 
         ;;
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-tree-expand-mode!
                                                          :arguments (list :leaves))))
               (fmt "~a&nbsp;Expand to Leaves (L)" 
                    (if (eql (the tree-expand-mode) :leaves) 
                        (the green-arrow) 
                      (the selected-character-spaces)))))
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-tree-expand-mode!
                                                           :arguments (list :children))))
               (fmt "~a&nbsp;Expand to Children (C)" 
                    (if (eql (the tree-expand-mode) :children) 
                        (the green-arrow) 
                      (the selected-character-spaces)))))
         (:li ((:a :href "#"  :onclick (the (gdl-ajax-call :function-key :set-tree-expand-mode!
                                                           :arguments (list :auto-close))))
               (fmt "~a&nbsp;Auto Close (A)"
                    (if (eql (the tree-expand-mode) :auto-close) 
                        (the green-arrow) 
                      (the selected-character-spaces)))))
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-tree-expand-mode!
                                                          :arguments (list :remember))))
               (fmt "~a&nbsp;Remember State (R)"
                    (if (eql (the tree-expand-mode) :remember) 
                        (the green-arrow) 
                      (the selected-character-spaces)))))))
       
       
       
       ((:li :id "menuView" :class "top")
        "View"
        (:ul
         
         
         ((:li :class "heading") "Viewport Actions")
         
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :reset-zoom!)))
               "Fit to Window!"))
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :clear!)))
               "Clear View! (CL!)"))
         
         #+nil ;; FLAG -- function not in use, could be used to restore all display settings to defaults
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-display-defaults!)))
               "Restore Display Defaults!"))
         
         ((:li :class "separator"):nbsp)   
         
         
         ((:li :class "heading") "Image Format")
         
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-image-format!
                                                          :arguments (list :png))))
               (fmt "~a&nbsp;PNG" 
                    (if (eql (the viewport image-format-selector value) :png)
                        (the blue-arrow)
                      (the selected-character-spaces)))))
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-image-format!
                                                          :arguments (list :jpeg))))
               (fmt "~a&nbsp;JPEG" 
                    (if (eql (the viewport image-format-selector value) :jpeg)
                        (the blue-arrow)
                      (the selected-character-spaces)))))

	 #+nil
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-image-format!
                                                          :arguments (list :vrml))));;:web3d))))
               (fmt "~a&nbsp;VRML" 
                    (if (eql (the viewport image-format-selector value) :vrml)
                        (the blue-arrow)
                      (the selected-character-spaces)))))

	 #+nil
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-image-format!
                                                          :arguments (list :x3d))))
               (fmt "~a&nbsp;X3D" 
                    (if (eql (the viewport image-format-selector value) :x3d)
                        (the blue-arrow)
                      (the selected-character-spaces)))))

         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-image-format!
                                                          :arguments (list :x3dom))))
               (fmt "~a&nbsp;X3DOM" 
                    (if (eql (the viewport image-format-selector value) :x3dom)
                        (the blue-arrow)
                      (the selected-character-spaces)))))

         
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-image-format!
                                                          :arguments (list :raphael))))
               
               (fmt "~a&nbsp;SVG/VML" 
                    (if (eql (the viewport image-format-selector value) :raphael)
                        (the blue-arrow)
                      (the selected-character-spaces)))))
         
         ((:li :class "separator"):nbsp) 
         
         
         ((:li :class "heading") "Click Modes")
         
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-zoom!
                                                          :arguments (list :in))))
               (fmt "~a&nbsp;Zoom in"
                    (if (eql (the viewport zoom-mode) :in)
                        (the green-arrow)
                      (the selected-character-spaces)))))
         ;;
         ;;FLAG -- SvdE @ 09-05-20 -- Zoom-out does not work
         ;;
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-zoom!
                                                          :arguments (list :out))))
               (fmt "~a&nbsp;Zoom out"
                    (if (eql (the viewport zoom-mode) :out)
                        (the green-arrow)
                      (the selected-character-spaces)))))
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-digitation-mode!
                                                          :arguments (list :measure-distance))))
               (fmt "~a&nbsp;Measure distance"
                    (if (eql (the viewport digitation-mode) :measure-distance)
                        (the green-arrow)
                      (the selected-character-spaces)))))
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-digitation-mode!
                                                          :arguments (list :report-point))))
               (fmt "~a&nbsp;Get coordinates"
                    (if (eql (the viewport digitation-mode) :report-point)
                        (the green-arrow)
                      (the selected-character-spaces)))))
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-digitation-mode!
                                                          :arguments (list :select-object))))
               
               (fmt "~a&nbsp;Select object"
                    (if (eql (the viewport digitation-mode) :select-object)
                        (the green-arrow)
                      (the selected-character-spaces)))))
         
         
         ((:li :class "separator"):nbsp) 
         
         ((:li :class "heading") "Perspective")        
         
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-view!
                                                          :arguments (list :trimetric))))
               (fmt "~a&nbsp;Trimetric"
                    (if (eql (the viewport view-selector value) :trimetric)
                        (the purple-arrow)
                      (the selected-character-spaces)))))
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-view!
                                                          :arguments (list :front))))
               
               (fmt "~a&nbsp;Front"
                    (if (eql (the viewport view-selector value) :front)
                        (the purple-arrow)
                      (the selected-character-spaces)))))        
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-view!
                                                          :arguments (list :rear))))
               (fmt "~a&nbsp;Rear"
                    (if (eql (the viewport view-selector value) :rear)
                        (the purple-arrow)
                      (the selected-character-spaces)))))        
         
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-view!
                                                          :arguments (list :left))))
               (fmt "~a&nbsp;Left"
                    (if (eql (the viewport view-selector value) :left)
                        (the purple-arrow)
                      (the selected-character-spaces)))))        
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-view!
                                                          :arguments (list :right))))
               (fmt "~a&nbsp;Right"
                    (if (eql (the viewport view-selector value) :right)
                        (the purple-arrow)
                      (the selected-character-spaces)))))        
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-view!
                                                          :arguments (list :top))))
               (fmt "~a&nbsp;Top"
                    (if (eql (the viewport view-selector value) :top)
                        (the purple-arrow)
                      (the selected-character-spaces)))))        
         
         (:li ((:a :href "#" :onclick (the (gdl-ajax-call :function-key :set-view!
                                                          :arguments (list :bottom))))
               (fmt "~a&nbsp;Bottom"
                    (if (eql (the viewport view-selector value) :bottom)
                        (the purple-arrow)
                      (the selected-character-spaces)))))))
       
       
       ((:li :id "menuWindow" :class "top")
        "Window"
        (:ul
         
         (:li ((:a :href "#" 
                   :onclick "initApplicationLayout();PageLayout.open('north');PageLayout.resizeAll()")"Standard Mode"))        
         (:li ((:a :href "#" 
                   :onclick "focusCenter();PageLayout.resizeAll()")"Expert Mode"))
         (:li ((:a :href "#" 
                   :onclick "focusCenter();PageLayout.resizeAll()")"End-User Mode (UI)"))))
       
       
       ((:li :id "menuHelp" :class "top")
        "Help"
        (:ul
	 (:li ((:a :href "/yadd" :target "_new")  "Reference Documentation"))
	 (:li ((:a :href "/tutorial.pdf" :target "_new") "Tutorial/Manual"))
         (:li ((:a :href "http://www.genworks.com" :target "_new") "Genworks Site"))        
         (:li ((:a :href "http://www.genworks.com/dl" :target "_new") "Check for Updates"))        
         (:li ((:a :href "http://www.genworks.com" :target "_new") "Contact Us"))  
         )))))


   (toolbar-view 
    (with-cl-who-string
        ()
      ((:ul :id "toolbar" :class "navbar ui-widget-header")
       
       ((:li :id "tbarDrawObject")
        ((:span 
          :onclick (the (gdl-ajax-call :function-key :set-click-mode!
                                       :arguments (list :add-leaves))))
     
	 (:img :src (string-append (the uri-static-gwl-unpix) "draw.png") 
	      
	       :height "32px" 
	       :width  "32px"
               :alt "Draw Object (AL)"
               :title "Draw Object (AL)"
               :border "0")
         ((:span :class "tbartext")"Draw Object (AL)")))
       
       ((:li :id "tbarInspectObject")
        ((:span 
          :onclick (the (gdl-ajax-call :function-key :set-click-mode!
                                       :arguments (list :inspect))))
         (:img :src (string-append (the uri-static-gwl-unpix) "inspect.png") 
               :height "32px" 
	       :width  "32px"
	       :alt "InspectObject"
               :title "Inspect Object"
               :border "0")
         ((:span :class "tbartext")"Inspect Object (I)")))
      
       ((:li :id "tbarClearObject")
        ((:span 
          :onclick (the (gdl-ajax-call :function-key :set-click-mode!
                                       :arguments (list :delete-leaves))))
         (:img :src (string-append (the uri-static-gwl-unpix) "draw-erase.png") 
               :alt "ClearObject"
	       :height "32px" 
	       :width  "32px"
               :title "Clear Object (DL)"
               :border "0")
         ((:span :class "tbartext")"Clear Object (DL)")))
       
       
       ((:li :class "separator")
        (:img :src (string-append (the uri-static-gwl-unpix) "tbar_separator.png")))

       ((:li :id "tbarClearView")
        ((:span 
          :onclick (the (gdl-ajax-call :function-key :clear!)))
         (:img :src (string-append (the uri-static-gwl-unpix) "cleare-view.png")
	       :height "32px" 
	       :width  "32px"
               :alt "ClearView"
               :title "Clear View (CL!)"
               :border "0")
         ((:span :class "tbartext")"Clear View (CL!)")))
        
       ((:li :id "tbarZoomIn")
        ((:span 
          :onclick (the (gdl-ajax-call :function-key :set-zoom!
                                       :arguments (list :in))))
         (:img :src (string-append (the uri-static-gwl-unpix) "viewmag+.png") 
               :alt "ZoomIn"
	       :height "32px" 
	       :width  "32px"
               :title "Set click mode of the viewport to Zoom In"
               :border "0")
         ((:span :class "tbartext")"Zoom In (+)")))
       
       ((:li :id "tbarZoomOut")
        ((:span 
          :onclick (the (gdl-ajax-call :function-key :set-zoom!
                                       :arguments (list :out))))
         (:img :src (string-append (the uri-static-gwl-unpix) "viewmag-.png") 
               :alt "ZoomOut"
	       :height "32px" 
	       :width  "32px"
               :title "Set click mode of the viewport to Zoom Out"
               :border "0")
         ((:span :class "tbartext")"Zoom Out (-)")))

       
       ((:li :class "separator")
        (:img :src (string-append (the uri-static-gwl-unpix) "tbar_separator.png")))

       (when (typep (the tatu-root root-object) 'base-html-sheet)
         (htm
          ((:li :id "tbarUI")
           ((:span 
             ;; we need to do use javascript as an :a is not possible in the menu
             :onclick (format nil "window.location='~a'" (the tatu-root root-object url)))
            (:img :src (string-append (the uri-static-gwl-unpix) "ui.png") 
		  :height "32px" 
		  :width  "32px"
                  :alt "UI"
                  :title "Change to application User Interface (UI)"
                  :border "0")
            ((:span :class "tbartext")"User interface (UI)")))))

       
       ((:li :class "separator")
        (:img :src (string-append (the uri-static-gwl-unpix) "tbar_separator.png")))
       
       ((:li :id "tbarSetSelf")
        ((:span 
          :onclick (the (gdl-ajax-call :function-key :set-click-mode!
                                       :arguments (list :break))))
         (:img :src (string-append (the uri-static-gwl-unpix) "break.png") 
	       :height "32px" 
	       :width  "32px"
               :alt "Break/Setself"
               :title "Set click mode of an object in the tree to Break and Setself!"
               :border "0")
         ((:span :class "tbartext")"Break/SetSelf")))
       
       ((:li :id "tbarUpdate")
        ((:span 
          :onclick (the (gdl-ajax-call :function-key :update-root!)))
         ;;:arguments (list :inspect))))
         (:img :src (string-append (the uri-static-gwl-unpix) "update.png")
	       :height "32px" 
	       :width  "32px"
               :alt "Update"
               :title "Perform Full Update of the object tree!"
               :border "0")
         ((:span :class "tbartext")"Update")))
       
               (unless (the show-line-thicknesses?)
         (htm ((:li :id "lineThicknesses")
               ((:span :style "cursor: pointer;"
                       :onclick (the (gdl-ajax-call :function-key :set-slot!
                                                    :arguments (list :show-line-thicknesses? t))))
                "Line Thicknesses"))))
       
       (when (the show-line-thicknesses?)
         (htm ((:li :id "lineThickness")
               ((:span :style " height: 32pt; cursor: pointer; "
                       :onclick (the (gdl-ajax-call :function-key :set-slot!
                                                    :arguments (list :show-line-thicknesses? nil))))
                (:sup "X "))
               (dolist (thickness '((0.5 "line-d.gif" )
                                    (1.0 "line-1.gif" )
                                    (2.0 "line-2.gif" )
                                    (3.0 "line-3.gif" )
                                    (4.0 "line-4.gif" )))
                 (htm
                  ((:span :style "cursor: pointer;" 
                          :title (format nil "Set Line Thickness to ~a" (first thickness))
                          :onclick (the (gdl-ajax-call :function-key :set-slot!
                                                       :arguments (list :line-thickness-selected 
                                                                        (first thickness)))))
                    (:img :border 0  :src (format nil "/images/gwl/~a" (second thickness)))))
                  (if (and (the line-thickness-selected)
                           (= (the line-thickness-selected) (first thickness)))
                      (htm ((:span :style " height: 32pt; cursor: pointer; color:green "
                                   :title "Restore Default Line Thickness"
                                   :onclick (the (gdl-ajax-call :function-key :restore-slot-default!
                                                                :arguments (list :line-thickness-selected))))
                             "&lArr; " "")))
              ))))

       (htm ((:li :id "colors")
             (:table (:tr ((:td :style (format nil "width: 25pt; height: 25pt; cursor: pointer; ~a "
                                               (if (the color)
                                                   (format nil "background: #~a;" (the color))
                                                 ""))
                                :onclick (if (the color)
                                             (the (gdl-ajax-call :function-key :restore-slot-default!
                                                                 :arguments (list :color)))
                                           (unless (the show-colors?) 
                                             (the (gdl-ajax-call :function-key :set-slot!
                                                                 :arguments (list :show-colors? t))))))
                           
                           )))))
       
       
       (unless (the show-colors?)
         (htm ((:li :id "colors")
               ((:span :style "cursor: pointer;"
                       :onclick (the (gdl-ajax-call :function-key :set-slot!
                                                    :arguments (list :show-colors? t))))
                "Colors"))))
       
       (when (the show-colors?)
         (htm ((:li :id "colors")
               ((:span :style "cursor: pointer;"
                       :onclick (the (gdl-ajax-call :function-key :set-slot!
                                                    :arguments (list :show-colors? nil))))
                "X")))
         
         (str (the  color-chooser full-grid)))
       
       
       
       
       
       ))))

  :objects ((color-chooser :type 'color-chooser))
  
  :functions
  (
   
   (update-root!
    ()
    (the tatu-root update-root-object!))
   
   (clear!
    ()
    (the viewport clear!))
   
   
   ;;
   ;; FLAG SvdE @ 14-08-09 -- set-display-defaults! should be defined in viewport
   ;;
   (set-display-defaults!
    ()
    (the viewport set-display-defaults!))
  
   
   (up-root!
   ()
   (the parent up-root!)) ;;parent refers to the ojbect 'assembly' in assembly'.lisp
   
   (reset-root!
   ()
   (the parent reset-root!)) ;;parent refers to the ojbect 'assembly' in assembly'.lisp
   
   (set-click-mode!
    (mode)
    (the click-mode (set-slot! :value mode)))
   
   (set-image-format!
    (format)
    (the viewport image-format-selector  (set-slot! :value format)))
   
   (set-view!
    (view)
    (the viewport view-selector (set-slot! :value view)))
   
   (set-zoom!
    (zoom)
    (the viewport (set-slot! :digitized-point nil))
    (the viewport (set-slot! :digitized-distance nil))
    (the viewport (set-slot! :digitation-mode :zoom-and-center))
    (the viewport (set-slot! :zoom-mode zoom)))
   
   (reset-zoom!
    ()
    (the viewport reset-zoom!))
   
   (set-digitation-mode!
    (click-mode)
    #+nil
    (unless (eql click-mode :report-point) 
      (the viewport (set-slot! :digitized-point nil)))
    (the viewport (set-slot! :digitized-point nil))
    (the viewport (set-slot! :digitized-distance nil))
    (the viewport (set-slot! :zoom-mode nil))
    (the viewport (set-slot! :digitation-mode click-mode)))
   
   (set-tree-expand-mode!
    (expand-mode)
    (the tree (set-slot! :expand-mode expand-mode)))
   
   ))


(define-object color-chooser ()
  :input-slots
  ((components (list "FF" "CC" "99" "66" "33" "00"))
   (cell-size 7))
  
  :computed-slots
  ((full-grid
    (let ((colors (the components)))
      (with-cl-who-string ()
        ((:table :border "0" :cellspacing "0" :cellpadding "0")
         (:tr (dolist (red colors)
                (htm
                 (:td 
                  ((:table :border "0" :cellspacing "0" :cellpadding "0")
                   (dolist (green colors)
                     (htm 
                      (:tr (dolist (blue colors)
                             (htm ((:td :style (format nil "cursor: pointer; background: #~a~a~a; height: ~apx; width: ~apx;" 
                                                       red green blue
                                                       (the cell-size) (the cell-size))
                                        :title (format nil "Set Color for Next Matched Object(s) to Hex RGB: ~a~a~a"
                                                       red green blue)
                                        :onclick (the parent
                                                   (gdl-ajax-call :function-key :set-slot!
                                                                  :arguments (list :color (format nil "~a~a~a" red green blue))))
                                        
                                        ))))))))))))))))))
