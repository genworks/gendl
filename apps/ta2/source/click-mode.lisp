;;
;; Copyright 2013 Genworks International
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

(in-package :ta2)


;;
;; In general, return a URI for where to fetch the latest html for a div id.
;;
;;
;; FLAG -- why does this need base-html-sheet?
;;
(define-object click-mode (base-html-sheet)
  :input-slots 
  (tatu-root pop-operation-function clear-viewport-function object-update-function object-update-full-function  
   toggle-view-toggle-function up-root-function reset-root-function debug-function
   tatu-update-function  root-object)
  
  :computed-slots
  ((dom-id "ta2clickmode")
   
   (click-mode :inspect :settable)
   
   (color "Default" :settable)
   (line-thickness -1 :settable)
   
   (tree-width 400 :settable)
   (tree-length 300 :settable)
   (page-length 850 :settable)
    
   (color-selected (unless (string-equal (the color) "default") (format nil "#~a" (the color))))
   (line-thickness-selected (unless (= (the line-thickness) -1) (the line-thickness)))
   
   (respondent (the tatu-root))
   
   (mode-labels (let ((ht (make-hash-table)))
		  (dolist (pair (list (list :inspect "I...")
				      (list :draw-leaves "DL...")
				      (list :draw-node "DN...")
				      (list :add-leaves "AL...")
				      (list :add-leaves* "AL*...")
				      (list :delete-leaves "DL...")
				      (list :add-node "AN...")
				      (list :break "B...")
				      (list :ui "UI...")
				      (list :uh "UH...")
				      (list :clear! "CL!")
				      (list :update! "U!")
				      (list :update-full! "UF!")
				      (list :set-root "SR...")
				      (list :update-node "UN...")
				      (list :up-root! "UR!")
				      (list :reset-root! "RR!")) ht)
		    (setf (gethash (first pair) ht) (second pair))))))
  
  :hidden-objects
  (
   
   (color-chooser :type 'color-chooser)
   
   (line-thickness-selectors :type 'vanilla-mixin
			     :pseudo-inputs (line-thickness)
			     :sequence (:size 5)
			     :line-thickness (nth (the-child index) (list 0.5 1.0 2.0 3.0 4.0))
			     )
   (line-thickness-default-setter :type 'general-callback)))



;;
;; FLAG -- merge these into generic setter object
;;

(define-object general-callback (base-html-sheet)
  :input-slots (return-object function)
  :functions
  ((before-present! () (funcall (the function)))
   (write-html-sheet () (the return-object write-html-sheet))))
  


(define-lens (html-format click-mode)()
  
  ;; :amend? t
  
  :output-functions
  ((inner-html
    ()
    (with-html-form (:name "click-mode" :on-submit "return false;")
      ((:table :border 0 :cellpadding 0 :cellspacing 0)
       (:tr 
	(:td
	 ((:table :border 0 :cellpadding 0 :cellspacing 1)
	  (:tr 
	   (let ((mode-labels (the mode-labels)))
	     (dolist (mode (list :inspect :draw-leaves :draw-node :add-leaves :add-leaves* :delete-leaves 
				 :add-node :set-root :update-node :break :ui)) ;; :uh
	       (let ((label (gethash mode mode-labels))
		     (tooltip 
		      (ecase mode 
			(:inspect "Set the inspector for subsequently seleted objects")
			(:draw-leaves "Draw leaves (replacing existing displayed geometry) in graphics viewport for subsequently seleted objects.")
			(:draw-node "Draw node (replacing existing displayed geometry) in graphics viewport for subsequently seleted objects.")
			(:add-leaves "Add leaves in graphics viewport for subsequently selected objects.")
			(:add-leaves* "Add leaves individually in graphics viewport (So they can be individually deleted later)")
			(:delete-leaves "Delete leaves in graphics viewport for subsequently seleted objects.")
			(:add-node "Add node in graphics viewport for subsequently seleted objects.") 
			(:set-root "Set the root of displayed tree to the next selected object.") 
			(:update-node "Apply Update action to the next selected object.")
			(:break "Enter debugger with self set to next selected object.") 
			(:ui "Enter the base-html-sheet UI (if one exists) for the next selected object.") 
			(:uh "Display the base-html-sheet UI (if one exists) in place of the Graphics Viewport area for the next selected object."))))

			       
			       
		 (if (eql (the click-mode) mode)
		     (html ((:td :bgcolor (getf (the colors-default) :headers)) (:princ label)))
		   (html (:td ((:input :type :submit :name mode :value label
				       :title tooltip
				       :onclick (format nil "return ta2clickbutton('~a', '~a');" (the instance-id) mode)
				       :onmouseover (format nil "window.status='~a'; return true;" tooltip)
				       :onmouseout  "window.status=''; return true;"))))))))
	   (:td ((:span :id "ta2status") "Done."))
			 
	   ))))
       (:tr (:td
	     ((:table :border 0 :cellpadding 0 :cellspacing 1)
	      (:tr (let ((mode-labels (the mode-labels)))
		     (dolist (mode (remove nil (list :clear! :update! 
						     (when (not (eql (the root-object) (the root-object root))) :update-full!)
						     :up-root! :reset-root!)))
		       (let ((label (gethash mode mode-labels))
			     (tooltip (ecase mode 
					(:clear! "Clear Graphics Viewport.") 
					(:update! "Update from current node down for any changed object definitions.")
					(:update-full! "Update from root for any changed object definitions.")
					(:up-root! "Move up one level in tree if you are not already at the root level.") 
					(:reset-root! "Reset the tree to its original root level."))))
			 (if (eql (the click-mode) mode)
			     (html ((:td :bgcolor (getf (the colors-default) :headers)) (:princ label)))
			   (html (:td ((:input :type :submit :name mode :value label
					       :title tooltip
					       :onclick (format nil "return ta2clickbutton('~a', '~a');" (the instance-id) mode)
					       :onmouseover (format nil "window.status='~a'; return true;" tooltip)
					       :onmouseout  "window.status=''; return true;"))))))))
		   (:td :br)
		   (:td
		    (if (= (the line-thickness) -1) (html (:small (:i "Default")))
				
		      (let ((image-file (cond ((<= (the line-thickness)  1.0) "line-1.gif")
					      ((<= (the line-thickness) 2.0) "line-2.gif")
					      ((<= (the line-thickness) 3.0) "line-3.gif")
					      (t "line-4.gif"))))
			(html
			 ((:span :style "cursor: pointer;"
				 :title "Reset Line Thickness to Default"
				 :onclick (format nil "return ta2setcontrol('~a', '~s', 'gdl::restore-default');"
						  (the instance-id) :line-thickness))
			  ((:img :border nil :src (format nil "/images/gwl/~a" image-file))))))))
		   (:td
		    ((:table :border 0 :cellpadding 0 :cellspacing 0)
		     (mapc #'(lambda(selector image)
			       (html 
				(:tr
				 (:td
				  ((:span :style "cursor: pointer;" 
					  :title "Set Line Thickness"
					  :onclick (format nil "return ta2setcontrol('~a', '~s', '~a');" 
							   (the instance-id) :line-thickness (the-object selector line-thickness)))
				   ((:img :border 0 :src (format nil "/images/gwl/~a" image))))))))
			   (list-elements (the line-thickness-selectors))
			   (list "line-1.gif" "line-1.gif" "line-2.gif" "line-3.gif" "line-4.gif"))))
			 
			 
		   (:td 
		    
		    (:table
		     (:tr
		      (:td (:table (:tr ((:td :if* (the color-selected) 
					      :style (format nil "cursor:pointer; background: ~a; height: 10px; width: 10px;" 
							     (the color-selected))
					      :title "Reset Color to Defaults"
					      :onclick (format nil "return ta2setcontrol('~a', '~s', 'gdl::restore-default');"
							       (the instance-id) :color))
					 (unless (the color-selected) (html (:princ (the color))))))))
		      (:td (write-the color-chooser (full-grid)))))))))))))))
 

(define-object color-chooser ()
  :input-slots
  ((components (list "FF" "CC" "99" "66" "33" "00"))))

(define-lens (html-format color-chooser)()
  :output-functions
  ((full-grid 
    (&key (cell-size 7))
    (let ((colors (the components)))
      (html ((:table :border "0" :cellspacing "0" :cellpadding "0")
	     (:tr (dolist (red colors)
		    (html 
		     (:td 
		      ((:table :border "0" :cellspacing "0" :cellpadding "0")
		       (dolist (green colors)
			 (html 
			  (:tr (dolist (blue colors)
				 (html ((:td :style (format nil "cursor: pointer; background: #~a~a~a; height: ~apx; width: ~apx;" 
							    red green blue
							    cell-size cell-size)
					     :title (format nil "Set Color for Next Matched Object(s) to Hex RGB: ~a~a~a"
							    red green blue)
					     :onclick (format nil "return ta2setcontrol('~a', '~s', '~a');"
							      (the instance-id)
							      :color
							      (format nil "~a~a~a" red green blue))))))))))))))))))))
