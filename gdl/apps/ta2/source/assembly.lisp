(in-package :ta2)

;;
;; Clean up this file.
;;
(define-object assembly (base-html-sheet base-object)
  
  :input-slots
  (;;(root-object nil :settable)
   
   (root-object (the root-object-object) :settable)
   
   (page-length (the click-mode page-length))
   (initial-tree-depth 1)
   (package-default :gdl-user)
   (colors-default (list :headers (gethash :yellow *color-table*)))
   (color-selected (the click-mode color-selected))
   (line-thickness-selected (the click-mode line-thickness-selected))
   (inspector-relative-font-size "-1")
   (inspector-dimensions (list :width (the tree width) :length (- (the page-length) (the tree length))))
   (tree-dimensions (list :width (the click-mode tree-width) :length (the click-mode tree-length)))
   (viewport-dimensions (list :width 800 :length 550))
   (tatu-update-function 
    #'(lambda() 
	(the (set-slot! :root-object (the root-object)))(the tree update!))))

  
  :trickle-down-slots (colors-default)
  
  
  :computed-slots
  ((html-sections (list (the tree) (the click-mode) (the viewport) (the inspector)))
   
   (root-object-type 'null-part :settable)
   
   )

  
  :hidden-objects
  (
   (root-object-object :type (the root-object-type)
		       :tree-root (the-child)
		       :root (the-child)
		       :$$ta2-object self
		       :strings-for-display (format nil "~s" (the root-object-type)))

   
   (part-type-form :type 'part-type-form
		   :pass-down (package-default)
		   :tatu-root self
		   :set-root-function #'(lambda(object) (the (set-root! object)))
		   :make-instance-function #'(lambda(type) (the (make-root-instance type))))
   
   (tree :type 'object-tree
	 :tatu-root self
	 :length (getf (the tree-dimensions) :length)
	 :width (getf (the tree-dimensions) :width)
	 :display-controls-hash (the viewport display-controls-hash)
	 :click-mode (the click-mode click-mode)
	 :initial-depth (the initial-tree-depth)
	 :pass-down (root-object))
   
   (click-mode :type 'click-mode
	       :clear-viewport-function #'(lambda() (the viewport clear!))
	       :object-update-function #'(lambda() (when (the root-object) (the root-object update!)))
	       :object-update-full-function #'(lambda() (when (the root-object) (the root-object root update!)))
	       :toggle-view-toggle-function #'(lambda() (the viewport (toggle-view-toggle!)))
	       :tatu-update-function (the tatu-update-function)
	       :up-root-function #'(lambda() (the up-root!))
	       :reset-root-function #'(lambda() (the reset-root!))
	       :pop-operation-function #'(lambda() (the viewport pop-operation!))
	       :debug-function #'(lambda() (the viewport enter-debugger!))
	       :pass-down (root-object)
	       :tatu-root self)
   
   (viewport :type 'viewport
	     :pass-down (root-object color-selected line-thickness-selected)
	     :inspected-object (the inspector node)
	     :length (getf (the viewport-dimensions) :length (getf (the viewport-dimensions) :height))
	     :width (getf (the viewport-dimensions) :width)
	     :tatu-root self)
   
   (inspector :type 'inspector
	      :pass-down (root-object colors-default)
	      :length (getf (the inspector-dimensions) :length)
	      :width (getf (the inspector-dimensions) :width)
	      :click-mode (the click-mode click-mode)
	      :relative-font-size (the inspector-relative-font-size)
	      :display-controls-hash (the viewport display-controls-hash)
	      :tatu-root self))
  
  
  :functions
  (
   (make-root-instance
    (type)
    (the (set-slot! :root-object-type
		    (let ((*package* (find-package (the package-default))))
		      (read-safe-string type)))))

   (set-root!
    (object)
    (when (typep object 'base-html-sheet) (the-object object (set-slot! :$$ta2-object self)))
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
	  (the-object root (follow-root-path tatu-root-path) viewport toggle-view-toggle!)
	  (list :go-to (the-object root (follow-root-path tatu-root-path)))))))
   
   (up-root!
    ()
    (unless (typep (the root-object parent) 'assembly)
      (the (set-slot! :root-object (the root-object parent)))))
   
   (reset-root!
    ()
    (when (not (eql (the root-object) (the root-object-object)))
      (the (set-slot! :root-object (the root-object-object)))))
   
   (perform-action!
    (object &key kids-error)
    (ecase (the click-mode click-mode)
      (:inspect (the inspector (set-object! object)))
      (:set-root (the (set-root! object)))
      (:update-node (the (update-node! object)))
      (:draw-leaves (the viewport (draw-leaves! object)))
      (:draw-node (the viewport (draw-node! object)))
      (:add-leaves (the viewport (add-leaves! object)))
      (:add-leaves* (the viewport (add-leaves*! object)))
      (:delete-leaves (the viewport (delete-leaves! object)))
      (:add-node (the viewport (add-node! object)))
      
      (:break (set-self object)
	      (let ((*package* (symbol-package (the-object object root type))))
		
		(format t "~aSelf is now set to to ~s, you may use command-line interaction....~%~%" 
			(if kids-error
			    (format nil "~&NOTE: Children cannot expand -- evaluate (the children) to reproduce the error. 
The error was: ~a~%~%" kids-error) "")
			object)
		
		
		#+nil
		(break "~aDebugging with self set to ~s..." 
		       (if kids-error
			   (format nil "~&NOTE: Children cannot expand -- evaluate (the children) to reproduce the error. 
The error was: ~a~%~%" kids-error) "")
		       object)))
      
      
      (:uh (the viewport (set-sheet-object! object)))))))


(define-lens (html-format assembly)()
  :output-functions
  ((main-sheet
    ()
    
    (if (typep (the root-object) 'null-part) (write-the part-type-form main-sheet)
      (write-the (tatu-sheet))))
    
   
   (tatu-sheet
    ()
    
    (html
     ;;(format *html-stream* "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">~%")
     ;;(format *html-stream* "<!doctype HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">~%")
     (:html
      (:head (:title "Genworks Ta2: " (:princ (the root-object strings-for-display)))
	     ((:script :language "JavaScript" :type "text/javascript")
	      (:princ *javascript-functions*)))
      (:body
       ((:table :cellpadding 1 :cellspacing 0 :bgcolor (gethash :grey-light-very *color-table*))
	(:tr 
	 
	 ((:div :style (format nil "overflow:auto; width: ~a" (the tree width)))
	  ((:td :valign :top :width (format nil "~a" (the tree width)))
	   ((:table :border 0 :cellpadding 1 :cellspacing 0 :bgcolor (gethash :blue-neon *color-table*)) 
	    (:tr
	     (:td
	      ((:div :id (the tree dom-id) :style (format nil "overflow:auto; height: ~apx; width: ~apx" 
							  (the tree length)
							  (the tree width)))
	       (:princ (the tree main-view)))))
	    (:tr
	     (:td
	      ((:div :style (format nil "overflow:auto; height: ~apx; width: ~apx;" 
				    (the inspector length)
				    (the tree width))
		     :id (the inspector dom-id))
	       (:princ (the inspector main-view))))))))
	 ((:td :valign :top) 
	  ((:table :cellpadding 1 :cellspacing 0 :bgcolor (gethash :blue-neon *color-table*))
	   (:tr (:td ((:div :id (the click-mode dom-id)) (:princ (the click-mode main-view)))))
	   (:tr ((:td :bgcolor :white) ((:div :id (the viewport dom-id))
					(:princ (the viewport main-view)))))))))))))))





