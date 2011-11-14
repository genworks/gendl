(in-package :ta2)

(define-object action-object (base-html-sheet)
  :input-slots
  (node tatu-root tatu-color click-mode kids-error)

  :computed-slots
  ((strings-for-display (multiple-value-bind (strings error)
			    (ignore-errors (the node strings-for-display))
			  (cond ((typep error 'error)
				 (format nil "! ~a ! from strings-for-display" error))
				((the color-error?)
				 (format nil "~a ! ~a ! from color-hex" strings (the color-or-error)))
				(t strings))))
   
   (color-or-error (multiple-value-bind (color error)
		       (ignore-errors (the node color-hex))
		     (if (typep error 'error) error color)))
   
   (color-error? (typep (the color-or-error) 'error))
   
   (color-hex (if (the color-error?) (lookup-color :red :format :hex) (the color-or-error))))
  
  :functions
  ((before-present! 
    ()
    ;;
    ;; FLAG -- consider passing just the perform-action function and not the whole object
    ;;
    
    (when (not (eql (the click-mode) :ui))
      (the tatu-root (perform-action! (the node) :kids-error (the kids-error)))))))
   

