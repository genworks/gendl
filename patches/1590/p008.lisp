(in-package :geom-base)

;;
;; FLAG -- update compiler to do proper eval-when for gdl-output::
;; methods, to make the following unecessary to avoid warning.
;;
(defgeneric gdl-output::pdf-command (format object skin &rest args))



(#+allegro 
 excl:without-package-locks #-allegro progn
 (#+allegro 
  excl:without-redefinition-warnings
  #-allegro progn
  (define-lens (pdf view-object-cache)()
    :output-functions
    ((pdf-command 
      (mode coords)
      (ecase mode 
	(:move (let ((coord (first coords))) (pdf:move-to (%get-x% coord) (%get-y% coord))))
	(:line (let ((coord (first coords))) (pdf:line-to (%get-x% coord) (%get-y% coord))))
	(:curve (destructuring-bind (c1 c2 end) coords
		  (pdf:bezier-to (%get-x% c1) (%get-y% c1) (%get-x% c2) 
				 (%get-y% c2) (%get-x% end) (%get-y% end))))))
     (lines-and-curves
      ()
      (when (typep (the object) 'gdl::gdl-basis)
	(let ((object (the object)) (2d-vertices (the vertex-array-2d-scaled)))
	  (let ((line-index-pairs (the-object object %line-vertex-indices%))
		(curve-index-quadruples (the-object object %curve-vertex-indices%))
		(path-info (the path-info-2d-scaled)))
	    (pdf:with-saved-state (write-the-object object line-thickness-setting)
          
				  (write-the-object object dash-pattern-setting)
          
				  (write-the-object object rgb-stroke-setting)
				
				  (if path-info
				      (let (mode coords)
					(dolist (item path-info)
					  (if (keywordp item) 
					      (progn
						(print-variables mode coords)
						(when (and mode coords) (write-the (pdf-command mode (nreverse coords))))
						(setq mode item) (setq coords nil))
					      (push item coords)))
					(write-the (pdf-command mode (nreverse coords))))
				    
				      (progn
					(mapc #'(lambda(line-index-pair)
						  (destructuring-bind (start-index end-index) line-index-pair
						    (let ((start (svref 2d-vertices start-index)) 
							  (end (svref 2d-vertices end-index)))
						      (pdf:move-to (%get-x% start) (%get-y% start))
						      (pdf:line-to (%get-x% end)(%get-y% end))))) line-index-pairs)
            
            
					(let (start end prev-end)
              
					  (mapc #'(lambda(curve-index-quadruple)
						    (destructuring-bind (start-index c1-index c2-index end-index) 
							curve-index-quadruple
						      (declare (ignorable start-index))
                          
						      (setq prev-end end)
						      (setq start (svref 2d-vertices start-index)
							    end (svref 2d-vertices end-index))
                          
						      (let ((c1 (svref 2d-vertices c1-index))
							    (c2 (svref 2d-vertices c2-index)))

							(unless (and start prev-end (coincident-point? start prev-end))
							  (pdf:move-to (%get-x% start) (%get-y% start)))
                            
							(pdf:bezier-to (%get-x% c1) (%get-y% c1) (%get-x% c2) 
								       (%get-y% c2) (%get-x% end) (%get-y% end)))))
						curve-index-quadruples))))
            
            
				  (if (the-object object fill-color-decimal)
				      (pdf:fill-and-stroke)
				      (pdf:stroke)))))))))))
