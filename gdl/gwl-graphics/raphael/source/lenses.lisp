;;
;; Copyright 20012 Genworks International
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

(in-package :raphael)

(defparameter *result* nil)

(defun one-line (string)
  (glisp:replace-regexp (glisp:replace-regexp string (format nil "~%") " ") "'" "\\'" ))

(define-lens (raphael base-drawing)()
  :output-functions
  ((cad-output
    ()
    (let ((view-center (if (the user-center) 
                           (scalar*vector (the user-scale) (the user-center)) 
                         (make-point 0 0 0))))
      (with-format-slots (view)
        (let ((parent-scale (when view (the-object view view-scale-total))))
          (mapc #'(lambda(child-view)
                    (let ((old-scale (the-object child-view user-scale)))
                      (when parent-scale (the-object child-view (set-slot! :user-scale parent-scale)))
                      (let ((width (the-object child-view width))
                            (length (the-object child-view length)))

                        (format *stream* "~&~%var paper = Raphael('~a', ~a, ~a);~%"
                                (the raphael-canvas-id) width length)

                        (with-translated-state (:raphael (make-point (- (get-x view-center)) 
                                                                     (- (get-y view-center))))
                          (write-the-object child-view cad-output)))
                      (when parent-scale (the-object child-view (set-slot! :user-scale old-scale)))))
                (the views))))))))



(define-lens (raphael base-view)()
  
  :output-functions
  ((cad-output
    ()
    
    (set-format-slot view self)
    
    (let ((center (the center)) 

          (view-center (scalar*vector (the view-scale-total)
                                      (keyed-transform*vector (the view-transform)
                                                              (the view-center)))))
      
      (with-translated-state (:raphael 
                              (make-point (+ (get-x center) (half (the width)))
                                          (+ (get-y center) (half (the length)))))
        
        
        
        
        
        ;;
        ;; FLAG - look into capturing this translate in the
        ;; vertex-array-2d-scaled in view-object-cache.  so it will
        ;; become unecessary here.
        ;;
        (with-translated-state (:raphael (make-vector (- (get-x view-center)) 
                                                      (- (get-y view-center))))
          
          (dolist (cache (list-elements (the object-caches))) 
            (write-the-object cache lines-and-curves)))
        
        (dolist (cache (list-elements (the object-caches))) 
          (write-the-object cache object cad-output))
        
        (with-format-slots (view)
          (set-format-slot view nil)
          (dolist (cache (list-elements (the annotation-caches)))
            ;;
            ;; FLAG -- why are these both here?
            ;;
            (write-the-object cache object cad-output) 
            (write-the-object cache lines-and-curves))
          (set-format-slot view view))
        
        
        ))
    
    
    
    
    (set-format-slot view nil))))



(define-lens (raphael geom-base::view-object-cache)()
  :output-functions
  ((lines-and-curves
    ()
    (unless (zerop (length (the vertex-array-2d-scaled)))
      (let ((object (the object)) 
            (2d-vertices 
             (map 'vector #'(lambda(vertex) 
                              (let ((point 
                                     (add-vectors (subseq vertex 0 2) 
                                                  geom-base:*raphael-translation*)))
                                (make-point (get-x point) 
                                            (- (the length) (get-y point)))))
                  (the vertex-array-2d-scaled))))
      
        ;;
        ;; FLAG -- update this to handle global-filleted-polyline and any other odd types.
        ;;
        
        (let ((line-index-pairs (the-object object %line-vertex-indices%))
              (curve-index-quadruples (the-object object %curve-vertex-indices%))
              (display-controls (or (geom-base::find-in-hash object *display-controls*)
                                    (the object display-controls)))

              (*read-default-float-format* 'single-float))
          
	  (let (prev-end 
		(move? t))
	    (mapc 
	     #'(lambda(line-index-pair)
		 ;;(setq move? t)
		 (destructuring-bind (start-index end-index) line-index-pair
		   (let ((start (svref 2d-vertices start-index)) 
			 (end (svref 2d-vertices end-index))
			 (name (base64-encode-safe 
				(format nil "~s" (remove :root-object-object 
							 (the-object object  root-path))))))
		     (when nil 
		       (setq move? nil))
		     (setq prev-end end)
		     (with-cl-who ()
		       (str 
			(format 
			 nil 
			 "var ~a_lines = paper.path('~aL ~a ~a').attr({stroke: '~a'});~%"
                       
			 name 
                       
			 (if move? (format nil "M ~a ~a "
					   (to-single-float (get-x start))
					   (to-single-float (get-y start)))
			     "")
			 (to-single-float (get-x end))
			 (to-single-float (get-y end))
                       
			 (or (when (getf display-controls :color)
			       (lookup-color (getf display-controls :color) :format :hex))
			     (the-object object color-hex))))
                       
		       (let ((stroke-width-string
			      (let* ((line-thickness (getf display-controls :line-thickness))
				     (line-thickness (or line-thickness (the object line-thickness)))
				     (line-thickness (if (zerop line-thickness) 1 line-thickness)))
				(if line-thickness
				    (format nil "~a_lines.attr('stroke-width','~a');" 
					    name (to-single-float line-thickness)) ""))))
			 (htm (str stroke-width-string)))))))
	     line-index-pairs))
                      
          (let (result)
            (mapc #'(lambda(curve-index-quadruple)
                      (destructuring-bind (start-index c1-index c2-index end-index) 
                          curve-index-quadruple

                        (let ((start (svref 2d-vertices start-index))
                              (end (svref 2d-vertices end-index))
                              (c1 (svref 2d-vertices c1-index))
                              (c2 (svref 2d-vertices c2-index)))
                          (push (list start c1 c2 end) result)))) curve-index-quadruples)
              
            (setq result (nreverse result))

            (when result

              (let* ((name (base64-encode-safe 
                            (format nil "~s" (remove :root-object-object 
                                                     (the-object object  root-path)))))
                     (line-thickness (getf display-controls :line-thickness))
                     (line-thickness (or line-thickness (the object line-thickness)))
                     (line-thickness (if (zerop line-thickness) 1 line-thickness)))
                  
                
                (let ((start-string 
                       
                       (format nil "var ~a_curves = paper.path('~{~a~}').attr({stroke: '~a'});~%"
                               name
                               (let* (prev-end
				      (move? t)
				      (curve-strings
				       (mapcar 
					#'(lambda(curve)
					    (setq move? t)
					    (destructuring-bind (start c1 c2 end) curve
					      (when (and prev-end (coincident-point? start prev-end))
						(setq move? nil))
					      (setq prev-end end)
					      (format nil "~aC  ~a ~a ~a ~a ~a ~a "
						      (if move? (format nil "M ~a ~a "
									(to-single-float (get-x start))
									(to-single-float (get-y start)))
							  "")
						      (to-single-float (get-x c1) )
						      (to-single-float (get-y c1))
						      (to-single-float (get-x c2) )
						      (to-single-float (get-y c2))
						      (to-single-float (get-x end) )
						      (to-single-float (get-y end))
                                                     
                                                     
						      ))) result)))
                                 curve-strings)
                               (let ((color (or (when (getf display-controls :color)
                                                  (lookup-color (getf display-controls :color) :format :hex))
                                                (the-object object color-hex))))
                                 
                                 (or color "#000"))))
                      
                      ;;
                      ;; FLAG make this into separate output-function to be amended for objects containing a viewport. 
                      ;;
                      (onclick-string nil)
                      #+nil 
                      (onclick-string
                       (when (ignore-errors (and (the viewport) (eql (the viewport digitation-mode) :select-object)))
                         (format nil "~a_curves.node.onclick = function (event) {~a};"
                                 name
                                 (the viewport (gdl-ajax-call :function-key :set-object-to-inspect!
                                                              :arguments (list object))))))
                               
                               
                      
		      
                      (onmouseover-string
		       nil 
			#+nil
			(format nil 
				" ~a_curves.node.onmouseover = function (){~a_curves.attr({'stroke-width': '~a'});};" 
				name name 
				(floor (to-single-float (* (or line-thickness 1) 3)))
				))
                      
                      (onmouseout-string
		       nil
			#+nil
			(format nil " ~a_curves.node.onmouseout = function (){~a_curves.attr({'stroke-width': '~a'});};" 
				name name (to-single-float (or line-thickness 1))))
                      
                      (fill-string
                       (let ((color (or (when (getf display-controls :fill-color)
                                          (lookup-color (getf display-controls :fill-color) :format :hex))
                                        (the-object object fill-color-hex))))
                         (if color
                             (format nil "~a_curves.attr('fill','~a');" 
                                     name color) "")))

                      
                      (stroke-width-string
                       (if line-thickness
                           (format nil "~&~a_curves.attr('stroke-width','~a');" 
                                   name (to-single-float line-thickness))
                         "")))
                    
                  (with-cl-who () 
                    (str start-string) 
                    ;;
                    ;; FLAG -- this has to be conditionalized based on :display-controls 
                    ;;         of the object. 
                    ;;
                    (when onclick-string (str onclick-string))
                    (when onmouseover-string (str onmouseover-string))
                    (when onmouseout-string (str onmouseout-string))
                    (str fill-string) 
                    (str stroke-width-string)
                    )))
                  
                  
              ))))))))



(define-lens (raphael text-line)()
  :output-functions
  
  ((cad-output
    ()
    (let* ((object self)
           (display-controls (or (geom-base::find-in-hash object *display-controls*)
                                 (the-object object display-controls))))

      (with-format-slots (view)
        (let* ((view-scale (if view (the-object view view-scale-total) 1))
               (center (translate (the center) :front (half (half (* 0.7 (the length))))))

	       )

	  (print-variables (the center) view view-scale center )

	  (setq center (if view (the-object view (view-point center)) center))

	  (print-variables center)
	  
          (let ((font-size (* 0.9 (* (the character-size) view-scale)))
                (rotation 
                 (angle-between-vectors-d 
                  geom-base::+rear-vector+ (the (face-normal-vector :rear)) 
                  geom-base::+top-vector+)))
          
	    (setq center (let ((point 
                                (add-vectors (subseq center 0 2) 
                                             geom-base:*raphael-translation*)))
                           (make-point (get-x point)
                                       (- (the-object view length) (get-y point)))))

	    (print-variables center)
          
            (unless (zerop rotation) ;; FLAG -- rotate in raphael(pdf:rotate rotation)
              )
            (with-cl-who () 
              (fmt "paper.text(~a, ~a, \"~a\").attr({\"font\": \"~2,3fpx Arial\", \"antialias\":  \"false\", \"stroke\": \"~a\", \"fill\": \"~a\"});"
                   (get-x center)
                   (get-y center)
                   (the %text-to-draw%)
                   font-size
                   
                   ;;
                   ;; FLAG -- clean up the color fetching.
                   ;; 
                   (or
                    (or (when (getf display-controls :color)
                          (lookup-color (getf display-controls :color) :format :hex))
                        (the-object object color-hex)) "#000")


                   (or (or (when (getf display-controls :fill-color)
                             (lookup-color (getf display-controls :fill-color) :format :hex))
                           (the-object object fill-color-hex)) "#000"))))))))))




(define-lens (raphael global-polyline)()
  :output-functions
  ((cad-output
    ()
    (with-format-slots (view)
      (let ((line-index-pairs (the %%line-vertex-indices%%))
            (2d-vertices (map 'vector
                           (if view
                               #'(lambda(point) 
				   (let ((p (add-vectors (let ((point (subseq (the-object view (view-point point)) 0 2)))
							   point)
							 geom-base:*raphael-translation*)))
				     (make-point (get-x p) (- (the-object view length) (get-y p)))
				     ))
			       #'identity) (the %%vertex-array%%)))
            (name (base64-encode-safe 
                     (format nil "~s" (remove :root-object-object 
                                              (the  root-path))))))
        
         (with-cl-who ()
          (let ((*read-default-float-format* 'single-float)
		(start (aref 2d-vertices (first (first line-index-pairs)))))
            (str 
             (format 
              nil
              "var ~a_polyline = paper.path('M ~a ~a ~{~a~^ ~}').attr({stroke: '~a'});~%"
              ;; FLAG -- add fill-color
              name
              (to-single-float (get-x start))
              (to-single-float (get-y start))
              
              (mapcar #'(lambda(line-index-pair) 
			  (destructuring-bind (start-index end-index) line-index-pair
			    (declare (ignore start-index))
			    (let ((end   (svref 2d-vertices end-index)))
			      (format nil "L ~a ~a" 
				      (to-single-float (get-x end))
				      (to-single-float (get-y end))))))
			  line-index-pairs)
              
              (the color-hex))))))))))


;;
;; FLAG -- this is not working - sort it out. 
;;
#+nil
(define-lens (raphael global-polyline)()
  :output-functions
  (
   (cad-output
    ()
    (with-format-slots (view)
      (let (;;(line-index-pairs (the %%line-vertex-indices%%))
            (2d-vertices (map 'vector
                           (if view
                               #'(lambda(point) 
                                   (add-vectors (subseq (the-object view (view-point point)) 0 2)
                                                geom-base:*raphael-translation*))
                             #'identity) (the %%vertex-array%%)))
            (name (base64-encode-safe 
                     (format nil "~s" (remove :root-object-object 
                                              (the  root-path))))))
        
         (with-cl-who ()
          (let ((*read-default-float-format* 'single-float))
            (str 
             (format 
              nil
              "var ~a_polyline = paper.path('M ~a ~a ~{~a~^ ~}').attr({stroke: '~a'});~%"
              ;; FLAG -- add fill-color
              name
              (to-single-float (get-x (aref 2d-vertices 0)))
              (to-single-float (get-y (aref 2d-vertices 0)))
              
              (mapcar #'(lambda(point) (format nil "L ~a ~a" 
                                               (to-single-float (get-x point)) 
                                               (to-single-float (get-y point))))
                      (rest (coerce (subseq 2d-vertices 1) 'list)))
              
              (the color-hex))))))))))


(define-lens (raphael point)()
  :output-functions
  ((cad-output
    ()
    (with-format-slots (view)
      
      (let ((center (if view (the-object view (view-point (the center))) (the center)))
            (view-scale (if (and view (the scaled?)) (the-object view view-scale) 1)))
        
        (setq center (let ((point 
                            (add-vectors geom-base:*raphael-translation* (subseq center 0 2))))
                       (make-point (get-x point) (- (the-object view length) (get-y point)))))
        
        (let ((start-x (to-single-float (- (get-x center) (* (the crosshair-length) view-scale))))
              (start-y (to-single-float (- (get-y center) (* (the crosshair-length) view-scale))))
              (end-x (to-single-float (+ (get-x center) (* (the crosshair-length) view-scale))))
              (end-y (to-single-float (+ (get-y center) (* (the crosshair-length) view-scale))))
              (center-x (to-single-float (get-x center)))
              (center-y (to-single-float (get-y center)))
              (name (base64-encode-safe 
                     (format nil "~s" (remove :root-object-object 
                                              (the  root-path))))))
          
          (with-cl-who ()
            (let ((*read-default-float-format* 'single-float))
              (str
               (format
                nil
                "var ~a_lines = paper.path('M ~a ~a L ~a ~a M ~a ~a L ~a ~a').attr({stroke: '~a'});~%"
                name 

                start-x 
                center-y
                end-x 
                center-y
                center-x 
                start-y
                center-x 
                end-y
                (the color-hex)))))))))))




