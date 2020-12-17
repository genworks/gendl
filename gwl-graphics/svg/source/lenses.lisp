;;
;; Copyright 2012 Genworks International
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

(in-package :svg)


(define-lens (svg base-drawing)()
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


                        (with-html-output (*stream*)
                          ((:div
                            :width "100%" :height "100%"
                            :onmousedown (when (the parent vector-graphics-onclick?)
                                           (the parent (gdl-ajax-call :function-key :dig-point :bashee (the parent)
                                                                      :respondent (the parent :respondent)))))
                          
                           ((:svg :id "svg-1" :viewBox (format nil "0 0 ~a ~a" width length)
                                  :width width
                                  :height length
                                  :style (format nil "background-color: ~a" (lookup-color (format-slot background-color)
                                                                                          :format :hex)))
                                 
                            (with-translated-state (:svg (make-point (- (get-x view-center)) 
                                                                     (- (get-y view-center))))
                              (write-the-object child-view cad-output))))))
                      
                      (when parent-scale (the-object child-view (set-slot! :user-scale old-scale)))))
                (the views))))))))


(define-lens (svg base-view)()
  
  :output-functions
  ((cad-output
    ()
    
    (set-format-slot view self)
    
    (let ((center (the center)) 

          (view-center (scalar*vector (the view-scale-total)
                                      (keyed-transform*vector (the view-transform)
                                                              (the view-center)))))
      (with-translated-state (:svg 
                              (make-point (+ (get-x center) (half (the width)))
                                          (+ (get-y center) (half (the length)))))
        ;;
        ;; FLAG - look into capturing this translate in the
        ;; vertex-array-2d-scaled in view-object-cache.  so it will
        ;; become unecessary here.
        ;;
        (with-translated-state (:svg (make-vector (- (get-x view-center)) 
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



(define-lens (vector-graphics geom-base::view-object-cache) ()
  :output-functions
  ((line-path-string
    (line-index-pairs 2d-vertices)
    (when line-index-pairs
      (let (prev-end (move? t))
	(format nil "~{~a~^ ~}"
		(mapcar 
		 #'(lambda(line-index-pair)
		     (destructuring-bind (start-index end-index) line-index-pair
		       (let ((start (svref 2d-vertices start-index)) 
			     (end (svref 2d-vertices end-index)))

			 (setq move? (not (and prev-end (coincident-point? start prev-end))))
			 (setq prev-end end)

			 (format nil "~a ~a ~a"
				 (if move?
				     (format nil "M ~a ~a "
					     (number-round (get-x start) 4)
					     (number-round (get-y start) 4))
				     " ")
				 (number-round (get-x end) 4)
				 (number-round (get-y end) 4)))))
		 line-index-pairs)))))


   (curve-path-string
    (curve-index-quadruples 2d-vertices)
    (when curve-index-quadruples
      (let (coords)
	(mapc #'(lambda(curve-index-quadruple)
		  (destructuring-bind (start-index c1-index c2-index end-index) 
		      curve-index-quadruple
		    (let ((start (svref 2d-vertices start-index))
			  (end (svref 2d-vertices end-index))
			  (c1 (svref 2d-vertices c1-index))
			  (c2 (svref 2d-vertices c2-index)))
		      (push (list start c1 c2 end) coords)))) curve-index-quadruples)
	(setq coords (nreverse coords))
	(let (prev-end (move? t))
	  (format nil "~{~a~}"
		  (mapcar 
		   #'(lambda(curve)
		       (setq move? t)
		       (destructuring-bind (start c1 c2 end) curve
			 (when (and prev-end (coincident-point? start prev-end))
			   (setq move? nil))
			 (setq prev-end end)
			 (format nil "~aC  ~a ~a ~a ~a ~a ~a "
				 (if move? (format nil "M ~a ~a "
						   (number-round (get-x start) 4)
						   (number-round (get-y start) 4)) "")
				 (number-round (get-x c1) 4)
				 (number-round (get-y c1) 4)
				 (number-round (get-x c2) 4)
				 (number-round (get-y c2) 4)
				 (number-round (get-x end) 4)
				 (number-round (get-y end) 4)
                                                     
				 ))) coords))))))


   (other-path-string
    (path-info)
    (format nil "~{~a~^ ~}" (mapcar #'(lambda(component)
					(if (keywordp component)
					    (ecase component (:move "M") (:line "L") (:curve "C"))
					    (format nil "~a ~a"
						    (number-round (get-x component) 4)
						    (number-round (get-y component) 4)))) path-info)))))
    


(define-lens (svg geom-base::view-object-cache)()
  :output-functions
  ((lines-and-curves
    ()
    (unless (zerop (length (the vertex-array-2d-scaled)))
      (let ((object (the object)) 
            (2d-vertices 
	     (unless (the path-info-2d-scaled)
	       (map 'vector #'(lambda(vertex) 
				(let ((point 
				       (add-vectors (subseq vertex 0 2) 
						    geom-base:*svg-translation*)))
				  (make-point (get-x point) 
					      (- (the length) (get-y point)))))
		    (the vertex-array-2d-scaled))))
	    

	    (path-info (when (the path-info-2d-scaled)
			 (mapcar #'(lambda(component)
				     (if (keywordp component) component
					 (let ((point 
						(add-vectors component geom-base:*svg-translation*)))
					   (make-point (get-x point) 
						       (- (the length) (get-y point))))))
				 (the path-info-2d-scaled)))))


	(let ((line-index-pairs (when 2d-vertices (the-object object %line-vertex-indices%)))
              (curve-index-quadruples (when 2d-vertices (the-object object %curve-vertex-indices%)))
              (display-controls (or (geom-base::find-in-hash object *display-controls*)
                                    (the object display-controls)))
	      #+nil ;; FLAG -- do we need some kind of unique ID? If so, this name is too long, find something else. 
	      (name (base64-encode-safe 
		     (format nil "~s~s" 
			     (the-object object  root-path)
			     (the  root-path)))))

	  (let ((line-path-string (write-the (line-path-string line-index-pairs 2d-vertices)))
		(curve-path-string (write-the (curve-path-string curve-index-quadruples 2d-vertices)))
		(other-path-string (write-the (other-path-string path-info)))
		(stroke-linejoin (getf display-controls :stroke-linejoin "round"))
		(stroke (or (when (getf display-controls :color)
			      (lookup-color (getf display-controls :color) :format :hex))
			    (the-object object color-hex)))
		(fill (or (when (getf display-controls :fill-color)
			    (lookup-color (getf display-controls :fill-color) :format :hex))
			  (the-object object fill-color-hex) "transparent"))
		(stroke-width (let ((value (getf display-controls :line-thickness)))
				(when value (setq value (number-round value 4))) (or value 1.0)))
		(onclick (cond ((the object onclick-function)
				(the parent parent parent  ;; FLAG -- pass this in!
				     (gdl-ajax-call :function-key :call-onclick-function!
						    :arguments (list object))))
			       ((and (defaulting (the viewport)) (eql (the viewport digitation-mode) :select-object))
				(the viewport (gdl-ajax-call :function-key :set-object-to-inspect!
							     :arguments (list object))))
			       (t nil))))

	    (when (or line-path-string curve-path-string other-path-string)
	      (with-html-output (*stream*)
		((:path :d (string-append line-path-string curve-path-string other-path-string)
			:vector-effect "non-scaling-stroke"
			:stroke-linejoin stroke-linejoin
			:stroke stroke :fill fill :stroke-width stroke-width
			:onclick onclick
			:onmouseover (format nil "this.style.strokeWidth = ~a;" (* stroke-width 3))
			:onmouseout (format nil "this.style.strokeWidth = ~a;" stroke-width))
		 (:title (str (one-line
			       (format nil "~s" (cons 'the (let ((root-path (reverse (the object root-path))))
							     (if (eql (first root-path) :root-object-object)
								 (rest root-path) root-path)))))))))))))))))
	


(define-lens (svg text-line)()
  :output-functions
  
  ((cad-output
    ()
    (format t "~&Fill in svg text-line!!~%")
    )))


(define-lens (svg point)()
  :output-functions
  ((cad-output
    ()
    (format t "~&Fill in svg point!!~%"))))


(define-lens (svg vector)()
  :output-functions
  ((cad-output
    ()
    (format t "~&Fill in svg vector!!~%"))))



