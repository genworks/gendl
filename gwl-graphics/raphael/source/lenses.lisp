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

                        (format *stream* "if (typeof paper === 'undefined') {var paper = Raphael('~a', ~a, ~a)};~%

               if (typeof start === 'undefined') {
                var start = function () {
                    this.lastdx ? this.odx += this.lastdx : this.odx = 0;
                    this.lastdy ? this.ody += this.lastdy : this.ody = 0;
                    this.animate({opacity: .5}, 500, \">\");
                },


                move_cb = function (dx, dy) {
                    this.transform(\"T\"+(dx+this.odx)+\",\"+(dy+this.ody));
                    this.lastdx = dx;
                    this.lastdy = dy;
                    this.animate({opacity: .5}, 500, \">\");
                    ~a 
                },

                move = function (dx, dy) {
                    this.transform(\"T\"+(dx+this.odx)+\",\"+(dy+this.ody));
                    this.lastdx = dx;
                    this.lastdy = dy;
                    this.animate({opacity: .5}, 500, \">\");
                },

                up = function () {
                    this.animate({opacity: 1.0}, 500, \">\");
                    ~a 
                }};

"
                                (the raphael-canvas-id) width length
				;;
				;; FLAG -- pass in the containing
				;; base-ajax-graphics-sheet and refer
				;; to that, instead of referring to
				;; the parent here.
				;;
				(the parent (gdl-sjax-call :null-event? t :js-vals? t :function-key :on-drag))
				(the parent (gdl-sjax-call :null-event? t :js-vals? t :function-key :on-drop)))

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
	     (unless (the path-info-2d-scaled)
	       (map 'vector #'(lambda(vertex) 
				(let ((point 
				       (add-vectors (subseq vertex 0 2) 
						    geom-base:*raphael-translation*)))
				  (make-point (get-x point) 
					      (- (the length) (get-y point)))))
		    (the vertex-array-2d-scaled))))
	    

	    (path-info (when (the path-info-2d-scaled)
			 (mapcar #'(lambda(component)
				     (if (keywordp component) component
					 (let ((point 
						(add-vectors component geom-base:*raphael-translation*)))
					   (make-point (get-x point) 
						       (- (the length) (get-y point))))))
				 (the path-info-2d-scaled)))))
        
        (let ((line-index-pairs (when 2d-vertices (the-object object %line-vertex-indices%)))
              (curve-index-quadruples (when 2d-vertices (the-object object %curve-vertex-indices%)))
              (display-controls (or (geom-base::find-in-hash object *display-controls*)
                                    (the object display-controls)))
	      (name (base64-encode-safe 
		     (format nil "~s" (remove :root-object-object 
					      (the-object object  root-path)))))
              ;;(*read-default-float-format* 'single-float)
	      )
	  
	  (let ((line-path-string 
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
                                                     
					      ))) coords)))))))
		
		  (when (or path-info line-path-string curve-path-string)
		    (who:with-html-output (*stream*) 
		      (fmt "var ~a;~%" name)

		      (when (or line-path-string curve-path-string)
			(fmt "~a = paper.path('~a ~a');" 
			     name (or line-path-string "") (or curve-path-string "")))

		      (when path-info 
			(fmt "~&~a = paper.path('~{~a~^ ~}');~%"
			     name
			     (mapcar #'(lambda(component)
					 (if (keywordp component)
					     (ecase component 
					       (:move "M")
					       (:line "L")
					       (:curve "C"))
					     (format nil "~a ~a"
						     (number-round (get-x component) 4)
						     (number-round (get-y component) 4)))) path-info)))

		      (fmt "~a.attr({stroke: '~a'});" 
			   name (or (when (getf display-controls :color)
				      (lookup-color (getf display-controls :color) :format :hex))
				    (the-object object color-hex)))
		      (let ((fill (or (when (getf display-controls :fill-color)
					(lookup-color (getf display-controls :fill-color) :format :hex))
				      (the-object object fill-color-hex))))
			(when fill (fmt "~a.attr({fill: '~a'});" name fill)))
		      (let ((line-thickness (getf display-controls :line-thickness)))
			(when line-thickness 
			  (fmt "~&~a.attr('stroke-width','~a');" name (number-round line-thickness 4))))

		      (write-the (drag-controls :name name :display-controls display-controls))
		      ;;
		      ;; FLAG work these back in as generic output-functions.
		      ;;
		      #+nil
		      (onclick-string 
		       (when (ignore-errors (and (the viewport) (eql (the viewport digitation-mode) :select-object)))
			 (format nil "~a_curves.node.onclick = function (event) {~a};"
				 name
				 (the viewport (gdl-ajax-call :function-key :set-object-to-inspect!
							      :arguments (list object))))))
		      #+nil
		      (onmouseover-string 
		       (format nil 
			       " ~a_curves.node.onmouseover = function (){~a_curves.attr({'stroke-width': '~a'});};" 
			       name name 
			       (floor (to-single-float (* (or line-thickness 1) 3)))))

		      #+nil
		      (onmouseout-string
		       (format nil " ~a_curves.node.onmouseout = function (){~a_curves.attr({'stroke-width': '~a'});};" 
			       name name (to-single-float (or line-thickness 1))))
                      
                      )))))))))




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
  ((cad-output ())

   #+nil
   (cad-output
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
        
         (who:with-html-output (*stream*)
          (let ((*read-default-float-format* 'single-float)
		(start (aref 2d-vertices (first (first line-index-pairs))))
		(display-controls (or (geom-base::find-in-hash self *display-controls*)
				      (the display-controls))))
            (str 
             (format 
              nil
              "var ~a = paper.path('M ~a ~a ~{~a~^ ~}').attr({stroke: '~a'}).data('name','\\\"~a\\\"'); ~a~%;"
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
              
              (the color-hex)
	      
	      name

	      (when (getf (the display-controls) :fill-color)
		(let ((fill-color (lookup-color (getf (the display-controls) :fill-color) :format :hex)))
		  (format nil "~a.attr({fill: '~a'});" name fill-color)))))

	    (write-the (drag-controls :name name :display-controls display-controls))
	    )))))))



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


(define-lens (raphael vector)()
  :output-functions
  ((cad-output
    ()
    (with-format-slots (view)
      
      (let ((center (if view (the-object view (view-point self)) self))
	    (crosshair-length 3)
            (view-scale 1))
        
        (setq center (let ((point 
                            (add-vectors geom-base:*raphael-translation* (subseq center 0 2))))
                       (make-point (get-x point) (- (the-object view length) (get-y point)))))
        
        (let ((start-x (to-single-float (- (get-x center) (* crosshair-length view-scale))))
              (start-y (to-single-float (- (get-y center) (* crosshair-length view-scale))))
              (end-x (to-single-float (+ (get-x center) (* crosshair-length view-scale))))
              (end-y (to-single-float (+ (get-y center) (* crosshair-length view-scale))))
              (center-x (to-single-float (get-x center)))
              (center-y (to-single-float (get-y center)))
              (name "pt"))
          (with-cl-who ()
            (let ((*read-default-float-format* 'single-float))
              (str
               (format
                nil
                "var ~a_lines = paper.path('M ~a ~a L ~a ~a M ~a ~a L ~a ~a').attr({stroke: '#000'});~%"
                name 

                start-x 
                center-y
                end-x 
                center-y
                center-x 
                start-y
                center-x 
                end-y
                ))))))))))



(define-lens (raphael t)()
  :output-functions
  ((cad-output ())
   (drag-controls
    (&key name display-controls)
    (let ((drag-controls (ensure-list (getf display-controls :drag-controls))))
      (when drag-controls
	(with-cl-who ()
	  (str (format nil "~a.attr({cursor: 'pointer'}).data('name','\\\"~a\\\"');~%" 
		       name name))
	  (cond
	    ((or (member :drag drag-controls)
		 (member :drag-and-drop drag-controls))
	     (str (format nil "~&paper.set(~a).drag(move_cb,start,up);" name)))
	    ((or (member :drop drag-controls)
		 (member :drag-and-drop drag-controls))
	     (str (format nil "~&paper.set(~a).drag(move,start,up);" name))))))))))





#|

var start = function () {
  this.lastdx ? this.odx += this.lastdx : this.odx = 0;
  this.lastdy ? this.ody += this.lastdy : this.ody = 0;
  this.animate({"fill-opacity": 0.2}, 500);
},
move = function (dx, dy) {
  this.transform("T"+(dx+this.odx)+","+(dy+this.ody));
  this.lastdx = dx;
  this.lastdy = dy;
},
up = function () {
  this.animate({"fill-opacity": 1}, 500);
};

tri.drag(move, start, up);

<script>
            window.onload = function () {
                var R = Raphael(0, 0, "100%", "100%"),
                    r = R.circle(100, 100, 50).attr({fill: "hsb(0, 1, 1)", stroke: "none", opacity: .5}),
                    g = R.circle(210, 100, 50).attr({fill: "hsb(.3, 1, 1)", stroke: "none", opacity: .5}),
                    b = R.circle(320, 100, 50).attr({fill: "hsb(.6, 1, 1)", stroke: "none", opacity: .5}),
                    p = R.circle(430, 100, 50).attr({fill: "hsb(.8, 1, 1)", stroke: "none", opacity: .5});
                var start = function () {
                    this.ox = this.attr("cx");
                    this.oy = this.attr("cy");
                    this.animate({r: 70, opacity: .25}, 500, ">");
                },
                move = function (dx, dy) {
                    this.attr({cx: this.ox + dx, cy: this.oy + dy});
                },
                up = function () {
                    this.animate({r: 50, opacity: .5}, 500, ">");
                };
                R.set(r, g, b, p).drag(move, start, up);
            };
        </script>

|#
