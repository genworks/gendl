;;
;; Copyright 2002-2011 Genworks International 
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

(in-package :geom-base)

(defparameter *fixed-scale?* nil)


(eval-when (compile load eval) (export '*fixed-scale?*))

;;
;; FLAG -- the basic PDF views from here will go back to internal codebase.
;;

(defmacro with-translated-state ((format center) &body body)
  (ecase format
    (:pdf `(pdf:with-saved-state (pdf:translate (get-x ,center) (get-y ,center)) ,@body))
    (:dxf `(let ((*dxf-translation* (add-vectors (subseq ,center 0 2)  *dxf-translation*)))
	     ,@body))
    (:raphael `(let ((*raphael-translation* (add-vectors (subseq ,center 0 2)  *raphael-translation*))) ,@body))
    (:svg `(let ((*svg-translation* (add-vectors (subseq ,center 0 2)  *svg-translation*))) ,@body))

    ))
             

(define-lens (pdf base-drawing)()
  :output-functions
  ((cad-output
    ()
    (with-format-slots (view)
      (let ((view-center (if (the user-center) 
                             (scalar*vector (the user-scale) (the user-center)) 
                           (make-point 0 0 0))))

        (pdf:with-saved-state
            (when view (unless *fixed-scale?* (pdf:scale (the-object view view-scale-total) 
                                                         (the-object view view-scale-total))))

          (mapc #'(lambda(child-view) 
                    (let ((width (the-object child-view width))
                          (length (the-object child-view length))
                          (center (the-object child-view center)))
                      
                      (pdf:with-saved-state
                          (let ((rotation (angle-between-vectors-d 
                                           +rear-vector+ 
                                           (the-object child-view (face-normal-vector :rear)) 
                                           +top-vector+)))
                            (pdf:rotate rotation)
                          
                            (pdf:basic-rect (+ (get-x center) (- (half width)))
                                            (+ (get-y center) (- (half length)))
                                            width length)
                            (pdf:clip-path) 
                            (if (the-object child-view border-box?) (pdf:stroke) 
                              (pdf:end-path-no-op))
                            (with-translated-state (:pdf  (make-point (- (get-x view-center)) (- (get-y view-center))))
                              (write-the-object child-view cad-output)))))) (the views))))))))


(define-lens (pdf base-view)()
  :output-functions
  ((cad-output
    ()
    
    (set-format-slot view self)
    
    (let ((center (the center)) 

          (view-center (scalar*vector (the view-scale-total)
                                      (keyed-transform*vector (the view-transform)
                                                              (the view-center)))))
      (with-translated-state (:pdf center)
        
        
        ;;
        ;; FLAG - look into capturing this translate in the
        ;; vertex-array-2d-scaled in view-object-cache.  so it will
        ;; become unecessary here.
        ;;
        (with-translated-state (:pdf (make-vector (- (get-x view-center)) (- (get-y view-center))))
          
          (dolist (cache (list-elements (the object-caches))) (write-the-object cache lines-and-curves)))

        (dolist (cache (list-elements (the object-caches))) (write-the-object cache object cad-output))
        
        (with-format-slots (view)
          (set-format-slot view nil)
          (dolist (cache (list-elements (the annotation-caches)))
            (write-the-object cache object cad-output) 
            (write-the-object cache lines-and-curves))
          (set-format-slot view view))
      ))
    
    (set-format-slot view nil))))
    





(define-lens (dxf base-drawing)()
  :output-functions
  ((cad-output ()
	       (let ((*dxf-entity-id* 107))
		 (mapc #'(lambda(view) (write-the-object view cad-output)) (the views))))))



(define-lens (dxf base-view)()
  :output-functions
  ((cad-output
    ()
    (let ((*dxf-entity-id* (or *dxf-entity-id* 107)))
      (set-format-slot view self)
      (let ((center (the center))(view-center (scalar*vector (the view-scale-total)
							     (keyed-transform*vector (the view-transform)
										     (the view-center)))))
	(with-translated-state (:dxf center)
	  (with-format-slots (view)
	    (set-format-slot view nil)
	    (dolist (cache (list-elements (the annotation-caches)))
	      (write-the-object cache object cad-output) (write-the-object cache lines-and-curves))
	    (set-format-slot view view))
	  (dolist (cache (list-elements (the object-caches))) 
          
	    (write-the-object cache object cad-output))
	  ;;
	  ;; FLAG - look into capturing this translate in the
	  ;; vertex-array-2d-scaled in view-object-cache.  so it will
	  ;; become unecessary here.
	  ;;
	  (with-translated-state (:dxf (make-vector (- (get-x view-center)) 
						    (- (get-y view-center))))
	    (dolist (cache (list-elements (the object-caches))) 
	      (write-the-object cache lines-and-curves)))))))))


;;
;; FLAG - update this lens to be like Raphael so the fill will work for closed shapes.
;;

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
				    (pdf:stroke)))))))))



;;
;; FLAG - update this lens to be like Raphael so the fill will work for closed shapes.
;;

(define-lens (dxf view-object-cache)()
  :output-functions
  ((lines-and-curves
    ()
    
    (let ((2d-vertices (the vertex-array-2d-scaled))) 
      (let ((line-index-pairs (the object %line-vertex-indices%)))
        (let ((2d-vertices (map 'vector #'(lambda(vertex) (add-vectors (subseq vertex 0 2) *dxf-translation*)) 
                                2d-vertices)))
          
          (mapc #'(lambda(line-index-pair)
                    (destructuring-bind (start-index end-index) line-index-pair
                      (let ((start (svref 2d-vertices start-index)) (end   (svref 2d-vertices end-index)))
                        (format *stream* "  0~%LINE~% 5~%~a~% 100~%AcDbEntity~% 100~%AcDbLine~% 10~%~3,16f~% 20~%~3,16f~% 11~%~3,16f~% 21~%~3,16f~%"
				(incf *dxf-entity-id*)
                                (get-x start) (get-y start) (get-x end) (get-y end))))
                    
                    (write-the object line-thickness-setting)
                    (write-the object dash-pattern-setting)
                    (write-the object rgb-stroke-setting))
                    
                line-index-pairs)))

      (mapc #'(lambda(curve)
		(let ((control-points (mapcar #'(lambda(point) (add-vectors *dxf-translation* point)) (getf curve :control-points)))
		      (knots (getf curve :knot-vector))
		      (degree (getf curve :degree))
		      (weights (getf curve :weights))
		      (top-vector (make-vector 0 0 1) ;;(the (face-normal-vector :top))
			))
		  (declare (ignore top-vector weights)) ;; FLAG -- work these in. 
		  (format *stream*

			  " 0
SPLINE
 5
~a
 100
AcDbEntity
 100
AcDbSpline
 210
0.0
 220
0.0
 230
1.0
 70
8
 71
~a
 72
~a
 73
~a
 74
0
 42
0.0000001
 43
0.0000001
~{ 40~%~,7f~^~%~}
~{~a~%~}"
			  (incf *dxf-entity-id*)
			  degree
			  (length knots)
			  (length control-points)
			  knots
			  (mapcar #'(lambda(point)
				      (format nil " 10~%~,7f~% 20~%~,7f~% 30~%0.0"
					      (get-x point) (get-y point)))
				  control-points))


		  (write-the object line-thickness-setting)
		  (write-the object dash-pattern-setting)
		  (write-the object rgb-stroke-setting)))
	    
	    
	    (the curves-2d-scaled))
      
      (mapc #'(lambda(arc 2d-arc-center)
                
                (let ((angle-correction 
                       (angle-between-vectors-d +rear-vector+
                                                (the object (face-normal-vector :rear))
                                                +top-vector+)))
                  (let ((start-angle (+ (radians-to-degrees 
                                         (the-object arc start-angle-normalized)) angle-correction))
                        (end-angle (+ (radians-to-degrees 
                                       (the-object arc end-angle-normalized)) angle-correction))
                        (radius (* (the-object arc radius) (the view-scale)))

                        (center (add-vectors *dxf-translation* 2d-arc-center))
                          
                        (top-vector (the-object arc (face-normal-vector :top))))
                      
                    (if (>= (- end-angle start-angle) 360)
                        (format *stream* "  0~%CIRCLE~% 10~%~3,16f~% 20~%~3,16f~% 30~%~3,16f~% 40~%~3,15f
  210~%~3,15f~% 220~%~3,15f~% 230~%~3,15f~%" 
                                (get-x center) (get-y center) 0 radius 
                                (get-x top-vector) (get-y top-vector) (get-z top-vector))
                      (format *stream* "  0~%ARC~% 10~%~3,16f~% 20~%~3,16f~% 30~%~3,16f~% 40~%~3,15f
 50~%~3,15f~% 51~%~3,15f~% 210~%~3,15f~% 220~%~3,15f~% 230~%~3,15f~%" 
                              (get-x center) (get-y center) 0 radius start-angle end-angle
                              (get-x top-vector) (get-y top-vector) (get-z top-vector)))))
                  
                  
                (write-the object line-thickness-setting)
                (write-the object dash-pattern-setting)
                (write-the object rgb-stroke-setting))

            (the object %arcs%) (the arcs-array-2d-scaled))))))






(defparameter *command* nil)


(define-lens (raster base-drawing)()
  :output-functions
  ((cad-output
    ()
    (let ((temp-dir (glisp:temporary-folder))
          (temp-name (format nil "~a-~a" (glisp:get-pid) (gensym))))

      (let ((temp-pdf (make-pathname :directory (pathname-directory temp-dir)
                                     :device (pathname-device temp-dir)
                                     :name temp-name
                                     :type "pdf"))
            
            (temp-png (make-pathname :directory (pathname-directory temp-dir)
                                     :device (pathname-device temp-dir)
                                     :name temp-name
                                     :type "png")))
        (with-format-slots (page-width page-length background-color foreground-color)
          (with-format (pdf temp-pdf :page-width page-width :page-length page-length 
                            :background-color background-color :foreground-color foreground-color) (write-the cad-output)))
        
	
	;;
	;; FLAG -- try this list form of the command on all
	;; implementations, get it working that way, and remove this
	;; compiler conditional.
	;;
	;;#+nil
	(let ((command 
	       (if (or (null *gs-path*)
		       (and (or (stringp *gs-path*)
				(pathnamep *gs-path*))
			    (not (probe-file *gs-path*))))
		   (error "Gnu Ghostscript executable not found. It is set to ~s.~%" *gs-path*)
		   (list (format nil "~a" *gs-path*)
			 "-q"
			 (format nil "-sDEVICE=~a" (write-the gs-device))
			 (format nil "-sOutputFile=~a" temp-png)
			 (format nil "-dTextAlphaBits=~a" *gs-text-alpha-bits*)
			 (format nil "-dGraphicsAlphaBits=~a" *gs-graphics-alpha-bits*)
			 "-dSAFER"
			 "-dBATCH"
			 "-dNOPAUSE"
			 (namestring temp-pdf)))))
	  (multiple-value-bind (output error result)
	      (uiop:run-program command 
				:ignore-error-status t
				:error-output :string
				:output :string)
	    (declare (ignore output))
	    (unless (zerop result)
	      (error "Ghostscript threw result code ~s with error: ~a~%" result error))))

	#+nil
        (let ((command 
               (format nil "\"~a\" -q -sDEVICE=~a \"-sOutputFile=~a\" -dTextAlphaBits=~a -dGraphicsAlphaBits=~a -dSAFER -dBATCH -dNOPAUSE  \"~a\""
                               *gs-path* "png16m"  temp-png *gs-text-alpha-bits* 
                               *gs-graphics-alpha-bits* temp-pdf)))
          (glisp:run-gs command))
        
        (when *stream*
          (with-open-file  (image-stream temp-png :element-type 'unsigned-byte)
            (do ((val (read-byte image-stream nil nil)
                      (read-byte image-stream nil nil)))
                ((null val))

              (write-byte val *stream*))))
                                         
        (delete-file temp-pdf)

        (delete-file temp-png))))))





(defparameter *current-vrml-requests* nil)

(define-lens (png base-drawing)()
  :output-functions
  ((gs-device () "png16m")))

(define-lens (jpeg base-drawing)()
  :output-functions
  ((gs-device () "jpeg")))


