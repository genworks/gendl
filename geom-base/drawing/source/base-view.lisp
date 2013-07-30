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

(defparameter *previous-scale* 1)

(defparameter *center-circ* nil)

;;
;; FLAG -- this will move back to internal codebase on the next build.
;;

(define-object base-drawing (base-object)
  
  :documentation (:description "Generic container object for displaying one or more scaled
transformed views of geometric or text-based entities. The contained views are generally 
of type <tt>base-view</tt>. In a GWL application-mixin, you can include one 
object of this type in the ui-display-list-leaves.

For the PDF output-format, you can also use the cad-output output-function to write the 
drawing as a PDF document. 

Since base-drawing is inherently a 2D object, only the top view (getf *standard-views* :top) 
makes sense for viewing it."
                  :examples "<pre>
 (in-package :gdl-user)

 (define-object cylinder-sample (cylinder)
   :computed-slots
   ((display-controls (list :color :pink-spicy))
    (length 10)
    (radius 3)
    (number-of-sections 25)))

 (define-object base-drawing-sample (base-drawing)
  
   :objects
   ((main-view :type 'base-view
               :projection-vector (getf *standard-views* :trimetric)
               :object-roots (list (the surf)))
 
    (surf :type 'cylinder-sample
          :hidden? t))) 

 (generate-sample-drawing :objects (make-object 'base-drawing-sample))                

 </pre>")
  
  :input-slots
  (
   
   ;;(touched-geometry nil)
   
   (width (the page-width))
   (length (the page-length))
   (height 0)

   ("Number in PDF Points. Left-to-right width of the paper being represented by this drawing. 
The default is (* 8.5 72) points, or 8.5 inches, corresponding to US standard letter-size paper."
    page-width (* 8.5 72))
   
   ("Number in PDF Points. Front-to-back (or top-to-bottom) length of the paper being represented 
by this drawing. The default is (* 11 72) points, or 11 inches, corresponding to US standard 
letter-size paper."
    page-length (* 11 72))
   
   (%corners% (list (the (vertex :top :left :rear))
                    (the (vertex :top :right :rear))
                    (the (vertex :top :right :front))
                    (the (vertex :top :left :front)))))

  
  :computed-slots 
  (
   
   (%vertex-array% (make-array 4 :initial-contents (the %corners%)))
   
   
   (views (remove-if-not #'(lambda(child) (typep child 'base-view)) (append (the children) (the hidden-children))))
   (objects (apply #'append (mapsend (the views) :objects)))
   (object-roots (apply #'append (mapsend (the views) :object-roots)))
   (annotation-objects (apply #'append (mapsend (the views) :annotation-objects)))
   
   (user-center nil)))
  


(define-object base-view (base-object)
  :documentation (:description "Generic container object for displaying a scaled transformed view of geometric or 
text-based objects. <tt>Base-view</tt> can be used by itself or as a child of a <tt>base-drawing</tt>

In a GWL application-mixin, you can include an object of this type in the ui-display-list-leaves.

For the PDF output-format, you can also use the cad-output output-function to write the 
view as a PDF document. 

Since base-view is inherently a 2D object, only the top view (getf *standard-views* :top) 
makes sense for viewing it."
                  :examples  "<pre>
                 
 (in-package :gdl-user)

 (define-object box-with-two-viewed-drawing (base-object)
  
   :objects
   ((drawing :type 'two-viewed-drawing
             :objects (list (the box) (the length-dim)))
    
    (length-dim :type 'horizontal-dimension
                :hidden? t
                :start-point (the box (vertex :rear :top :left))
                :end-point (the box (vertex :rear :top :right)))
   
    (box :type 'box
         :hidden? t
         :length 5 :width 10 :height 15)))

 (define-object two-viewed-drawing (base-drawing)
   
   :input-slots (objects)
   
   :objects
  
   ((main-view :type 'base-view
               :projection-vector (getf *standard-views* :trimetric)
               :length (half (the length))
               :center (translate (the center)
                                  :rear (half (the-child length)))
               :objects (the objects))
   
    (top-view :type 'base-view
              :projection-vector (getf *standard-views* :top)
              :length (* 0.30 (the length))
              :objects (the objects))))

   (generate-sample-drawing :objects 
    (the-object (make-object 'box-with-two-viewed-drawing) drawing top-view))
 
 </pre>")

  :input-slots 
  (
   ("3D-point. Center of the view box. Specify this or corner, not both. 
NOTE that the center is no longer defaulting (so that it can self-compute properly when corner 
is specified), so it is necessary to explicitly give either start or center for base-view."
    center (if *center-circ*
               (the parent center)
             (translate (the corner) 
                        :right (half (the width))
                        :front (half (the length)))))
   
   ("3D-point. Top left (i.e. rear left from top view) of the view box. Specify this or center, not both."
    corner (let ((*center-circ* t))
            (translate (the center) 
                       :left (half (the width))
                       :rear (half (the length)) )))

   
   
   
   ;;(touched-geometry nil)
   
   ("List of GDL objects. The leaves from each of these objects will be displayed in each view by default."
    object-roots nil) 
   
   ("List of GDL objects. These objects will be displayed in each view by default."
    objects nil)
   
   ("List of GDL objects. These objects are immune from view scaling and transform computations and so can freely refer 
to the view-scale, view-center, and other view information for self-scaling views. Defaults to NIL."
    immune-objects nil)
   
   ("List of GDL objects. These objects will be displayed in each view by default, with no scaling or transform (i.e. they are in Drawing space."
    annotation-objects nil)
   
   ("Number. Ratio of drawing scale (in points) to model scale for this view. Defaults to being auto-computed."
    view-scale (getf (the view-contents-data) :view-scale)) 
   
   ("3D Point in Model space. Point relative to each object's center to use as center of the view."
    ;;
    ;; FLAG -- convert this to model coordinates, then back to view-coords in the output-functions.
    ;;
    view-center (getf (the view-contents-data) :view-center))
   
   
   ("3D Unitized Vector. Direction of camera pointing to model (the object-roots and/or the objects) to create 
this view. The view is automatically ``twisted''about this vector to result in ``up'' being as close as 
possible to the Z vector, unless this vector is parallel to the Z vector in which case ``up'' is taken 
to be the Y (rear) vector. This vector is normally taken from the <tt>*standard-views*</tt> built-in GDL 
parameter. Defaults to <tt>(getf *standard-views* :top)</tt>, which is the vector [0, 0, 1]."
    projection-vector (getf *standard-views* :top))
   
   
   ("Number in Drawing scale (e.g. points). Amount of margin on left and right 
of page when <tt>view-scale</tt> is to be computed automatically. Defaults to 25."
    left-margin (* (the width) 25/612)) 
   
   ("Number in Drawing scale (e.g. points). Amount of margin on front and rear 
of page when <tt>view-scale</tt> is to be computed automatically. Defaults to 25."
    front-margin (* (the length) 25/792))
   
   
   ("Boolean. Determines whether a rectangular border box is drawn around the view, 
with the view's length and width. Defaults to nil."
    border-box? nil)
   
   (user-scale nil :settable :defaulting)
   
   ("3D Vector. For a top view, this vector specifies the direction that the rear of
the box should be facing. Defaults to <tt>*nominal-y-vector*</tt>." 
    snap-to *nominal-y-vector*))

  
  :computed-slots
  (
   (view-scale-total (cond ((the user-scale) (* (the view-scale) (the user-scale)))
                           (t (the view-scale))))
   
   
   (immune-objects-hash (let ((ht (glisp:make-sans-value-hash-table :size (length (the immune-objects)))))
                          (dolist (object (the immune-objects) ht) 
                            (setf (gethash object ht) t))))
    
   ;;
   ;; FLAG -- watch out for memory issues with vertex-array-2d captured here.
   ;;
   ;;

   (2d-boxes (remove nil (mapsend (the object-caches) :2d-bounding-box)))
   
   (view-contents-data
    (let* ((2d-boxes (the 2d-boxes)))
      (if 2d-boxes 
      
          (let (xmin xmax ymin ymax)
            (dolist (2d-box 2d-boxes)
              
              (let ((new-xmin (getf 2d-box :xmin))
                    (new-ymin (getf 2d-box :ymin))
                    (new-xmax (getf 2d-box :xmax))
                    (new-ymax (getf 2d-box :ymax)))
                (when (or (null xmin) (< new-xmin xmin)) (setq xmin new-xmin))
                (when (or (null ymin) (< new-ymin ymin)) (setq ymin new-ymin))
                (when (or (null xmax) (> new-xmax xmax)) (setq xmax new-xmax))
                (when (or (null ymax) (> new-ymax ymax)) (setq ymax new-ymax))))
                
            (let ((x-spread (- xmax xmin)) (y-spread (- ymax ymin))
                  (printable-x (- (the width) (twice (the left-margin))))
                  (printable-y (- (the length) (twice (the front-margin)))))
              
              
              (let ((scale (cond ((and (zerop x-spread) (zerop y-spread)) 
                                  (warn "No Information to Determine the view-scale -- defaulting to 1.") 1)
                                 ((zerop x-spread) (/ printable-y y-spread))
                                 ((zerop y-spread) (/ printable-x x-spread))
                                 (t (min (/ printable-x x-spread) (/ printable-y y-spread)))))
                    (center (keyed-transform*vector (the view-transform-inverse)
                                            (make-point (+ xmin (half x-spread)) 
                                                        (+ ymin (half y-spread)) 0)
                                            
                                            :snap-to (the snap-to)
                                            )))
                (list :view-scale scale
                      :view-center center))))
                    
        (progn 
          (warn "No objects of substance in the view. Cannot auto-compute a scale or center")
          (list :view-scale 1 :view-center  
                (keyed-transform*vector (the view-transform-inverse)
                                        (make-point 0 0 0)
                                        :snap-to (the snap-to)))))))
        
        
            
   
   (view-transform (view-transform (the projection-vector) :snap-to (the snap-to)))
   
   (view-transform-inverse (transpose-keyed-transform (the view-transform)
                                                      :snap-to (the snap-to)))
   
   (leaf-objects-from-roots 
    (remove-if 
     #'(lambda(obj) 
         (or (typep obj 'null-part)
             ;;(typep obj 'outline-specialization-mixin)
             ))
     (let ((leaves
            (apply #'append (mapcar #'(lambda(object) 
					(if (typep object 'gdl::gdl-basis)
					    (the-object object leaves)
					    (list object)))
                                    (the object-roots)))))
       (append leaves
               (apply #'append
                      (mapcar #'(lambda(leaf)
                                  (when (typep leaf 'outline-specialization-mixin)
                                    (the-object leaf outline-leaves))) leaves))))))
   
   ;;(annotation-array (coerce (the annotation-objects) 'vector))
   
   (annotation-array 
    (remove-duplicates
     (coerce 
      (append (the annotation-objects) 
              (remove-if #'(lambda(obj) (or (typep obj 'null-part)
                                            (typep obj 'outline-specialization-mixin)))
                         (apply #'append
                                (mapcar #'(lambda(object)
                                            (when (typep object 'outline-specialization-mixin)
                                              (the-object object outline-leaves)))
                                        (the annotation-objects)))))
      'vector)))   

   
   (object-array 
    (remove-duplicates
     (coerce 
      (append (the objects) 
	      (remove-if #'(lambda(obj) (or (typep obj 'null-part)
					    (typep obj 'outline-specialization-mixin)
					    ))
			 (apply #'append
                                                    
				(mapcar #'(lambda(object)
					    (when (typep object 'outline-specialization-mixin)
					      (the-object object outline-leaves)))
					(the objects))))
	      (the leaf-objects-from-roots)) 'vector))))
  
  :hidden-objects
  ((border-box :type 'box)
   
   (annotation-caches :type 'view-object-cache
                      :sequence (:size (length (the annotation-array)))
                      :projection-vector (getf *standard-views* :top)
                      :view-transform (view-transform :top) 
                      ;;:view-transform (view-transform :top :snap-to (the snap-to))
                      ;;:snap-to (the snap-to)
                      ;;:snap-to-y? (coincident-point? (the snap-to) *nominal-y-vector*)
		      :snap-to-y? nil
                      :scale? nil
                      :object (svref (the annotation-array) (the-child index)))
   
   
   (object-caches :type 'view-object-cache
                  :sequence (:size (length (the object-array)))
                  :view-scale (the view-scale-total)
                  :pass-down (projection-vector view-transform ;;touched-geometry
                                                snap-to)
                  :snap-to-y? (coincident-point? (the snap-to) *nominal-y-vector*)
                  :immune? (gethash (the-child object) (the immune-objects-hash))
                  :object (svref (the object-array) (the-child index))))
   
  
  :functions
  (("3D Point. Takes point in view coordinates and returns corresponding point in model coordinates.

:arguments (view-point \"3D Point. Point in view coordinates.\")"
    model-point 
    (view-point)
    (let ((local-point (scalar*vector (/ (the view-scale-total)) 
                                       view-point)))
      (add-vectors (the view-center) (keyed-transform*vector 
                                      (the view-transform-inverse) 
                                      local-point
                                      :snap-to (the snap-to)))))
   
   
   ("3D Point. Takes point in model coordinates and returns corresponding point in view coordinates.

:arguments (model-point \"3D Point. Point in model coordinates.\")"
    view-point
    (model-point)
    (subtract-vectors
     (project-to-plane model-point (the view-scale-total) (the projection-vector) 
                       (the view-transform) :snap-to (the snap-to))
     (project-to-plane (the view-center) (the view-scale-total) 
                       (the projection-vector) (the view-transform)
                       :snap-to (the snap-to))))))



(define-object view-object-cache ()
  :input-slots
  (object projection-vector view-transform view-scale 
   (scale? t) ;;(touched-geometry nil) 
   (immune? nil)
   (snap-to *nominal-y-vector*)
   snap-to-y? 
   )

  
  :computed-slots
  ((vertex-array-2d-scaled (if (the scale?) 
                               (let ((scale (the view-scale)))
                                 (map 'vector #'(lambda(point)(scalar*vector scale point))
                                      (the vertex-array-2d)))
                             (the vertex-array-2d)))
   
   
   (arcs-array-2d-scaled (if (the scale?) 
                             (let ((scale (the view-scale)))
                               (map 'list #'(lambda(point)(scalar*vector scale point))
                                       (the arcs-array-2d)))
                           (the arcs-array-2d)))

   (path-info-2d-scaled (when (the path-info-2d)
			  (if (the scale?) 
			      (let ((scale (the view-scale)))
				(mapcar #'(lambda(point)
					    (if (keywordp point) point (scalar*vector scale point)))
					(the path-info-2d)))
			      (the path-info-2d))))
   
   ;;
   ;; FLAG -- fold code from this and next message into a method function
   ;;
   (arcs-array-2d 
    (let ((raw-points (mapsend (the object %arcs%) :center)))
      (map 'vector 
        (if (and (keywordp (the view-transform)) (the snap-to-y?))
            #'(lambda(point)
                
                ;;
                ;; FLAG this case basically repeats code from (defun keyed-transform*vector ...)
                ;;
                (case (the view-transform)
                  (:top (make-vector (get-x point) (get-y point)))
                  (:bottom (make-vector (get-x point) (- (get-y point))))
                  (:rear (make-vector (- (get-x point)) (get-z point)))
                  (:front (make-vector (get-x point) (get-z point)))
                  (:right (make-vector (get-y point) (get-z point)))
                  (:left  (make-vector (- (get-y point)) (get-z point)))))
          #'(lambda(point) 
	      (subseq 
	       (project-to-plane point 1 (the projection-vector) (the view-transform)
				 :snap-to (the snap-to)) 0 2)))
        raw-points)))
   
   
   (vertex-array-2d 
    (when (typep (the object) 'gdl::gdl-basis)
      (let ((raw-points (or (the object %vertex-array%) (the object %corners%))))
	(map 'vector 
	     #'(lambda(point)
		 ;;
		 ;; FLAG this case basically repeats code from (defun keyed-transform*vector ...)
		 ;;
		 (if (and (keywordp (the view-transform)) (the snap-to-y?))
		     (case (the view-transform)
		       (:top (subseq point 0 2)) ;;(make-vector (get-x point) (get-y point)))
		       (:bottom (make-vector (get-x point) (- (get-y point))))
		       (:rear (make-vector (- (get-x point)) (get-z point)))
		       (:front (make-vector (get-x point) (get-z point)))
		       (:right (make-vector (get-y point) (get-z point)))
		       (:left  (make-vector (- (get-y point)) (get-z point))))
		     (subseq
		      (project-to-plane point 1 (the projection-vector) (the view-transform)
				       :snap-to (the snap-to)) 0 2)))
	     raw-points))))


   (path-info-2d 
    (when (and (typep (the object) 'gdl::gdl-basis) (the object path-info))
      (let ((raw-points (the object path-info)))
	(mapcar
	 #'(lambda(point)
	     ;;
	     ;; FLAG this case basically repeats code from (defun keyed-transform*vector ...)
	     ;;
	     (if (keywordp point) point
		 (if (and (keywordp (the view-transform)) (the snap-to-y?))
		     (case (the view-transform)
		       (:top (subseq point 0 2)) ;;(make-vector (get-x point) (get-y point)))
		       (:bottom (make-vector (get-x point) (- (get-y point))))
		       (:rear (make-vector (- (get-x point)) (get-z point)))
		       (:front (make-vector (get-x point) (get-z point)))
		       (:right (make-vector (get-y point) (get-z point)))
		       (:left  (make-vector (- (get-y point)) (get-z point))))
		     (subseq
		      (project-to-plane point 1 (the projection-vector) (the view-transform)
					:snap-to (the snap-to)) 0 2)))) raw-points))))

   
   (2d-bounding-box
    (unless (or (not (typep (the object) 'gdl::gdl-basis))
		(the immune?))
      (let* ((vertex-array-2d (the vertex-array-2d))
             (xs (map 'list #'get-x vertex-array-2d))
             (ys (map 'list #'get-y vertex-array-2d)))
      
        (when (and xs ys)
          (list :xmin (reduce #'min xs) :xmax (reduce #'max xs)
                :ymin (reduce #'min ys) :ymax (reduce #'max ys)))))))
  
  :functions
  ())




(defun project-to-plane (model-point view-scale projection-vector view-transform
                         &key (snap-to *nominal-y-vector*))
  (let ((drop-length (dot-vectors model-point projection-vector)))
    (scalar*vector view-scale
                   (keyed-transform*vector 
                    view-transform
                    (translate-along-vector model-point projection-vector
                                            (- drop-length))
                    :snap-to snap-to))))



(defun keyed-transform*vector (transform vector &key (snap-to *nominal-y-vector*))
  (if (coincident-point? snap-to *nominal-y-vector*)
      (case transform 
        (:top (make-vector (get-x vector) (get-y vector) 0))
        (:bottom (make-vector (get-x vector) (- (get-y vector)) 0))
        (:rear (make-vector (- (get-x vector)) (get-z vector) 0))
        (:front (make-vector (get-x vector) (get-z vector) 0))
        (:right (make-vector (get-y vector) (get-z vector) 0))
        (:left  (make-vector (- (get-y vector)) (get-z vector) 0))
        (otherwise (matrix*vector transform vector)))
    (matrix*vector transform vector)))



(defun view-transform (projection-vector &key (snap-to *nominal-y-vector*))

  (if (and (eql projection-vector :top)
           (not (coincident-point? snap-to *nominal-y-vector*)))

      (let ((projection-vector (make-vector 0 0 1)))
        (or (gethash (list projection-vector snap-to) *view-transform-inverses-hash*)
            (setf (gethash (list projection-vector snap-to) *view-transform-inverses-hash*)
                  (projection-vector-to-transform projection-vector
                                                  :snap-to snap-to))))
  
      (cond
        ((and (keywordp projection-vector)
              (coincident-point? snap-to *nominal-y-vector*)) projection-vector)
        ((and (coincident-point? projection-vector *nominal-z-vector*)
              (coincident-point? snap-to *nominal-y-vector*)) :top)
        ((coincident-point? projection-vector *nominal-y-vector*) :rear)
        ((coincident-point? projection-vector *nominal-x-vector*) :right)
        ((coincident-point? projection-vector *nominal-z-vector-r*) :bottom)
        ((coincident-point? projection-vector *nominal-y-vector-r*) :front)
        ((coincident-point? projection-vector *nominal-x-vector-r*) :left)
        (t

         (or (gethash (list projection-vector snap-to) *view-transform-inverses-hash*)
             (setf (gethash (list projection-vector snap-to) *view-transform-inverses-hash*)
            
                   (projection-vector-to-transform projection-vector
                                                   :snap-to snap-to)))))))


(defun transpose-keyed-transform (transform &key (snap-to *nominal-y-vector*))
  (if (keywordp transform) 
      ;;
      ;; FLAG -- vet the special case of :bottom.
      ;;
      (case transform (:bottom :top)
            (otherwise
             (let ((projection-vector (getf *standard-views* transform)))
               (matrix:transpose-matrix 
                (projection-vector-to-transform projection-vector :snap-to snap-to)))))
    (matrix:transpose-matrix transform)))



(defun projection-vector-to-transform (projection-vector 
                                       &key (snap-to *nominal-y-vector*))
  (alignment 
   :top projection-vector
   :rear (if (or (same-direction-vectors? projection-vector 
                                          *nominal-z-vector*)
                 (same-direction-vectors? projection-vector 
                                          *nominal-z-vector-r*))
             snap-to *nominal-z-vector*)))






