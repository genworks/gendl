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


(glisp:define-constant +identity-lh-matrix+ (alignment :top (make-vector 0 0 1)
						       :left (make-vector 1 0 0)
						       :rear (make-vector 0 1 0)))

;;
;; FLAG -- consider name change to something indicating that this is geometric.
;;
(define-object base-object ()
  
  :documentation
  (:description "Base-Object is a superclass of most of GDL's geometric primitives. It 
provides an imaginary geometric reference box with a length, width, height, center, 
and orientation."
   
:examples 
   "<pre>

 (in-package :gdl-user)

 (define-object tower (base-object)
  
  :input-slots 
   ((number-of-blocks 50) (twist-per-block 1)
    (block-height 1) (block-width 5) (block-length 7))
  
  :objects
   ((blocks :type 'box
            :sequence (:size (the number-of-blocks))
            :center (translate (the center) 
                               :up (* (the-child index) 
                                      (the-child height)))
            :width (the block-width) 
            :height (the block-height) 
            :length (the block-length)
            :orientation (alignment 
                          :rear (if (the-child first?)
                                    (rotate-vector-d (the (face-normal-vector  :rear))
                                                     (the twist-per-block)
                                                     (the (face-normal-vector :top)))
                                    (rotate-vector-d (the-child previous 
                                                     (face-normal-vector :rear))
                                                     (the twist-per-block)
                                                     (the (face-normal-vector :top))))
                          :top (the (face-normal-vector :top))))))
;;
;;Test run
;;
#|
gdl-user(46): (setq self (make-object 'tower))
 #tower @ #x750666f2
gdl-user(47): (setq test-center (the (blocks 10) center))
#(0.0 0.0 10.0)
gdl-user(48): (the (blocks 10) (global-to-local test-center))
#(0.0 0.0 0.0)
gdl-user(49): (the (blocks 10) (local-to-global (the (blocks 10) 
                                                (global-to-local test-center))))
#(0.0 0.0 10.0)
gdl-user(50): 
gdl-user(50): (setq test-vertex (the (blocks 10) (vertex :top :right :rear)))
#(1.7862364748012536 3.9127176305081863 10.5)
gdl-user(51): (the (blocks 10) (global-to-local test-vertex))
#(2.500000000000001 3.500000000000001 0.5)
gdl-user(52): (the (blocks 10) (local-to-global (the (blocks 10) 
                                                (global-to-local test-vertex))))
#(1.786236474801254 3.9127176305081877 10.5)
gdl-user(53): 
|#
;;
;;
;;

</pre>" )
  
  :input-slots
  (("Lambda function of zero arguments, or nil. If non-nil, this
    function gets invoked when the user clicks the object in graphics
    front-ends which support this functionality, e.g. SVG/Raphael and X3DOM." 
    onclick-function nil)
   
   ("Pathname or string. Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil" image-file nil)
   

   ("Number. X-axis dimension of the reference box. Defaults to zero."
    width 0 :defaulting)
   ("Number. Y-axis dimension of the reference box. Defaults to zero."
    length 0 :defaulting)
   ("Number. Z-axis dimension of the reference box. Defaults to zero."
    height 0 :defaulting)

   
   ("3D Point. Indicates in global coordinates where the center of the reference 
box of this object should be located."
    center (make-point 0 0 0) :defaulting)
   
   ("3x3 Matrix of Double-Float Numbers. Indicates the absolute Rotation Matrix used to create 
the coordinate system of this object. This matrix is given in absolute terms (i.e. with 
respect to the root's orientation), and is generally created with the alignment function. 
It should be an <i>orthonormal</i> matrix, meaning each row is a vector with a magnitude 
of one (1.0).
"
    orientation nil :defaulting)
   
   (left-handed? (and (the orientation) (minusp (determinant (the orientation)))))


   (orientation* (if (the left-handed?)
		     (matrix:multiply-matrix (the orientation) +identity-lh-matrix+)
		     (the orientation)))


   (local-left-handed? (and (the local-orientation) (minusp (determinant (the local-orientation)))))
   
   (%orientation% nil)
   
   
   
   ("3x3 Orthonormal Matrix of Double-Float Numbers. This is synonymous with the <tt>orientation</tt>."
    obliqueness (the orientation))
   
   
   
   ("Plist. May contain keywords and values indicating display characteristics for
 this object. The following keywords are recognized currently:
 
<dl>
<dt>:color</dt>
<dd> color keyword from the *color-table* parameter, or an HTML-style hexidecimal
RGB string value, e.g. \"#FFFFFF\" for pure white. Defaults to :black.</dd>
<dt>:line-thickness</dt>
 <dd> an integer, defaulting to 1, indicating relative line thickness for wireframe
representations of this object.</dd>
<dt>:dash-pattern</dt>
<dd>(currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length,
 in pixels, of the dashes and blank spaces in a dashed line. The optional third number
 indicates how far into the line or curve to start the dash pattern.</dd>
</dl>"
    display-controls nil :defaulting)
   
   
   
   ("List of two 3D points. The left front bottom and right rear top corners, in global coordinates,
of the rectangular volume bounding this geometric object."
    local-box (let ((vertices (let (vertices) (dolist (lateral '(:left :right) vertices)
                                                (dolist (longitudinal '(:front :rear))
                                                  (dolist (vertical '(:bottom :top))
                                                    (push (the (vertex lateral longitudinal vertical)) vertices)))))))
                (let (xmin ymin zmin xmax ymax zmax)
                  (mapc #'(lambda(vertex)
                            (let ((x (get-x vertex)) (y (get-y vertex)) (z (get-z vertex)))
                              (when (or (null xmin) (< x xmin)) (setq xmin x))
                              (when (or (null xmax) (> x xmax)) (setq xmax x))
                              (when (or (null ymin) (< y ymin)) (setq ymin y))
                              (when (or (null ymax) (> y ymax)) (setq ymax y))
                              (when (or (null zmin) (< z zmin)) (setq zmin z))
                              (when (or (null zmax) (> z zmax)) (setq zmax z)))) vertices)
                  (list (make-point xmin ymin zmin) (make-point xmax ymax zmax)))))
   
   
   ("List of two 3D points. The left front bottom and right rear top corners, in global coordinates,
of the rectangular volume bounding the tree of geometric objects rooted at this object."
    bounding-box (bounding-box-from-list (the children) :local-objects (when (the use-local-box?) (list self))))

  
   (use-local-box? (not (and (zerop (the length))
                             (zerop (the width))
                             (zerop (the height)))))
   
   (viewpoints nil)
   
   (background-color (getf *colors-default* :background))
   
   )
  
  :trickle-down-slots
  (:length :height :width :center :orientation :display-controls :line-thickness :use-local-box?)

  
  :computed-slots
  (("3D Point. The center of this object, from the perspective of the parent. Starting
from the parent's center and using the parent's orientation, this is the relative center
of this object.
"
    local-center (the (convert-point-to-local (the center)))

    )

   ("3D Point. The center of this object, from the perspective of the parent. Starting
from the parent's center and using the parent's orientation, this is the relative center
of this object.
"
    local-center* (the (convert-point-to-local* (the center)))

    )


   
   (%dimension-mapping% (let ((dimensions (list :width :length :height)))
                          (swap-dimensions dimensions (the %orientation%))) :uncached)
   
   (%true-width% (first (the %dimension-mapping%)) :uncached)   
   (%true-length% (second (the %dimension-mapping%)) :uncached)
   (%true-height% (third (the %dimension-mapping%)) :uncached)

   (%local-width% (the (evaluate (the %true-width%))) :uncached)   
   (%local-length% (the (evaluate (the %true-length%))) :uncached)
   (%local-height% (the (evaluate (the %true-height%))) :uncached)    


   (vrml-center (the local-center))
    
   
   ("3x3 Matrix of Double-Float Numbers. Indicates the local Rotation Matrix used 
to create the coordinate system of this object. This is the ``local'' 
orientation with respect to the parent. Multiplying the parent's orientation 
with this matrix will always result in the absolute orientation for this part.

 :note An orientation of NIL indicates the 3x3 identity matrix.

"
    local-orientation (let ((parent-inverse
                             (when (and (the parent) 
                                        (the parent orientation))
                               (matrix:transpose-matrix (the parent orientation)))))
                        (if parent-inverse
                            (matrix:multiply-matrix (the orientation) parent-inverse)
                          (the orientation))))


   (local-orientation* (let ((parent-inverse
			      (when (and (the parent) 
					 (the parent orientation*))
				(matrix:transpose-matrix (the parent orientation*)))))
			 (if (and parent-inverse (the orientation*))
			     (matrix:multiply-matrix (the orientation*) parent-inverse)
			     (the orientation*))))


   

   ;;
   ;; FLAG -- replace face hash tables with struct since it's always
   ;; the same 6 fields.
   ;;
   (%face-ht% (let* ((orientation (the :orientation))
                     (ob-key
                      (when orientation
                        (list (aref orientation 0 0) (aref orientation 0 1)
                              (aref orientation 0 2) (aref orientation 1 0)
                              (aref orientation 1 1) (aref orientation 1 2)
                              (aref orientation 2 0) (aref orientation 2 1)
                              (aref orientation 2 2)))))
                (or (gethash ob-key *standard-face-hts*)
                    (let ((ht (make-hash-table :size 6)))
                      (dolist (face (list :right :rear :top))
                        (setf (gethash face ht)
                          (if orientation
                              (array-to-3d-vector (matrix:multiply-matrix
                                                   (ecase
                                                       face
                                                     (:right *nominal-x-array*)
                                                     (:rear *nominal-y-array*)
                                                     (:top *nominal-z-array*))
                                                   orientation))
                            (ecase face
                              (:right *nominal-x-vector*)
                              (:rear *nominal-y-vector*)
                              (:top *nominal-z-vector*)))))
                      (setf (gethash :left ht) (reverse-vector (gethash :right ht))
                            (gethash :front ht) (reverse-vector (gethash :rear ht))
                            (gethash :bottom ht) (reverse-vector (gethash :top ht)))
                      (glisp:w-o-interrupts
                         (setf (gethash ob-key *standard-face-hts*)
                           ht))))))

   ("Vector of three real numbers. The RBG color of this object specified in :display-controls. 
Defaults to the foreground color specified in <tt>*colors-default*</tt>. This message should not normally be overridden in user application code."
    color-decimal (multiple-value-bind (result found?) 
                      (lookup-color (getf (the display-controls) :color))
                    (when found? result)))
   
   (fill-color-decimal (multiple-value-bind (result found?) 
                           (lookup-color (getf (the display-controls) :fill-color) :ground :background)
                         (when found? result)))
   
   (line-thickness (getf (the display-controls) :line-thickness *line-thickness-default*))

   
   (dash-pattern (getf (the display-controls) :dash-pattern))
   
   (ui-display-list-objects nil)
   
   ;;
   ;; FLAG -- The following information is a catch-all until each
   ;; primitive computes more efficient info.
   ;;
   (%vertex-array% (append (flatten-lines (the %lines-to-draw%)) 
			   (flatten-curves (the %curves-to-draw%))))
   
   (%corners% nil)
   
   (%line-vertex-indices% (let ((count -1))
                            (mapcar #'(lambda(line)
                                        (declare (ignore line))
                                        (list (incf count) (incf count)))
                                    (the %lines-to-draw%))))
   
   (%curve-vertex-indices% (let ((count (or (lastcar (lastcar (the %line-vertex-indices%))) -1)))
                             (mapcar #'(lambda(curve)
                                         (declare (ignore curve))
                                         (list (incf count) (incf count) (incf count) (incf count)))
                                     (the %curves-to-draw%))))
   
   
   (%renderer-info% nil)
   


   (%internal-hidden-object-keywords% (append (list :local-bbox
                                                    :bounding-bbox
                                                    :beziers)
                                              (call-next-method)))
   
   )

  
  :hidden-objects
  (("GDL object of type Box. A box representing the local-box."
    local-bbox :type 'bounding-box
    :display-controls (list :color :grey :transparency 0.7)
    :corners (the local-box))
   
   ("GDL object of type Box. A box representing the bounding-box."
    bounding-bbox 
    :type 'bounding-box
    :display-controls (list :color :grey :transparency 0.7)
    :corners (the bounding-box))
   
   (beziers :type (if (the %curves-to-draw%) 'bezier-curve 'null-part)
            :sequence (:size (length (the %curves-to-draw%)))
            :control-points (nth (the-child index) (the %curves-to-draw%))))
  
  :functions 
  (
   (path-info ())

   ;;
   ;; FLAG -- check if we are at root orientation and center,
   ;; i.e. confirm transform is really necessary.
   ;;
   ("
3D-point. This function returns the point given in global coordinates, into relative local coordinates,
based on the orientation and center of the object to which the global-to-local message is sent.

:arguments (point \"3D-point. The point to be converted to local coordinates\")

:examples  Please see the examples area."
    
    global-to-local
    (point) 
    (let ((point (if (the orientation) (matrix*vector (the orientation) point) point))
          (center (if (the orientation) (matrix*vector (the orientation) (the center)) (the center))))
      (subtract-vectors point center)))


   (global-to-local*
    (point) 
    (let ((point (if (the orientation*) (matrix*vector (the orientation*) point) point))
          (center (if (the orientation*) (matrix*vector (the orientation*) (the center)) (the center))))
      (subtract-vectors point center)))


   
   ("3D-point. This function returns the point given in relative local coordinates, converted into global coordinates,
based on the orientation and center of the object to which the local-to-global message is sent.

:arguments (point \"3D-point. The local point to be converted to global coordinates\")

:examples  Please see the examples area.

"
    local-to-global
    (point)
    
    (translate (the center) :right (get-x point) :rear (get-y point) :up (get-z point)))

   (local-to-global*
    (point)
    
    (translate (the center) :right (get-x point) :rear (- (get-y point)) :up (get-z point)))

   
   (convert-point-to-local
    (point)
    (let ((point
           (let* ((parent-orientation
                   (when (the parent) (the parent orientation)))
                  (parent-center (when (the parent) (the parent center)))
                  (point-transformed
                   (if parent-orientation
                       (matrix*vector parent-orientation point)
                     point))
                  (parent-center-transformed
                   (when (the parent)
                     (if parent-orientation
                         (matrix*vector parent-orientation parent-center)
                       parent-center))))
             (if parent-center-transformed
                 (subtract-vectors point-transformed
                                   parent-center-transformed)
               point-transformed))))
      
      point))


   (convert-point-to-local*
    (point)
    (let ((point
           (let* ((parent-orientation
                   (when (the parent) (the parent orientation*)))
                  (parent-center (when (the parent) (the parent center)))
                  (point-transformed
                   (if parent-orientation
                       (matrix*vector parent-orientation point)
                     point))
                  (parent-center-transformed
                   (when (the parent)
                     (if parent-orientation
                         (matrix*vector parent-orientation parent-center)
                       parent-center))))
             (if parent-center-transformed
                 (subtract-vectors point-transformed
                                   parent-center-transformed)
               point-transformed))))
      
      point))

 
   (distance-along-axis
    (axis point &key (from (the center)))
    (let ((u-plane-1 (ecase axis
                       ((:front :rear :longitudinal :right :left :lateral) (the (face-normal-vector :top)))
                       ((:top :bottom :vertical) (the (face-normal-vector :rear)))))
          (u-plane-2 (ecase axis
                       ((:front :rear :longitudinal :top :bottom :vertical) (the (face-normal-vector :right)))
                       ((:right :left :lateral) (the (face-normal-vector :rear)))))
          (p-plane (the center)))
      (let ((start (inter-line-plane point u-plane-1 p-plane u-plane-1))
            (end   (inter-line-plane from u-plane-1 p-plane u-plane-1)))
        (let ((start (inter-line-plane start u-plane-2 p-plane u-plane-2))
              (end (inter-line-plane end u-plane-2 p-plane u-plane-2)))
          (3d-distance start end)))))

   
   (parallel-dimension
    (vector)
    (cond ((or (same-direction-vectors? vector (the (face-normal-vector :right)))
               (same-direction-vectors? vector (the (face-normal-vector :left)))) (the width))
          ((or (same-direction-vectors? vector (the (face-normal-vector :rear)))
               (same-direction-vectors? vector (the (face-normal-vector :front)))) (the length))
          ((or (same-direction-vectors? vector (the (face-normal-vector :top)))
               (same-direction-vectors? vector (the (face-normal-vector :bottom)))) (the height))))
   
   (parallel-coordinate
    (vector)
    (cond ((or (same-direction-vectors? vector (make-vector 1 0 0))
               (same-direction-vectors? vector (make-vector -1 0 0))) :x)
          ((or (same-direction-vectors? vector (make-vector 0 1 0))
               (same-direction-vectors? vector (make-vector 0 -1 0))) :y)
          ((or (same-direction-vectors? vector (make-vector 0 0 1))
               (same-direction-vectors? vector (make-vector 0 0 -1))) :z)))

   
   (coordinate-key
    (direction)
    (the (parallel-coordinate (the (face-normal-vector direction)))))
   
   (translate 
    (origin offsets)
    (if (null offsets) origin
      (the (translate (translate-along-vector origin 
                                              (the (:face-normal-vector (ecase (first offsets)
                                                                          ((:up :top) :top)
                                                                          ((:down :bottom) :bottom)
                                                                          (:left :left)
                                                                          (:right :right)
                                                                          (:front :front)
                                                                          ((:rear :back) :rear))))
                                              (second offsets))
                      (rest (rest offsets))))))
   
   (translate-center 
    (&rest specs)
    (ecase (length specs)
      (2 (translate (the center) (first specs) (second specs)))
      (4 (translate (the center) (first specs) (second specs) (third specs) (fourth specs)))
      (6 (translate (the center) (first specs) (second specs) (third specs) (fourth specs)
                    (fifth specs) (sixth specs)))))
                     
   (quantify-distance 
    (direction &key (reference-box (the quantify-box)))
    (let ((cardinality (the aggregate number-of-elements))
          (fill-distance (the-object reference-box (evaluate (ecase direction 
                                                               (:lateral :width)
                                                               (:longitudinal :length)
                                                               (:vertical :height)))))
          (local-distance (the (evaluate (ecase direction
                                           (:lateral :%local-width%)
                                           (:longitudinal :%local-length%)
                                           (:vertical :%local-height%))))))
      (let* ((span (- fill-distance local-distance))
             (spacing (/ span (1- cardinality)))
             (start (- (half span))))
        (+ start (* spacing (the index))))))
   
   
   
   ("List of four 3D points. Returns the vertices of the indicated face.

:arguments (direction \"Direction keyword, e.g. :top, :bottom etc. Indicates for which face to return the vertices.\")

"
    face-vertices 
    (direction)
    (ecase direction
      (:top (list (the (vertex :top :right :rear))
                  (the (vertex :top :right :front))
                  (the (vertex :top :left :front))
                  (the (vertex :top :left :rear))))
      (:bottom (list (the (vertex :bottom :right :rear))
                     (the (vertex :bottom :right :front))
                     (the (vertex :bottom :left :front))
                     (the (vertex :bottom :left :rear))))
      (:rear (list (the (vertex :rear :top :right))
                   (the (vertex :rear :top :left))
                   (the (vertex :rear :bottom :left))
                   (the (vertex :rear :bottom :right))))
      (:front (list (the (vertex :front :top :right))
                    (the (vertex :front :top :left))
                    (the (vertex :front :bottom :left))
                    (the (vertex :front :bottom :right))))
      (:right (list (the (vertex :right :top :rear))
                    (the (vertex :right :top :front))
                    (the (vertex :right :bottom :front))
                    (the (vertex :right :bottom :rear))))
      (:left (list (the (vertex :left :top :rear))
                   (the (vertex :left :top :front))
                   (the (vertex :left :bottom :front))
                   (the (vertex :left :bottom :rear))))))
   
   
   
   ("Boolean. Returns non-nil if the given point is in halfspace defined by the plane given a point and direction.

:arguments (point \"3D point. a point in the plane\"
            direction \"3D vector. The normal of the plane\")
"
    
    in-face? 
    (point direction)
    (when point
      (let ((vectors (mapcar #'(lambda(vertex)(subtract-vectors vertex point))
                             (the (face-vertices direction)))))
        (let ((angles (mapcar #'angle-between-vectors
                              vectors (append (rest vectors) (list (first vectors))))))
          (when (near-to? (apply #'+ angles) 2pi) point)))))
   
   
   
   ("List of 3D points. Returns the points of intersection between given line and the reference box of this object.

:arguments (p-line \"3D point. A point in the line\"
            u-line \"3D vector. The direction vector of the line\")

"
    line-intersection-points
    
    (p-line u-line)
    (let ((top (the (in-face? (inter-line-plane p-line u-line 
                                                (the (face-center :top))
                                                (the (face-normal-vector :top))) :top)))
          (bottom (the (in-face? (inter-line-plane p-line u-line 
                                                   (the (face-center :bottom))
                                                   (the (face-normal-vector :bottom))) :bottom)))
          (left (the (in-face? (inter-line-plane p-line u-line 
                                                 (the (face-center :left))
                                                 (the (face-normal-vector :left))) :left)))
          (right (the (in-face? (inter-line-plane p-line u-line 
                                                  (the (face-center :right))
                                                  (the (face-normal-vector :right))) :right)))
          (front (the (in-face? (inter-line-plane p-line u-line 
                                                  (the (face-center :front))
                                                  (the (face-normal-vector :front))) :front )))
          (rear (the (in-face? (inter-line-plane p-line u-line 
                                                 (the (face-center :rear))
                                                 (the (face-normal-vector :rear))) :rear))))
      (sort (remove nil (list top bottom left right front rear))
            #'(lambda(p1 p2) (< (3d-distance p-line p1)(3d-distance p-line p2))))))
   
   
   ;;
   ;; FLAG -- performance -- if %face-ht% always has unit vectors pass optional T arg into translate-along-vector.
   ;;
   ("3D Point. Returns the center of the requested face of this object's reference box.
:arguments (direction \"Keyword. One of the standard direction keywords: :right, :left, :rear, :front, :top, :bottom.\")"
    face-center
    (direction)
    (translate-along-vector (the :center) (gethash direction (the :%face-ht%))
                            (ecase direction
                              ((:top :bottom) (half (the :height)))
                              ((:left :right) (half (the :width)))
                              ((:front :rear) (half (the :length))))))

   ("3D Point. Returns the center of the requested edge of this object's reference box.
:arguments (direction-1 \"Keyword. One of the standard direction keywords: :right, :left, :rear, :front, :top, :bottom.\"
            direction-2 \"Keyword. A standard direction keyword orthogonal to direction-1.\")"
    edge-center
    (direction-1 direction-2)
    (translate-along-vector (the (:face-center direction-1))
                            (gethash direction-2 (the :%face-ht%))
                            (ecase direction-2
                              ((:top :bottom) (half (the :height)))
                              ((:left :right) (half (the :width)))
                              ((:front :rear) (half (the :length))))))

   ("3D Point. Returns the center of the requested vertex (corner) of this object's reference box.
:arguments (direction-1 \"Keyword. One of the standard direction keywords: :right, :left, :rear, :front, :top, :bottom.\"
            direction-2 \"Keyword. A standard direction keyword orthogonal to direction-1.\"
            direction-3 \"Keyword. A standard direction keyword orthogonal to direction-1 and direction-2.\")"
    vertex
    (direction-1 direction-2 direction-3)
    (translate-along-vector (the (:edge-center direction-1 direction-2))
                            (gethash direction-3 (the :%face-ht%))
                            (ecase direction-3
                              ((:top :bottom) (half (the :height)))
                              ((:left :right) (half (the :width)))
                              ((:front :rear) (half (the :length))))))

   ("3D Vector. Returns the vector pointing in the positive direction of the specified axis of this object's reference box.
:arguments (axis \"Keyword. One of the standard axis keywords: :lateral, :longitudinal, :vertical.\")"
    axis-vector
    (axis)
    (let ((orientation (the :orientation)))
      (ecase axis
        (:lateral
         (if orientation
             (array-to-3d-vector (multiply-matrices *nominal-x-array* orientation))
           *nominal-x-vector*))
        (:longitudinal
         (if orientation
             (array-to-3d-vector (multiply-matrices *nominal-y-array* orientation))
           *nominal-y-vector*))
        (:vertical
         (if orientation
             (array-to-3d-vector (multiply-matrices *nominal-z-array* orientation))
           *nominal-z-vector*)))))

   ("3D Vector. Returns the vector pointing from this object's reference box center to its requested face-center.
:arguments (axis \"Keyword. One of the standard direction keywords: :right, :left, :rear, :front, :top, :bottom.\")"
    face-normal-vector
    (direction)
    (gethash direction (the :%face-ht%)))

   (%lines-to-draw%
    nil
    nil)
   
   (%curves-to-draw%
    nil
    nil)
   
   (%arcs% nil nil)
   
   
   
   (fill-color-hex
    ()
    (let* ((display-controls (find-in-hash self *display-controls*))
           (fill-color-decimal (getf display-controls :fill-color-decimal))
           (fill-color-decimal (or fill-color-decimal (coerce (the fill-color-decimal)  'list))))
      
      (when (or fill-color-decimal (getf (the display-controls) :fill-color))
        (lookup-color (or fill-color-decimal (getf (the display-controls) :fill-color)) :format :hex))))
   
   (color-hex
    ()
    (let* ((display-controls (find-in-hash self *display-controls*))
           (color-decimal (getf display-controls :color-decimal))
           (color-decimal (or color-decimal (coerce (or (the color-decimal) 
                                                        (format-slot foreground-color)
                                                        (lookup-color (the color-decimal))) 'list))))
      (lookup-color color-decimal :format :hex)))))

(define-object base-geometry-object (base-object))

(define-object base-coordinate-system (base-object)
  :documentation (:description "This provides a default 3D Cartesian
   coordinate system. It mixes in base-object and does not extend it
   in any way, so as with base-object, it provides an imaginary
   geometric reference box with a length, width, height, center, and
   orientation."))


(define-object bounding-box (box)
  
  :input-slots (corners)
  
  :computed-slots
  (
   (orientation nil)
   (lower-left (first (the corners))) (upper-right (second (the corners)))
   (center (make-point (+ (get-x (the lower-left)) (half (the width)))
                       (+ (get-y (the lower-left)) (half (the length)))
                       (+ (get-z (the lower-left)) (half (the height)))))
   (width (- (get-x (the upper-right)) (get-x (the lower-left))))
   (length (- (get-y (the upper-right)) (get-y (the lower-left))))
   (height (- (get-z (the upper-right)) (get-z (the lower-left))))))
   

(defun find-in-hash (object hash)
  (when hash
    (or (gethash object hash)
        (let ((parent (the-object object parent)))
          (when parent (find-in-hash parent hash))))))


(defun swap-dimensions (dimensions symbolic)
  (cond ((null symbolic) dimensions)
        ((eql (first symbolic) :rotate)
         (let ((dimensions (case (second symbolic)
                             ((:right :left) (list (first dimensions)(third dimensions)(second dimensions)))
                             ((:rear :front) (list (third dimensions)(second dimensions)(first dimensions)))
                             ((:top :bottom) (list (second dimensions)(first dimensions)(third dimensions)))
                             (otherwise dimensions))))
           (swap-dimensions dimensions (rest (rest symbolic)))))
        (t (swap-dimensions dimensions (rest (rest symbolic))))))


(defun flatten-lines (lines)
  (let (result)
    (dolist (line lines (nreverse result))
      (push (first line) result)
      (push (second line) result))))

(defun flatten-curves (curves)
  (let (result)
    (dolist (curve curves (nreverse result))
      (push (first curve) result)
      (push (second curve) result)
      (push (third curve) result)
      (push (fourth curve) result))))
      
