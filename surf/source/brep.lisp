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


(in-package :surf)


(define-object brep (geometry-kernel-object-mixin ifs-output-mixin base-object)
  
  :documentation (:description "A general superclass for all boundary representation geometric entities. 
This currently follows the smlib topology model, with breps containing regions, regions containing shells, 
and shells containing faces and edges. Shells which completely enclose a volume are considered 
to make up a solid brep."
                  
                  :author "Dave Cooper, Genworks"
                  )
  
  :input-slots 
  (
   (hide-faces? t)

   ("GDL Brep object. Defaults to nil.
Specify this if you want this brep to be a clone of an existing brep. (note -
this uses a shared underlying brep object, it does not make a copy)"
    built-from nil)
   
   (native-pointer nil)
   
   (%native-brep% (or (the native-pointer)
                      (if (the built-from)
                          (the built-from %native-brep%)
                        (error "%native-brep% has to be passed in to brep or overridden in the subclass"))))
   
   
   ("Plist of keyword symbols and numbers. This controls tessellation for this brep.
The keys are as follows:
<ul>
          <li><tt>:min-number-of-segments</tt></li>
          <li><tt>:max-3d-edge-factor</tt></li>
          <li><tt>:min-parametric-ratio</tt></li>
          <li><tt>:max-chord-height</tt></li>
          <li><tt>:max-angle-degrees</tt></li>
          <li><tt>:min-3d-edge</tt></li>
          <li><tt>:min-edge-ratio-uv</tt></li>
          <li><tt>:max-aspect-ratio</tt></li>
</ul>

and the defaults come from the following parameters:

<pre>
   (list 
       :min-number-of-segments *tess-min-number-of-segments*
       :max-3d-edge-factor *tess-max-3d-edge-factor*
       :min-parametric-ratio *tess-min-parametric-ratio*
       :max-chord-height *tess-max-chord-height*
       :max-angle-degrees *tess-max-angle-degrees*
       :min-3d-edge *tess-min-3d-edge*
       :min-edge-ratio-uv *tess-min-edge-ratio-uv*
       :max-aspect-ratio *tess-max-aspect-ratio*)

</pre>

"
    tessellation-parameters
    
    (list :min-number-of-segments *tess-min-number-of-segments*
          :max-3d-edge-factor *tess-max-3d-edge-factor*
          :min-parametric-ratio *tess-min-parametric-ratio*
          :max-chord-height *tess-max-chord-height*
          :max-angle-degrees *tess-max-angle-degrees*
          :min-3d-edge *tess-min-3d-edge*
          :min-edge-ratio-uv *tess-min-edge-ratio-uv*
          :max-aspect-ratio *tess-max-aspect-ratio*))
   
      
   ("Integer. Used for tessellations. Default comes from (the tessellation-parameters)."
    min-number-of-segments (getf (the tessellation-parameters) :min-number-of-segments) 
    :settable)
   
   ("Number. Used for tessellations. Default comes from (the tessellation-parameters)."
    max-3d-edge-factor (getf (the tessellation-parameters) :max-3d-edge-factor)
    :settable)
   
   ("Number. Used for tessellations. Computed from (the max-extent) and (the max-3d-edge-factor).

WARNING: Modify this value at your peril. Small values can cause intractable tessellations. 
         It is better to tweak max-3d-edge-factor to a small value like 0.1, as this will
         be taken relative to the max-extent of the brep."
    max-3d-edge (* (the max-extent) (the max-3d-edge-factor)) :settable)
   
   ("Number. Used for tessellations. Default comes from (the tessellation-parameters)."
    min-parametric-ratio (getf (the tessellation-parameters) :min-parametric-ratio)
    :settable)
   
   ("Number. Used for tessellations. Default comes from (the tessellation-parameters)."
    max-chord-height (getf (the tessellation-parameters) :max-chord-height)
    :settable)
   
   ("Number. Used for tessellations. Default comes from (the tessellation-parameters)."
    max-angle-degrees (getf (the tessellation-parameters) :max-angle-degrees)
    :settable)
   
   ("Number. Used for tessellations. Default comes from (the tessellation-parameters)."
    min-3d-edge (getf (the tessellation-parameters) :min-3d-edge)
    :settable)
   
   ("Number. Used for tessellations. Default comes from (the tessellation-parameters)."
    min-edge-ratio-uv (getf (the tessellation-parameters) :min-edge-ratio-uv)
    :settable)
   
   ("Number. Used for tessellations. Default comes from (the tessellation-parameters)."
    max-aspect-ratio (getf (the tessellation-parameters) :max-aspect-ratio)
    :settable)

   ("Boolean. Smooth results for poly-brep? Defaults to t."
    poly-brep-smooth-results? t
    :settable)
   
   ("Number. The density per unit volume of the brep. Defaults to 1" 
    density 1
    :settable)
   
   ("Boolean. Determines whether the isoparametric curves of each face of the brep are used for wireframe display.
The default is T."
    display-iso-curves-wireframe? (not *brep-wireframe-tessellation?*)
    :settable)
   

   ("Plist with keys :n-u and :n-v. The number of isoparametric curves to be displayed in each direction. 
This value comes from the value of :isos on the display-controls if that exists, and defaults 
to *isos-default* otherwise."
    isos (getf (the display-controls) :isos *brep-isos-default*) 
    :settable)


   ("Boolean. Determines whether the tessellation grid-lines of the brep are used for wireframe display.
The default is NIL."
    display-tessellation-lines-wireframe? *brep-wireframe-tessellation?*
    :settable)

   


   ("Number. Overall tolerance for the created brep solid. Defaults to nil.
Note that a value of nil indicates for SMLib to use value of 1.0e-05 of the
longest diagonal length of the brep."
    brep-tolerance *brep-tolerance-default*)
   
   ("List of Color Keywords. These indicate the colors 
for the breps produced by (the face-breps). If the 
number of face-breps exceeds the length of this list, 
the colors will be repeated in order. 
Defaults to a list with keys: 
<ul>
 <li> :green </li>
 <li> :red </li>
 <li> :blue </li>
 <li> :purple-dark </li>
 <li> :violet </li>
 <li> :cyan. </li>
</ul>"
    face-brep-colors 
    (list  :red :orange :gold-old :green :blue :purple-dark :violet))
   
   
   )

  

  :computed-slots
  (
   (ifs-array-and-indices (destructuring-bind (&key vertex-counts vertex-indices 3d-points
						    &allow-other-keys)
			      (the tessellation)

			    (let ((coords-array (make-array (length 3d-points) 
						     :initial-contents 3d-points))
				  (indices-array (make-array (length vertex-indices)
							     :initial-contents vertex-indices)))
			      (list :array coords-array
				    :indices
				    (let ((position 0))
				      (mapcar #'(lambda(count)
						  (prog1
						      (let (result)
							(dotimes (n count (nreverse result))
							  (push (aref indices-array (+ position n)) result)))
						    (setq position (+ position count))))
					      vertex-counts))))))
			  
				

   (ifs-array (getf (the ifs-array-and-indices) :array))

   (ifs-indices (getf (the ifs-array-and-indices) :indices))


   (bounding-box (brep-calculate-tight-bounding-box *geometry-kernel* (the %native-brep%)))
   
   (max-extent (apply #'3d-distance (the bounding-box)))


   ("Number. This is the actual tolerance stored in the SMLib object."
    adaptive-tolerance (brep-get-tolerance *geometry-kernel* (the %native-brep%)))
   
   ;;(adaptive-tolerance (* *3d-approximation-tolerance-default* (the max-extent)))
   

   ("FLAG -- for debug testing only 

   Plist with :min-x :max-x :min-y :max-y :min-z :max-z. Returns the
extreme points of the bounding-box of the brep in each direction. This
will be updated to return points on the brep itself, not necessarily
the bounding box."
    
    min-max-x-y-z (let ((box (the bounding-box)))
		    
		    ;;
		    ;; FLAG -- evaluate this just to see if it's the prob
		    ;;
		    
                    (list :min-x (first box)
                          :max-x (second box)
                          :min-y (first box)
                          :max-y (second box)
                          :min-z (first box)
                          :max-z (second box))))

   
   #+nil
   ("Plist with :min-x :max-x :min-y :max-y :min-z :max-z. Returns the extreme points of the 
brep in each direction (on the brep itself, not necessarily on the bounding box)."
    
    
    min-max-x-y-z (let ((points (list-elements (the face-points) (the-element center))))
                    (list :min-x (least #'get-x points)
                          :max-x (most #'get-x points)
                          :min-y (least #'get-y points)
                          :max-y (most #'get-y points)
                          :min-z (least #'get-z points)
                          :max-z (most #'get-z points))))

   
   (%triangle-data% (let ((result (the (tessellation :in-memory? t))))
                      (mapcan #'(lambda(key val)
                                  (list key (if (consp val) (coerce val 'vector) val)))
                              (plist-keys result) (plist-values result))))
   
   ("List of Plists, one for each face, format still being determined. 
Contains triangle and connectivity data for the tessellation
of this brep. Exact supported format will be documented here when ready." 
    triangle-data (mapsend (the face-triangles) :triangle-data))

   (one-face-data (list :vertex-counts (apply #'append 
                                               (mapcar #'(lambda(tris) (coerce (the-object tris vertex-counts)
                                                                               'list))
                                                       (list-elements (the face-triangles))))
                         :vertex-indices (apply #'append 
                                                (mapcar #'(lambda(tris) 
                                                            (coerce (the-object tris full-vertex-indices)
                                                                    'list))
                                                        (list-elements (the face-triangles))))
                         :3d-points (apply #'append 
                                           (mapcar #'(lambda(tris) (coerce (the-object tris 3d-points)
                                                                           'list))
                                                   (list-elements (the face-triangles))))

                         :uv-points (apply #'append 
                                           (mapcar #'(lambda(tris) (coerce (the-object tris uv-points)
                                                                           'list))
                                                   (list-elements (the face-triangles))))

                         :surface-normals (apply #'append 
                                           (mapcar #'(lambda(tris) (coerce (the-object tris surface-normals)
                                                                           'list))
                                                   (list-elements (the face-triangles))))))

   
   ;;
   ;; FLAG -- make this work for in-memory!!
   ;;
   (%tess-data% (the (tessellation :in-memory? nil)))

   ;;
   ;; FLAG -- copied back from base-object -- should be a cleaner way to do this.
   ;;
   (%vertex-array% (append (flatten-lines (the %lines-to-draw%)) 
                           (flatten-curves (the %curves-to-draw%))))
   

   (%lines-to-draw% (if (or (the display-tessellation-lines-wireframe?)
                            (member (the isos) '(:tess :tessellation)))
                        (the tessellation-lines)
                        (if (the isos)
                            (append-elements (the faces) (the-element %lines-to-draw%))
                            (append-elements (the edges) (the-element %lines-to-draw%)))))
   
   ;;
   ;; FLAG -- conditionalize all this based on :display-controls and :isos.
   ;;
   (%curves-to-draw% (cond ((null (the isos))
                            (when *debug?* (format *trace-output*
                                                   "Processing edges for ~s.~%" 
                                                   (cons 'the (reverse (the root-path)))))
                            (append-elements (the edges) (the-element %curves-to-draw%)))
                           ((member (the isos) '(:tess :tessellation))
                            nil)
                           (t (append-elements (the faces) (the-element %curves-to-draw%)))))

   (tessellation-lines 
    (let ((faces (the %tess-data%)))
      (mapcan 
       #'(lambda(face)
           (destructuring-bind (vertex-counts vertex-indices 3d-points surface-normals) 
               face
             (declare (ignore vertex-counts surface-normals))
             (let ((3d-points (coerce 3d-points 'vector))
                   (index-triplets (make-triplets vertex-indices)))
               (mapcan 
                #'(lambda(triplet)
                    (list (list (apply-make-point (svref 3d-points (first triplet)))
                                (apply-make-point (svref 3d-points (second triplet))))
                          (list (apply-make-point (svref 3d-points (second triplet)))
                                (apply-make-point (svref 3d-points (third triplet))))
                          (list (apply-make-point (svref 3d-points (third triplet)))
                                (apply-make-point (svref 3d-points (first triplet)))))) 
                       index-triplets)))) faces)))


   ;;
   ;; FLAG -- review this!
   ;;
   (local-box (the bounding-box))
   
   (%native-faces% (get-faces-from-brep *geometry-kernel* (the %native-brep%)))
   
   (%native-edges% (get-edges-from-brep *geometry-kernel* (the %native-brep%)))
   
   (%native-regions% (get-regions-from-brep *geometry-kernel* (the %native-brep%)))
   
   (%native-shells% (get-shells-from-brep *geometry-kernel* (the %native-brep%)))
   
   (%native-vertices% (get-vertices-from-brep *geometry-kernel* (the %native-brep%)))
   
   (faces-ht (let ((ht (make-hash-table :size (the faces number-of-elements))))
               (dolist (face (list-elements (the faces)) ht)
                 (setf (gethash (the-object face %native-face%) ht) face))))
   
   )

  
  :objects
  ()
  
  
  :hidden-objects
  (
   (bounding-box-solid :type 'box-solid
                       :center (the bounding-bbox center)
                       :width (the bounding-bbox width)
                       :length (the bounding-bbox length)
                       :height (the bounding-bbox height))
   
   (face-points :type 'point
                :sequence (:size (the bounding-box-solid face-breps number-of-elements))
                :center (the (brep-solve (the bounding-box-solid (face-breps (the-child index))) :minimize)))
   
   
   ("Sequence of GDL Brep objects. One brep for each face in the parent brep, containing only that face."
    face-breps 
    :type 'facial-brep
    :sequence (:size (the faces number-of-elements))
    :display-controls 
    (append (list :color (nth (mod (the-child index)
                                   (length (the face-brep-colors)))
                              (the face-brep-colors)))
            (the display-controls))
    :%source-native-brep% (the %native-brep%)
    :%native-face% (the (faces (the-child index)) %native-face%))

   
   ("Sequence of GDL Face Objects. The Faces contained within this brep."
    faces :type 'face
    ;;:hidden? (the hide-faces?)
    :sequence (:size (length (the %native-faces%)))
    :brep self
    :layer (the-child index)
    :pseudo-inputs (layer)
    :%native-brep% (the %native-brep%)
    :%native-face% (nth (the-child index) (the %native-faces%)))
   
   (face-triangles :type 'face-triangles
                   :sequence (:size (length (getf (the %triangle-data%) :vertex-index-counts)))
                   :triangle-data-in (the %triangle-data%))
   
   ("Sequence of GDL Region Objects. The Regions contained within this brep."
    regions :type 'region
    :sequence (:size (length (the %native-regions%)))
    :brep self
    :%native-brep% (the %native-brep%)
    :%native-region% (nth (the-child index) (the %native-regions%)))
   
   ("Sequence of GDL Shell Objects. The Shells contained within this brep."
    shells :type 'brep-shell
    :sequence (:size (length (the %native-shells%)))
    :brep self
    :%native-brep% (the %native-brep%)
    :%native-shell% (nth (the-child index) (the %native-shells%)))
   
   ("Sequence of GDL Edge Objects. The Edges contained within this brep."
    edges :type 'edge
    :sequence (:size (length (the %native-edges%)))
    :brep self
    :%native-brep% (the %native-brep%)
    :%native-edge% (nth (the-child index) (the %native-edges%)))
   
   ("The composed edges contained within this brep, this is valid just if the brep does not contain holes"
    composed-edges :type 'composed-curve
                   :curves (list-elements (the edges )))
                            
   ("Sequence of GDL Edge Objects. The Edges contained within this brep aranged clockwise or anticlockwise, 
this is valid just if the brep does not contain holes"
    edges-sequence :type 'subdivided-curve
    :continuity-type :c0
    :curve-in (the composed-edges))

  
   ("Sequence of GDL Vertex Objects. The Vertices contained within this brep."
    vertices :type 'vertex
    :sequence (:size (length (the %native-vertices%)))
    :brep self
    :%native-brep% (the %native-brep%)
    :%native-vertex% (nth (the-child index) (the %native-vertices%)))
   
   ("Polygonal Brep Object. This brep represented as a Polygonal Brep"
    poly-brep :type 'poly-brep
    :brep self
    :max-chord-height (the max-chord-height)
    :max-angle-degrees (the max-angle-degrees)
    :max-3d-edge (the max-3d-edge)
    :max-aspect-ratio (the max-aspect-ratio)
    :smooth-results? (the poly-brep-smooth-results?)))

  
  :functions
  (
   ("Value nil or t. This function performs an intersection between this brep and another brep.
The function returns a NIL value if no intersection is found and T if a intersection is found.
:arguments (other-brep \"GDL Brep. The brep with which to intersect.\")
:&key ((tolerance (the adaptive-tolerance)) \"Number, Controls how precisely the intersection is computed.\"
       (angle-tolerance (radians-to-degrees *angle-tolerance-radians-default*))  \"Number, in radians. The angle tolerance for intersection.\")"   

    brep-intersect? 
    (other-brep &key 
		(tolerance (the adaptive-tolerance))
		(angle-tolerance (radians-to-degrees *angle-tolerance-radians-default*)))
    (or (box-intersection (the bounding-box) (the-object other-brep bounding-box))
	(and (the (bbox-intersect? other-brep))
	     (brep-intersect? *geometry-kernel* (the %native-brep%) (the-object other-brep %native-brep%)
			      :tolerance tolerance :angle-tolerance angle-tolerance))))
   
   (bbox-intersect? 
    (brep)
    (block func
      (let ((my-min (first (the bounding-box)))
            (my-max (second (the bounding-box)))
            (his-min (first (the-object brep bounding-box)))
            (his-max (second (the-object brep bounding-box))))
        (when (not (and (< (get-x his-min)
                           (get-x my-max))
                        (> (get-x his-max)
                           (get-x my-min))))
          (return-from func nil))
        (when (not (and (< (get-y his-min)
                           (get-y my-max))
                        (> (get-y his-max)
                           (get-y my-min))))
          (return-from func nil))
        (when (not (and (< (get-z his-min)
                           (get-z my-max))
                        (> (get-z his-max)
                           (get-z my-min))))
          (return-from func nil))
        (return-from func t))))

   
   
   (brep-solve
    (brep operation)
    (global-brep-brep-solve *geometry-kernel* self 
                            :other-brep brep :operation-type operation))
   
   
   (assert-valid 
    ()
    (brep-assert-valid *geometry-kernel* (the %native-brep%)))
   
   (write-stl-file
    (file-name &key (format :ascii))
    (the poly-brep (write-stl-file file-name :format format)))
   
   
   ("Plist or list. Contains tessellation data for the brep
based on the values of the keyword args. This is used to produce
the value of (the triangle-data).

:&key ((min-number-of-segments (the min-number-of-segments)) \"\"
       (max-3d-edge (the max-3d-edge)) \"\"
       (min-parametric-ratio (the min-parametric-ratio)) \"\"
       (max-chord-height (the max-chord-height)) \"\"
       (max-angle-degrees (the max-angle-degrees)) \"\"
       (min-3d-edge (the min-3d-edge)) \"\"
       (min-edge-ratio-uv (the min-edge-ratio-uv)) \"\"
       (max-aspect-ratio (the max-aspect-ratio)) \"\"
       (in-memory? t) \"\")"
    
    tessellation 
    (&key (min-number-of-segments (the min-number-of-segments))
          (max-3d-edge (the max-3d-edge))
          (min-parametric-ratio (the min-parametric-ratio))
          (max-chord-height (the max-chord-height))
          (max-angle-degrees (the max-angle-degrees))
          (min-3d-edge (the min-3d-edge))
          (min-edge-ratio-uv (the min-edge-ratio-uv))
          (max-aspect-ratio (the max-aspect-ratio))
          (in-memory? t))
    (let ((raw-data
           (get-triangles *geometry-kernel*  
                          (the %native-brep%)
                          :min-number-of-segments  min-number-of-segments
                          :max-3d-edge max-3d-edge
                          :min-parametric-ratio min-parametric-ratio
                          :max-chord-height max-chord-height
                          :max-angle-degrees max-angle-degrees
                          :min-3d-edge min-3d-edge
                          :min-edge-ratio-uv min-edge-ratio-uv
                          :max-aspect-ratio max-aspect-ratio
                          :in-memory? in-memory?)))
      
      raw-data))
      
      
      
      
   
   ("Multiple values: Number, Number, Number, and Plist. Returns the area, volume, mass, 
and moments for the brep. The moments are labeled as: :area-static-moments, :area-moments-of-inertia, 
:area-products-of-inertia, :area-second-moment-about-coordinate-axii, :volume-static-moments, 
:volume-moments-of-inertia, :volume-products-of-inertia, and  :volume-second-moment-about-coordinate-axii.

:&key ((tolerance (the adaptive-tolerance)) \"Controls how precisely the properties are computed\")"    precise-properties 
    (&key (tolerance (the adaptive-tolerance)))
    (brep-compute-precise-properties *geometry-kernel* self tolerance))

   ("Plist with keys: :area, :volume, :mass, :moments. Returns the area, volume, mass, 
and moments for the brep. The moments are labeled as: :area-static-moments, :area-moments-of-inertia, 
:area-products-of-inertia, :area-second-moment-about-coordinate-axii, :volume-static-moments, 
:volume-moments-of-inertia, :volume-products-of-inertia, and  :volume-second-moment-about-coordinate-axii.

:&key ((tolerance (the adaptive-tolerance)) \"Controls how precisely the properties are computed\")"    
    precise-properties-plist
    (&key (tolerance (the adaptive-tolerance)))
    (multiple-value-bind (area volume mass moments)
	(brep-compute-precise-properties *geometry-kernel* self tolerance)
      (list :area area :volume volume :mass mass :moments moments)))

   ("Plist with keys: :area, :volume, :barycenter. Returns the approximate area, volume, 
and barycenter (center of mass) for the brep. These are computed with tessellation techniques, 
which may be less precise than the analytic techniques used in precise-properties, but should
be faster to compute and exhibit more stability.
    
:&key ((edge-tess-tolerance (the adaptive-tolerance)) \"Controls how precisely the properties are computed with respect to edge tessellation\")
      ((face-tess-tolerance (the adaptive-tolerance)) \"Controls how precisely the properties are computed with respect to face tessellation\")"

    properties 
    (&key (edge-tess-tolerance (the adaptive-tolerance))
          (face-tess-tolerance (the adaptive-tolerance)))
    (brep-compute-properties *geometry-kernel* self :edge-tess-tolerance edge-tess-tolerance 
			     :face-tess-tolerance face-tess-tolerance))
   
   ("Number. Area covered by the faces of the brep.
:&key ((tolerance (the adaptive-tolerance)) \"Controls how precisely the properties are computed\")"
    area
    (&key (tolerance (the adaptive-tolerance)))
    (multiple-value-bind (area volume mass moments) (the (precise-properties :tolerance tolerance))
      (declare (ignore volume mass moments)) area))
   
   ("Number. Volume enclosed by the brep.
:&key ((tolerance (the adaptive-tolerance)) \"Controls how precisely the properties are computed\")"
    volume
    (&key (tolerance (the adaptive-tolerance)))
    (multiple-value-bind (area volume mass moments) (the (precise-properties :tolerance tolerance))
      (declare (ignore area mass moments)) volume))
   
   ("Number. Mass represented by the brep, according to the density.
:&key ((tolerance (the adaptive-tolerance)) \"Controls how precisely the properties are computed\")"
    mass
    (&key (tolerance (the adaptive-tolerance)))
    (multiple-value-bind (area volume mass moments) (the (precise-properties :tolerance tolerance))
      (declare (ignore area volume moments)) (* (the density) mass)))

   
   ("Boolean. Returns t or nil depending on whether the point given is within the boundary of the brep (including faces).

:arguments (point \"Point to check for\")"
    in? (point) (in? *geometry-kernel* self point))
    
   
   ("Plist. Returns the moments of the brep. The plist contains keys: :area-static-moments, :area-moments-of-inertia, 
:area-products-of-inertia, :area-second-moment-about-coordinate-axii, :volume-static-moments, 
:volume-moments-of-inertia, :volume-products-of-inertia, and  :volume-second-moment-about-coordinate-axii.

:&key ((tolerance (the adaptive-tolerance)) \"Controls how precisely the properties are computed\")"
    moments 
    (&key (tolerance (the adaptive-tolerance)))
    (multiple-value-bind (area volume mass moments)
        (the (precise-properties :tolerance tolerance))
      (declare (ignore area volume mass)) moments))
   
   ("3D Vector (i.e. 3D Point). Returns the Area Static Moments of the brep.
:&key ((tolerance (the adaptive-tolerance)) \"Controls how precisely the properties are computed\")"
    area-static-moments
    (&key (tolerance (the adaptive-tolerance)))
    (getf (the (moments :tolerance tolerance)) :area-static-moments))
   

   ("3D Vector (i.e. 3D Point). Returns the Area Moments of Inertia of the brep.
:&key ((tolerance (the adaptive-tolerance)) \"Controls how precisely the properties are computed\")"
    area-moments-of-inertia
    (&key (tolerance (the adaptive-tolerance)))
    (getf (the (moments :tolerance tolerance)) :area-moments-of-inertia))
   
   ("3D Vector (i.e. 3D Point). Returns the Area Products of Inertia of the brep.
:&key ((tolerance (the adaptive-tolerance)) \"Controls how precisely the properties are computed\")"
    area-products-of-inertia
    (&key (tolerance (the adaptive-tolerance)))
    (getf (the (moments :tolerance tolerance)) :area-products-of-inertia))
   
   ("3D Vector (i.e. 3D Point). Returns the Area Second Moment About Coordinate Axii of the brep.
:&key ((tolerance (the adaptive-tolerance)) \"Controls how precisely the properties are computed\")"
    area-second-moment-about-coordinate-axii
    (&key (tolerance (the adaptive-tolerance)))
    (getf (the (moments :tolerance tolerance)) :area-second-moment-about-coordinate-axii))
   
   ("3D Vector (i.e. 3D Point). Returns the Volume Static Moments of the brep.
:&key ((tolerance (the adaptive-tolerance)) \"Controls how precisely the properties are computed\")"
    volume-static-moments
    (&key (tolerance (the adaptive-tolerance)))
    (getf (the (moments :tolerance tolerance)) :volume-static-moments))
   
   ("3D Vector (i.e. 3D Point). Returns the Volume Moments of Inertia of the brep.
:&key ((tolerance (the adaptive-tolerance)) \"Controls how precisely the properties are computed\")"
    volume-moments-of-inertia
    (&key (tolerance (the adaptive-tolerance)))
    (getf (the (moments :tolerance tolerance)) :volume-moments-of-inertia))
   
   ("3D Vector (i.e. 3D Point). Returns the Volume Products of Inertia of the brep.
:&key ((tolerance (the adaptive-tolerance)) \"Controls how precisely the properties are computed\")"
    volume-products-of-inertia
    (&key (tolerance (the adaptive-tolerance)))
    (getf (the (moments :tolerance tolerance)) :volume-products-of-inertia))
   
   ("3D Vector (i.e. 3D Point). Returns the Volume Second Moment about Coordinate Axii of the brep.
:&key ((tolerance (the adaptive-tolerance)) \"Controls how precisely the properties are computed\")"
    volume-second-moment-about-coordinate-axii
    (&key (tolerance (the adaptive-tolerance)))
    (getf (the (moments :tolerance tolerance)) :volume-second-moment-about-coordinate-axii))
   
   
   ("3D Point. Center of gravity of the mass of the brep.
:&key ((tolerance (the adaptive-tolerance)) \"Controls how precisely the properties are computed\")"
    center-of-gravity
    (&key (tolerance (the adaptive-tolerance)))
    (scalar*vector (/ (the (volume :tolerance tolerance)))
                   (getf (the (moments :tolerance tolerance)) :volume-static-moments)))))



;;
;; FLAG -- need glisp:split-regexp definition for these next two. 
;;
#+nil
(defun read-stl-file (&key (file-name "/tmp/try.stl"))
  (let (result)
    (with-open-file (in file-name)
      (read-line in)
      (do ((facet-normal (read-line in) (read-line in)))
          ((search "endsolid" facet-normal) (nreverse result))
        
        (let ((outer-loop (read-line in))
              (v1 (read-line in))
              (v2 (read-line in))
              (v3 (read-line in))
              (endloop (read-line in))
              (endfacet (read-line in)))
          (declare (ignore outer-loop endloop endfacet))
          (push (list :facet-normal (point-from-line facet-normal 2)
                      :v1 (point-from-line v1)
                      :v2 (point-from-line v2)
                      :v3 (point-from-line v3)) result))))))

#+nil
(defun point-from-line (line &optional (words 1))
  (let ((list (subseq (glisp:split-regexp "\\s" (string-trim (list #\space) line)) words)))
    
    (destructuring-bind (x y z) list
      (make-point (read-from-string x)
                  (read-from-string y)
                  (read-from-string z)))))

      
(define-object face-triangles ()
  :input-slots (triangle-data-in)
  
  :computed-slots ((vertex-count-count (svref (getf (the triangle-data-in) :vertex-count-counts)
                                              (the index)))
                   
                   (vertex-index-count (svref (getf (the triangle-data-in) :vertex-index-counts)
                                              (the index)))
                   
                   (face-vertex-count (svref (getf (the triangle-data-in) :face-vertex-counts)
                                             (the index)))
                   
                   (cum-vertex-count-count (if (the first?)
                                               (the vertex-count-count)
                                             (+ (the vertex-count-count)
                                                (the previous cum-vertex-count-count))))
                   
                   (cum-vertex-index-count (if (the first?)
                                               (the vertex-index-count)
                                             (+ (the vertex-index-count)
                                                (the previous cum-vertex-index-count))))
                   
                   (cum-face-vertex-count (if (the first?)
                                              (the face-vertex-count)
                                            (+ (the face-vertex-count)
                                               (the previous cum-face-vertex-count))))
                   
                   (vertex-counts (subseq (getf (the triangle-data-in) :vertex-counts)
                                          (- (the cum-vertex-count-count) (the vertex-count-count))
                                          (the cum-vertex-count-count)))
                   
                   (full-vertex-indices (map 'vector #'(lambda(index)
                                                         (+ index 
                                                            (- (the cum-face-vertex-count)
                                                               (the face-vertex-count))))
                                             (the vertex-indices)))
                   
                   (vertex-indices (subseq (getf (the triangle-data-in) :vertex-indices)
                                           (- (the cum-vertex-index-count) (the vertex-index-count))
                                           (the cum-vertex-index-count)))
                   
                   (3d-points (subseq (getf (the triangle-data-in) :3d-points)
                                      (- (the cum-face-vertex-count) (the face-vertex-count))
                                      (the cum-face-vertex-count)))

                   (surface-normals (subseq (getf (the triangle-data-in) :surface-normals)
                                      (- (the cum-face-vertex-count) (the face-vertex-count))
                                      (the cum-face-vertex-count)))

                   (uv-points (subseq (getf (the triangle-data-in) :uv-points)
                                      (- (the cum-face-vertex-count) (the face-vertex-count))
                                      (the cum-face-vertex-count)))
                   
                   
                   (triangle-data (list :vertex-counts (the vertex-counts)
                                        :vertex-indices (the vertex-indices)
                                        :3d-points (the 3d-points)
                                        :uv-points (the uv-points)
                                        :surface-normals (the surface-normals)

                                        ))))



;;
;; VRML/x3D parameter test development 
;;
#+nil
(define-object test-b-spline-surface (b-spline-surface)
  
  :input-slots ((display-controls (list :color :periwinkle 
                                        ;;:line-thickness 2
                                        )))
  
  :computed-slots
  ((points-data '(((0 0 0)(4 1 0)(8 1 0)(10 0 0)(8 -1 0)(4 -1 0)(0 0 0))
                  ((0 0 2) (4 2 2) (8 2 2) (10 0 2) (8 -2 2) (4 -2 2) (0 0 2))
                  ((0 0 4) (4 2 4) (8 2 4) (10 0 4) (8 -2 4) (4 -2 4) (0 0 4))
                  ((0 0 7) (4 1 7) (8 1 7) (10 0 7) (8 -1 7) (4 -1 7) (0 0 7))))
                  
   (control-points (mapcar #'(lambda(list) (mapcar #'apply-make-point list)) 
                           (the points-data))))

  
   :hidden-objects ((view :type 'base-view
                          :projection-vector (getf *standard-views* :trimetric)
                          :page-width (* 5 72) :page-length (* 5 72)
                          :objects (list self))))


#+nil
(define-object scaled-b-spline (base-object)
  
  :input-slots ((scale-factor 1 :settable))
  
  :objects
  ((surface :type 'test-b-spline-surface)
   
   
   (scaled :type 'boxed-surface
           :surface-in (the surface)
           :scale (the scale-factor))
   
   (brep :type 'brep
         :max-chord-height (* (the-child max-extent) 0.0005)
         :%native-brep% (the scaled brep %native-brep%))))

   
   
