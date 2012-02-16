(in-package :surf)

(define-format x3d (vrml))


(define-lens (x3d surface) ()
  :output-functions
  ((shape 
    ()
    (write-the brep shape))))
          


(define-lens (x3d brep)()
  :output-functions
  (
   (shape
    ()
    (write-the tri-shape))
   
   (tri-shape
    ()
    (mapcar #'(lambda (face)
                (write-the (shape-face face)))
            (the triangle-data)))
   
   (shape-face
    (face)
    (let* ((vertex-list (coerce (getf face :vertex-indices) 'list))
           (3d-point-list (coerce (getf face :3d-points) 'list))
           (formatted-vertices (format nil "~{~a -1 ~}" (surf::make-triplet-strings vertex-list)))
           (3d-points (mapcar #'(lambda (coord)
                                  (the (global-to-local coord)))
                              3d-point-list)))
      (cl-who:with-html-output (*stream* nil :indent nil)
        (:Shape
         (:Appearance (progn (if (getf (the display-controls) :pixel-texture)
                                 (write-the pixel-texture)
                               (write-the material-properties))
                             (when (getf (the display-controls) :linetype)
                               (write-the line-properties))))
         (:IndexedFaceSet :creaseAngle "1.571"
                          :solid "false"
                          :coordIndex formatted-vertices
                          (:Coordinate :point (format nil "~{~a~^, ~}" 
                                                      (let ((*read-default-float-format* 'single-float))
                                                        (mapcar #'(lambda(point) 
                                                                    (format nil "~a ~a ~a"
                                                                            (coerce (get-x point) 'single-float)
                                                                            (coerce (get-y point) 'single-float) 
                                                                            (coerce (get-z point) 'single-float)
                                                                            ))
                                                                3d-points)))))))))))

(define-lens (x3d base-drawing)()
  :output-functions
  ((cad-output
    ()
    (let (
          ;;(req (when (find-package :gwl) (symbol-value (read-from-string "gwl:*req*"))))
          )
      (let (;;(keep-alive? (and req (slot-value req (read-from-string "net.aserve::request-header-connection"))))
            ;;(protocol (and req (slot-value req (read-from-string "net.aserve::protocol"))))
            )
          
        ;;(when *debug?* (describe req) (print-variables self keep-alive? protocol))
          
        (write-the header)
        
        
        ;;
        ;; FLAG -- each view should really be in a separate X3D area or scene, but using references if possible. 
        ;;
        (cl-who:with-html-output (*stream* nil :indent t)
          (mapc #'(lambda(key viewpoint)
                    (let ((position (getf viewpoint :point))
                          
                          #+nil
                          (orientation (or (quaternion-to-rotation 
                                            (matrix-to-quaternion (getf viewpoint :orientation)))
                                           (make-vector 0 0 1 0)))
                          
                          (orientation (matrix-to-rotation (getf viewpoint :orientation)))
                          
                          (field-of-view (degree (getf viewpoint :field-of-view)))
                          
                          ;;(tri-view (getf *standard-views* :trimetric))
                          ;;(tri-view (make-vector 0.8342367128320977 -0.4377640254359154 0.3352786378480434 ))
                          (tri-view (getf *standard-views* :left))
                          
                          (distance 100)
                          )
                      
                      (declare (ignore orientation position field-of-view))
                      
                      (cl-who:htm 
                       ((:viewpoint :position  
                                    (let ((point (scalar*vector distance tri-view)))
                                      (format nil "~a ~a ~a" 
                                              (get-x point)
                                              (get-y point) 
                                              (get-z point)))

                                    :orientation 
                                    (let ((orientation 
                                           (matrix-to-rotation
                                            (let ((eye-vector (subtract-vectors (make-vector 0 0 0)
                                                                                tri-view)))
                                              (alignment :bottom eye-vector
                                                         :left (let ((ortho 
                                                                      (orthogonal-component 
                                                                       eye-vector 
                                                                       (if (coincident-point? eye-vector 
                                                                                              (the (face-normal-vector 
                                                                                                    :rear)))
                                                                           (the (face-normal-vector :right))
                                                                         (the (face-normal-vector :rear))))))
                                                                 (if (coincident-point? eye-vector ortho)
                                                                     (the (face-normal-vector :rear))))))))
                                          
                                          #+nil
                                          (orientation 
                                           (quaternion-to-rotation 
                                            (matrix-to-quaternion 
                                             (let ((eye-vector (subtract-vectors (make-vector 0 0 0)
                                                                                 tri-view)))
                                               (alignment :bottom eye-vector
                                                          :left (let ((ortho 
                                                                       (orthogonal-component 
                                                                        eye-vector 
                                                                        (if (coincident-point? eye-vector 
                                                                                               (the (face-normal-vector 
                                                                                                     :rear)))
                                                                            (the (face-normal-vector :right))
                                                                          (the (face-normal-vector :rear))))))
                                                                  (if (coincident-point? eye-vector ortho)
                                                                      (the (face-normal-vector :rear))))))))))
                                      (format nil "~a ~a ~a ~a"
                                              (get-x orientation)
                                              (get-y orientation)
                                              (get-z orientation)
                                              (get-w orientation)))
                                    :description (string-capitalize key)
                                    ;;:fieldofview (format nil "~a" (/ pi 4))
                                    ))

                       ((:navigationinfo :type "Examine"
                                         :headlight "TRUE"
                                         :speed (format nil "~a" (getf viewpoint :speed)))))))
                ;;(plist-keys (the viewpoints))
                ;;(plist-values  (the viewpoints))
                (list (first (the viewpoints)))
                (list (second  (the viewpoints)))))
        
        
        (mapc #'(lambda(view)
                  (let ((object-roots (ensure-list (the-object view object-roots)))
                        (objects (ensure-list (the-object view  objects))))
                      
                    (when *debug?* (print-variables object-roots objects))
                    
                    (mapc #'(lambda(root) (write-the-object root (cad-output-tree :header? nil))) object-roots)
                    
                    (mapc #'(lambda(leaf) (write-the-object leaf (cad-output :header? nil))) objects)))

              (the views))
          
        )))))


(define-lens (x3d base-object)()
  :output-functions
  (
   (header
    ()
    
    #+nil
    (format *stream* "~a~%~a~%"
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            "<!DOCTYPE X3D PUBLIC \"ISO//Web3D//DTD X3D 3.1//EN\" \"http://www.web3d.org/specifications/x3d-3.1.dtd\">"))
   
   (rgb-color
    (color)
    (when color
      (let ((fcolor (coerce (lookup-color color) 'list)))
        (format nil "~,3f ~,3f ~,3f" (nth 0 fcolor) (nth 1 fcolor) (nth 2 fcolor)))))
   
   (pixel-texture
    ()
    (cl-who:with-html-output (*stream* nil :indent nil)
      (:PixelTexture :image (getf (the display-controls) :pixel-texture))))
   
   (line-properties
    ()
    (cl-who:with-html-output (*stream* nil :indent nil)
      (:LineProperties :linetype (getf (the display-controls) :linetype))))
   
   (material-properties
    ()
    (cl-who:with-html-output (*stream* nil :indent nil)
      (:Material :diffuseColor (write-the (rgb-color (if (getf (the display-controls) :color)
                                                         (getf (the display-controls) :color)
                                                       :black)))
                 :ambientIntensity (getf (the display-controls) :ambient-intensity)
                 :emissiveColor (write-the (rgb-color (getf (the display-controls) :emissive-color)))
                 :shininess (getf (the display-controls) :shininess)
                 :specularColor (write-the (rgb-color (getf (the display-controls) :specular-color)))
                 :transparency (getf (the display-controls) :transparency)
                 )))
   
   (appearance
    ()
    (cl-who:with-html-output (*stream* nil :indent nil)
      (write-the pixel-texture)
      (write-the material-properties)))
   
   (transform
    ()
    (let* (;;(rotation (quaternion-to-rotation (matrix-to-quaternion (the orientation))))
           (rotation (matrix-to-rotation (the orientation)))
           (x (get-x (the center))) 
           (y (get-y (the center))) 
           (z (get-z (the center))))
      
      (cl-who:with-html-output (*stream* nil :indent nil)
        (:Transform :translation (when (not (every #'zerop (list x y z)))
                                   (format nil "~3,7f ~3,7f ~3,7f" x y z))
                    :rotation (when rotation
                                (format nil "~3,7f ~3,7f ~3,7f ~3,7f" (get-x rotation) (get-y rotation) (get-z rotation) (get-w rotation)))
                    (:Group (write-the shape))))))
   
   (scene 
    ()
    (cl-who:with-html-output (*stream* nil :indent nil)
      (:X3D :profile "Immersive"
            :version "3.1" 
            :xmlns\:xsd "http://www.w3.org/2001/XMLSchema-instance"
            :xsd\:noNamespaceSchemaLocation "http://www.web3d.org/specifications/x3d-3.1.xsd"
            (:Scene (:Transform (:MetadataString :name "fieldID" 
                                                 ;;:value (the x3d-node-id)
                                                 :value (the root-path-string)
                                                 :containerField "metadata")
                                (:Group (write-the cad-output-tree)))))))


   (cad-output
    (&key (header? t))
    
    (when header? (write-the header))
    
    (let ((orientation (the orientation)))
      
      (let (;;(rotation (quaternion-to-rotation (matrix-to-quaternion orientation)))
            (rotation (matrix-to-rotation orientation))
            (x (get-x (the center)))
            (y (get-y (the center))) 
            (z (get-z (the center)))
            )
        
        (cl-who:with-html-output (*stream* nil :indent nil)
	  ((:Transform :translation (unless (every #'zerop (list x y z))
				       (format nil "~3,7f ~3,7f ~3,7f" x y z)))
	   ((:Transform :rotation (when rotation
				   (format nil "~3,7f ~3,7f ~3,7f ~3,7f" 
					   (get-x rotation) (get-y rotation) (get-z rotation) (get-w rotation))))
	    (if gwl::*onclick-function* 
		(cl-who:htm 
		 ((:Anchor :url (format nil "javascript:event=null;~a"
					(funcall gwl::*onclick-function* self)))
                   
                   
                   
		  (write-the shape)))
		(write-the shape)))
	   (when (and (typep self 'outline-specialization-mixin)
		      (not (ignore-errors (typep self (read-from-string "surf:surface")))))
	     (let* ((center (reverse-vector (the center)))
		    (x (get-x center)) (y (get-y center)) (z (get-z center))
		    (inverse (when (the orientation)
			       (matrix-to-rotation (matrix:transpose-matrix 
						    (the orientation)))
			       #+nil
			       (quaternion-to-rotation (matrix-to-quaternion (matrix:transpose-matrix 
									      (the orientation)))))))
	       (cl-who:htm
		((:Transform :translation (when (not (every #'zerop (list x y z)))
					    (format nil "~3,7f, ~3,7f, ~3,7f" x y z)))
		 ((:Transform :rotation (when inverse
					  (format nil "~3,7f ~3,7f ~3,7f ~3,7f" 
						  (get-x inverse) (get-y inverse) (get-z inverse) (get-w inverse))))
		  (mapc #'(lambda(outline-object)
			    (write-the-object outline-object (cad-output :header? nil)))
			(the outline-leaves))))))))))))

   
   (cad-output-tree
    (&key (header? t) (from-root? t))
    
    (when header? (write-the header))
    
    (let ((center (if from-root? (the center) (the local-center)))
	  (orientation (if from-root? (the orientation) (the local-orientation))))
      (let (;;(rotation (quaternion-to-rotation (matrix-to-quaternion orientation)))
            (rotation (matrix-to-rotation orientation))
            (x (get-x center)) (y (get-y center)) (z (get-z center)))
        
         
        (cl-who:with-html-output (*stream* nil :indent nil)
          ((:Transform :translation (unless (every #'zerop (list x y z))
				      (format nil "~3,7f ~3,7f ~3,7f" x y z)))
	   ((:Transform :rotation (when rotation
				    (format nil "~3,7f ~3,7f ~3,7f ~3,7f" 
					    (get-x rotation) (get-y rotation) (get-z rotation) (get-w rotation))))
           
	    (if (null (the children))
		(if gwl::*onclick-function* 
		    (cl-who:htm 
		     ((:Anchor :url (format nil "javascript:event=null;~a"
					    (funcall gwl::*onclick-function* self)))
		      (write-the shape)))
		    (write-the shape))
		
		(mapc #'(lambda(child) 
			  (write-the-object child (cad-output-tree :header? nil :from-root? nil))) (the children)))
    
	    (when (and (typep self 'outline-specialization-mixin)
		       (not (ignore-errors (typep self (read-from-string "surf:surface")))))
      
	      (let* ((center (reverse-vector (the center)))
		     (x (get-x center)) (y (get-y center)) (z (get-z center))
		     (inverse (when (the orientation)
				#+nil
				(quaternion-to-rotation (matrix-to-quaternion (matrix:transpose-matrix (the orientation))))
				(matrix-to-rotation (matrix:transpose-matrix (the orientation))))))
		(cl-who:htm
		 ((:Transform :translation (when (not (every #'zerop (list x y z)))
					     (format nil "~3,7f, ~3,7f, ~3,7f" x y z)))
		  ((:Transform :rotation (when inverse
					   (format nil "~3,7f ~3,7f ~3,7f ~3,7f" 
						   (get-x inverse) (get-y inverse) (get-z inverse) (get-w inverse))))
		   (mapc #'(lambda(outline-object)
			     (write-the-object outline-object (cad-output :header? nil)))
			 (the outline-leaves)))))))))))))))
   



(define-lens (vrml line)()
  :output-functions
  ((shape
    ()
    
    (let* ((points (mapcar #'(lambda(point) (the (global-to-local point))) (list (the start) (the end))))
           (indices (list-of-numbers 0 (1- (length points)))))
      (cl-who:with-html-output (*stream* nil :indent nil)
        ((:Shape 
          (:Appearance (write-the material-properties)))
         ((:IndexedLineSet :coordIndex (format nil "~{~a ~}-1" indices))
          ((:Coordinate :point (format nil "~{~a~^ ~}" 
                                       (mapcar #'(lambda(point) (format nil "~a ~a ~a" 
                                                                        (get-x point) 
                                                                        (get-y point) 
                                                                        (get-z point))) points)))))))))))

(define-lens (x3d cone)()
  :output-functions
  (
   (shape
    ()
    (if (the simple?)
	(cl-who:with-html-output (*stream* nil :indent nil)
	  (:Shape
	   (:Appearance (write-the material-properties))
	   (:Cone :bottomRadius (the radius-1)
		  :topRadius (the radius-2)
		  :height (the length)
		  :bottom (when (the bottom-cap?) "TRUE"))))
	(call-next-method)))))
	


(define-lens (x3d sphere)()
  :output-functions
  (
   (shape
    ()
    (if (the simple?)
	(cl-who:with-html-output (*stream* nil :indent nil)
	  (:Shape
	   (:Appearance (if (getf (the display-controls) :pixel-texture)
			    (write-the pixel-texture)
			    (write-the material-properties)))
	   (:Sphere :radius (the radius))))
	(call-next-method)))))



(define-lens (x3d cylinder)()
  :output-functions
  (
   (shape
    ()
    (if (the simple?)
	(cl-who:with-html-output (*stream* nil :indent nil)
	  (:Shape
	   (:Appearance (write-the material-properties))
	   (:Cylinder :radius (the radius) 
		      :height (the length))))
	(call-next-method)))))



(define-lens (x3d box)()
  :output-functions
  (
   (shape
    ()
    (cl-who:with-html-output (*stream* nil :indent nil)
      (:Shape
       (:Appearance (write-the material-properties))
       (:Box :size (format nil "~a ~a ~a" 
                           (to-double-float (the width)) 
                           (to-double-float (the length))
                           (to-double-float (the height)))))))
   )
  )

(define-lens (x3d global-polyline)()
  :output-functions
  (
   (shape
    ()
    (cl-who:with-html-output (*stream* nil :indent nil)
      (:Shape
       (:Appearance (progn (when (getf (the display-controls) :linetype)
                             (write-the line-properties))
                           (write-the material-properties)))
       (:IndexedLineSet :coordIndex (format nil "~{~a ~}-1" (list-of-numbers 0 (1- (length (the vertex-list)))))
                        (:Coordinate :point (format nil "~{~a~^, ~}" 
                                                    (let ((*read-default-float-format* 'single-float))
                                                      (mapcar #'(lambda(point) (format nil "~a ~a ~a"
                                                                                       (coerce (get-x point) 'single-float)
                                                                                       (coerce (get-y point) 'single-float) 
                                                                                       (coerce (get-z point) 'single-float)
                                                                                       ))
                                                              (the vertex-list)))))))))
   )
  )



(define-lens (x3d ifs-output-mixin)()
  :output-functions
  ((shape
    ()
    (cl-who:with-html-output (*stream* nil :indent t)
      (:Shape
       (:Appearance (write-the material-properties))
       ((:IndexedFaceSet :solid "false"
			 :creaseAngle "1.571"
                         :coordIndex (format nil "~{~{~a~^ ~}~^ -1 ~}" (the ifs-indices)))
        ((:Coordinate :point (format nil "~{~{~a~^ ~}~^ ~}"
                                     (map 'list #'(lambda(point) 
                                                    (let ((point (the (global-to-local point))))
                                                      (list (get-x point) (get-y point) (get-z point))) )
                                          (the ifs-array)))))))))))


(define-lens (x3d curve)()
  :output-functions
  ((shape
    ()
    (let* ((total-points (floor (the total-length)))
           (points (the (equi-spaced-points total-points))))
      (let ((indices (list-of-numbers 0 (1- (length points)))))
        (cl-who:with-html-output (*stream* nil :indent nil)
          (:Shape
           (:Appearance (write-the material-properties))
           (:IndexedLineSet :coordIndex (format nil "~{~a ~}-1" indices)
                            (:Coordinate :point (format nil "~{~a~^, ~}"
                                                        (let ((*read-default-float-format* 'single-float))
                                                          (mapcar #'(lambda(point) 
                                                                      (let ((point (the (global-to-local point))))
                                                                        (format nil "~a ~a ~a" 
                                                                                (coerce (get-x point) 'single-float)
                                                                                (coerce (get-y point) 'single-float) 
                                                                                (coerce (get-z point) 'single-float)
                                                                                ))) points))))))))))))

#+nil
(define-lens (x3d curve)()
  :output-functions
  ((shape
    ()
    
    (let* ((beziers (the %decomposed-beziers%))
           ;;(point-distance 10)
           (first? t)
           (points (apply #'append (mapcar #'(lambda (crv)
                                                     (let ((bezier-points
                                                            (the-object crv (equi-spaced-points *curve-chords*))))
                                                       (if first? 
                                                           (progn (setq first? nil)
                                                                  bezier-points)
                                                         (rest bezier-points))))
                                                 beziers))))
      (let ((indices (list-of-numbers 0 (1- (length points)))))
        (cl-who:with-html-output (*stream* nil :indent nil)
          (:Shape
           (:Appearance (write-the material-properties))
           (:IndexedLineSet :coordIndex (format nil "~{~a ~}-1" indices)
                            (:Coordinate :point (format nil "~{~a~^, ~}"
                                                        (let ((*read-default-float-format* 'single-float))
                                                          (mapcar #'(lambda(point) 
                                                                      (let ((point (the (global-to-local point))))
                                                                        (format nil "~a ~a ~a" 
                                                                                (coerce (get-x point) 'single-float)
                                                                                (coerce (get-y point) 'single-float) 
                                                                                (coerce (get-z point) 'single-float)
                                                                                ))) points))))))))))))
