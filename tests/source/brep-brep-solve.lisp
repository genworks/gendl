(in-package :gdl-lift-tests)


(define-object brep-brep-solve-test (base-object)
  
  :computed-slots  ((brep (the sphere brep))
		    
		    (face-breps (mapcar #'(lambda(face)
					    (the-object face basis-surface brep))
					(list-elements (the box faces)))))
  
  
  :objects
  ((sphere :type 'spherical-surface
	   :radius 10
	   )
   
   (box :type 'box-solid
	:center (the sphere brep bounding-bbox center)
	:length (the sphere brep bounding-bbox length)
	:width (the sphere brep bounding-bbox width) 
	:height (the sphere brep bounding-bbox height))
   
   
   (face-points :type 'point
		:sequence (:size (length (the face-breps)))
		:center (the sphere brep (brep-solve (nth (the-child index) (the face-breps)) :minimize))))
  
  
  :functions
  ((test-solve
    ()
    (dolist (brep (the face-breps))
      (print-variables (the sphere brep (brep-solve brep :minimize)))))))
  
  
