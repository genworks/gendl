(in-package :wire-world)

(define-object assembly (base-object)

  :objects
  ((hangar :type 'hangar
	   :orientation (alignment :top (the (face-normal-vector :rear)))
	   :center (translate (the center) 
			      :right (half (the-child width))
			      :front (half (the-child height))))



   (ground :type 'ground
	   :height 0 :length 50 :width (* +phi+ 50))

   (tree :type 'tree
	 :center (translate (the center) :left 12 :front 12 :top (half (the-child length)))
	 :orientation (alignment :rear (the (face-normal-vector :top)))
	 :length 7
	 :radius-1 3 :radius-2 0)

   (teepee :type 'teepee 
	   :center (translate (the center) :right 4 :front 12 :top (half (the-child length)))
	   :orientation (alignment :rear (the (face-normal-vector :top)))
	   :length 3
	   :arc (* 3/2 pi)
	   :radius-1 2 :radius-2 0.5)


   (silo :type 'silo
	 :radius 1
	 :arc (* 3/2 pi)
	 :length 5
	 :orientation (alignment :rear (the (face-normal-vector :top)))
	 :center (translate (the center) :right 4 :rear 12 :top (half (the-child length))))))


(define-object silo (cylinder)
  :computed-slots ((display-controls (list :color :blue))))

(define-object teepee (cone)
   :computed-slots ((display-controls (list :color :brown))))


(define-object tree (cone)
   :computed-slots ((display-controls (list :color :green-forest))))


(define-object ground (box)
  :computed-slots ((display-controls (list :color :green-lime :transparency 0.3))))
    

(define-object hangar (base-object)
  
  :computed-slots ((display-controls (list :color :grey :transparency 0.7))
		   (width (- (apply #'max (mapcar #'get-x (the vertex-list-local)))
			     (apply #'min (mapcar #'get-x (the vertex-list-local)))))
		   (height (the shape projection-depth))
		   (length  (- (apply #'max (mapcar #'get-y (the vertex-list-local)))
			       (apply #'min (mapcar #'get-y (the vertex-list-local)))))
		   
		   (vertex-list-local (list (make-point 0 0 0)
					    (make-point 2 2 0)
					    (make-point 4 3 0)
					    (make-point 6 2 0)
					    (make-point 8 0 0)))

		   (vertex-list (mapcar #'(lambda(point)
					    (the (local-to-global point)))
					(the vertex-list-local))))

  :objects
  ((shape :type 'global-polygon-projection
	  :vertex-list (the vertex-list)
	  :projection-depth 10
	  :projection-vector (the (face-normal-vector :top)))))
			     
