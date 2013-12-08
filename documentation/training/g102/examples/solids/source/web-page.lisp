(in-package :gwl-user)

(define-object surface-ui (base-html-graphics-sheet)
  
  :hidden-objects
  ((b-spline-surface :type 'surf::test-b-spline-surface)
   
   (view-object :type 'web-drawing
		:objects (list (the b-spline-surface))
		:projection-vector (getf *standard-views* :trimetric)
		
		)))


(define-lens (html-format surface-ui)()
  :output-functions
  ((main-sheet
    ()
    (html (:html (:head (:title "Example of a Sectioned Surface"))
		 (:body (:p (the write-development-links))
			(:h2 "Example of a Sectioned Surface")
			(:p (:table
			     (:tr
			      ((:td :bgcolor :yellow) "Surface Area")
			      (:td (:princ (number-round 
					    (the b-spline-surface area) 3))))
			     
			     ))
			
			(with-html-form ()
			  (:p (the write-geometry)))))))))
			
			
			
