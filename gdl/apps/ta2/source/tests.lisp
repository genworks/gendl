(in-package :gdl-user)



(define-object tatu-test (gwl:base-html-sheet)
 
  :computed-slots
  ((list-test '(:a36 nil nil "W3b") :settable)
   
   (nil-test nil :settable)
   
   (keyword-test :a36 :settable)
   
   (string-test "hello" :settable)
   
   (number-test 45 :settable)))


(in-package :gwl-user)


(define-object tatu-children (base-html-sheet)
 
  :computed-slots
  ((length 1 :settable)
   
   (width 2 :settable)
   
   (height 3 :settable)
   
   (aapple (i-dont-know-how 1 2 3))
   
   )
  
  ;; It would be great to be able to see kids 1-3 even though 4 can't appear. Then you can use 
  ;; tatu to help track down the error that feeds kid-4's part type. 
  :objects
  ((kid-1 :type 'my-box
	  :pass-down (:length :width :height))
   
   (kid-2 :type 'my-box
	  :pass-down (:length :width :height))
   
   (kid-3 :type 'my-box
	  :pass-down (:length :width)
	  :height (error "This will cause all of the children not to appear This will cause all of the children not to appear This will cause all of the children not to appear This will cause all of the children not to appear This will cause all of the children not to appear"))
   

   (kid-4 :type (if (> (the kid-3 volume) 10)
		    'my-box
		  'box)		  
	  :pass-down (:length :width :height))
   
   (kid-5 :type 'my-box
	  :sequence (:size 5)
	  :display-controls (if (= (the-child index) 4) 
				   (error "Can't compute display-controls")
			      (list :color :green))
	  :strings-for-display (if (= (the-child index) 3) 
				   (error "Can't compute strings-for-display")
				 (format nil "kid-5 ~a" (the-child index))))
   
   (kid-6 :type 'tatu-children)
   ))



(define-object my-box (box))


(define-object draw-error (base-object)
  
  :objects
  ((box :type 'box 
	:height 20
	:width 10
	:length 10
	:display-controls (list :color :green)
	:center (translate (the center) :left 25))
	
   
   (box-1 :type 'box
	  :height 1 ;;(/ 1 0)
	  :width 10
	  :length 10)))


(define-object measure-distance (base-object)
  
  :objects
  ((left-box :type 'box 
	     :display-controls (list :color :red)
	     :length 10 :width 20 :height 30
	     :center (translate (the center) :left 100))
   
   (left-center-box :type 'box 
		    :display-controls (list :color :orange)
		    :length 10 :width 20 :height 30
		    :center (translate (the center) :left 50))
   
   (right-box :type 'box 
	     :display-controls (list :color :green)
	     :length 10 :width 20 :height 30
	     :center (translate (the center) :right 100))
   
   (right-center-box :type 'box 
	     :display-controls (list :color :blue)
	     :length 10 :width 20 :height 30
	     :center (translate (the center) :right 50))
   
   ))




