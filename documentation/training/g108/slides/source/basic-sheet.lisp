(in-package :training-g108)

(define-object basic-sheet (slide-show-leaf)
  
  :computed-slots
  ((strings-for-display "Making a basic web page with Data from your Application")
   
   (slide-data `((:title "Goals for G108" :bullet-points
			 ((:description "Create a basic static web page from information in your application.")
			  (:description "Allow user interaction with the model (i.e. changing or \"bashing\" values).")
			  (:description "Include geometry display (vector, raster, and shaded) in your application.")))

		 
		 (:title "The <tt>base-ajax-sheet</tt> Primitive"
			 :bullet-points
			 ((:description
			   "Basic mixin for representing a web page"
			   :examples
			   ((:code (gwl:define-package :training-g108))

			    (:code (in-package :training-g108))

			    (:define-object hello-world)

			    (:code (publish-gwl-app "/hello-world"
						    "training-g108::hello-world"))))


			  (:description
			   "With the above code, you can visit in your browser:
http://localhost:9000/hello-world
")))
			  

		 
		 (:title "<tt>with-cl-who</tt> and <tt>with-cl-who-string</tt> syntax"
			 :bullet-points
			 ((:description
			   "cl-who converts LHTML into standard HTML")

			  (:description
			   ,(with-cl-who-string ()
			      "Full docs available from "
			      ((:a :href "http://www.weitz.de/cl-who/") " Here ")))

			  (:description
			   "LHTML markup uses keyword and nested parentheses instead of raw HTML markup tags:"
			   :examples
			   ((:code (in-package :training-g108))

			    (:define-object hello-world-htm)

			    (:code (publish-gwl-app "/hello-world-htm"
						    "training-g108::hello-world-htm"))

			    ))

			  (:description
			   "Sheet will normally be in Standard Compliance mode now.")

			  #+nil
			  (:description
			   ,(with-cl-who-string ()
			      ((:img :src "/g108/images/hello-world-htm.png")))))
			 
			 
			 )

		 (:title "Generating Dynamic Content"
			 :bullet-points
			 ((:description
			   "You can drop into normal Lisp/GDL at any time within
cl-who markup")

			  (:description
			   "To get back to cl-who, use the htm directive"

			   :examples
			   ((:code (in-package :training-g108))

			    (:define-object revenue-table)

			    (:code (publish-gwl-app "/revenue-table"
						    "training-g108::revenue-table"))

			    ))))))))





