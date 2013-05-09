;;
;; Copyright 2012 Genworks International and the Delft University of
;; Technology
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

(in-package :training-g102-tud)


(defmacro with-htm (&body body)
  `(with-cl-who-string () ,@body))


(define-object assembly (slide-show-node)
  :input-slots
  ((title "G102: GDL Quickstart (customized for TU Delft)")
   (slide-package (find-package :training-g102-tud))
   (image-base-url "/g102-tud/images/")
   (images-path *images-path*)
   (style-url "/static/gwl/style/top.css"))

  
  :objects
  ((introduction :type 'introduction)
   (objects :type 'objects)
   (geometry :type 'geometry)
   

   ))



(define-object introduction (slide-show-leaf)
  
  :computed-slots
  ((strings-for-display "Introduction")
   
   (slide-data `((:title "Goals" :bullet-points
                         ((:description "Spark an interest in the Generative KBE approach for solving Engineering and other problems")
                          (:description "Provide Theoretical Knowledge combined with Hands-on Experience")
                          (:description ,(with-htm "Help you develop the Judgement to know when Generative KBE (and Genworks"
						   ((:font :size "-2") (:sup "&reg;"))
						   " GDL in particular) will be Appropriate for a given problem"))))

                 (:title "Topics Covered in G102" :bullet-points
			 ((:description ,(with-cl-who-string 
					  ()
					  "Objects in GDL and the " ((:span :class "gdl-operator") "define-object") " operator"
					  (:ul (:li "Debugging (basic)")
					       (:li "Inspection & Visualization (tasty)"))))

			  (:description ,(with-output-to-string (ss)
								(html-stream 
								 ss
								 "Geometry and Coordinate Systems"
								 (:ul (:li "Points")
								      (:li "Curves")
								      (:li "Surfaces")
								      (:li "Solids")))))
			  (:description "Custom User Interfaces (optional)")
			  (:description "Interacting with the Outside World")
			  (:description "Debugging and Performance (detailed)")))

		 (:title "What is Common Lisp?" :bullet-points
			 ((:description ,(with-htm "A " ((:a :href "http://en.wikipedia.org/wiki/Common_Lisp_HyperSpec" :target "supplemental") 
							 "Specification")
						   " for a "
						   ((:a :href "https://www.google.com/search?client=ubuntu&channel=fs&q=dynamic+languages&ie=utf-8&oe=utf-8"
							:target "supplemental") 
						    "Dynamic")
						   ", Object-oriented Language and Runtime environment"))
			  (:description "Industry-standard dialect of the Lisp language family")
			  (:description "Available in several Commercial and Open-Source implementations")))


                 (:title "What is GDL?" :bullet-points
			 ((:description "A Declarative language environment embedded in Common Lisp")
			  (:description "A technology enabling you to define 
problems and their solutions using an intuitive, straightforward structure")
			  (:description "A Compiler and Runtime engine enabling Model creation and Application deployment")
			  (:description ,(with-htm "A cross-platform "
						   ((:a :href "http://en.wikipedia.org/wiki/Web_application_framework")
						    "web application framework")
						   " seamlessly embedded in the core language"))
			  (:description 
			   ,(with-htm "Kernel and basic geometry freely available through the "
				      ((:a :href "http://www.gnu.org/licenses/agpl-3.0.html" :target "_fresh_") "AGPL")
				      "-licensed "
				      ((:a :href "http://github.com/genworks/gendl" :target "_fresh_") 
				       "Gendl" ((:font :size "-2") (:sup "&#0153;")))
				       " Project. Proprietary (closed-source) application distribution, Geometry Kernel, etc, available 
through commercial Genworks"
				       ((:font :size "-2") (:sup "&reg;"))
				       " GDL package."))))
			  
                 (:title "A Path of Discovery: GDL as a Learning Tool"
                         :bullet-points 
                         ((:description "Humans learn best through Action and Discovery")
                          (:description "Applies to learning GDL itself")
                          (:description "Applies to learning about your own engineering domain")))))))


(define-object objects (slide-show-leaf)

  :computed-slots
  ((strings-for-display "Functions and Objects")
   
   (slide-data 
    `((:title 
       "Function"
       :bullet-points
       ((:description
	 "A machine which accepts some Input then yields a defined Output"
	 :image-url "function-machine.jpg"
	 :image-caption "A Function Machine <small><i>(image from http://wbadvies.nl)</i></small>")
	
	))

      (:title 
       "Calling Functions in Common Lisp"
       :bullet-points 
       ((:description "Function Call is expressed as a List 
expression (a.k.a. \"Symbolic EXPression\" or \"S-exp\")")
	(:description "Function name first, then arguments, separated by spaces")
	(:description ,(with-htm "For example, we feed "
				 ((:span :class :lisp-code) "2")
				 " and "
				 ((:span :class :lisp-code) "2")
				 " into the function "
				 ((:span :class :lisp-code) "+")
				 " to yield the result which is "
				 ((:span :class :lisp-code) "4"))
		      :examples
		      ((:code 
			(+ 2 2)
			:return-value 4)))

	(:description 
	 ,(with-htm "The multiply function ("
		    ((:span :class :lisp-code) "*")
		    ") applied to "
		    ((:span :class :lisp-code) "3")
		    " and "
		    ((:span :class :lisp-code) "3"))
	 :examples
	 ((:code 
	   (* 3 3)
	   :return-value 9)))

	(:description 
	 ,(with-htm "The "
		    ((:span :class :lisp-code) "string-append")
		    " function applied to three strings")
	 :examples
	 ((:code 
	   (string-append "hey" " " "now")
	   :return-value "hey now")))))

      (:title 
       "Defining and Using Custom Functions in Common Lisp"
       :bullet-points 
       ((:description 
	 ,(with-htm "Use the operator "
		    ((:span :class :lisp-code) "defun")
		    ", followed by name, argument list, and body.")
	 :examples
	 ((:code 
	   (defun square (number) (* number number))
	   :return-value square)))
	(:description 
	 "Now you can call it just like any other function"
	 :examples
	 ((:code 
	   (square 3)
	   :return-value 9)))))

      (:title "Function as Object" 
	      :bullet-points
	      ((:description
		,(with-htm
		  ((:table :border 1)
		   (:tr ((:th :width 0.5) :br)
			((:th :width 0.5 ) 
			 "Function (Common Lisp)")
			((:th :width 0.5 ) 
			 "Object (GDL)"))
		   (:tr
		    (:td (:b "Typical Purpose"))
		    ((:td :bgcolor (lookup-color :aquamarine :format :hex)
			  :colspan 2 :align :center)
		     (:ol
		      (:li "Accept some " 
			   (:i "inputs"))
		      (:li "possibly performs some " 
			   (:i "side-effects"))
		      (:li "compute one or more "
			   (:i "outputs")))))

		   (:tr
		    (:td (:b "How to Use"))
		    ((:td :bgcolor (lookup-color :thistle :format :hex))
		     "You call it by name, by evaluating a Lisp expression")

		    ((:td :bgcolor (lookup-color :aquamarine :format :hex))
		     (:ol (:li "You create or retrieve an " (:i "object"))
			  (:li "You send " (:i "messages") 
			       " to the object to get the outputs"))))

		   (:tr
		    (:td (:b "How to Define"))
		    ((:td :bgcolor (lookup-color :thistle :format :hex))
		     (:pre
		      (:b ((:span :class :lisp-code) "(defun " (:i "name (arguments) body)")))))
		    ((:td :bgcolor (lookup-color :aquamarine :format :hex))
		     (:code
		      (:pre (:b ((:span :class :gdl-object-def)
				 ((:span :class :gdl-operator) "(define-object ")
				 ((:span :class :gdl-object-def) (:i "name ")) 

				 (:i "(mixins) " :br
				     "&nbsp;&nbsp;"  ((:span :class :lisp-code) "specifications)"))))))))

		   (:tr
		    (:td (:b "How to Decompose Complexity"))
		    ((:td :bgcolor (lookup-color :thistle :format :hex))
		     "Call other function definitions from within a function definition")
		    ((:td :bgcolor (lookup-color :aquamarine :format :hex))
		     (:ol (:li "<i>Inherit</i> <i>slots</i> from other definitions using <i>mixins</i>")
			  (:li "Include <i>objects</i> of other types inside an object"))))))
		:suppress-end-dot? t)))



      (:title "Empty Surface Mixin"
	      :bullet-points
	      ((:description "Define an Empty Surface (we will use real built-in surface later)"
			     :examples
			     ((:define-object gdl-user::empty-surface)))

	       
	       (:description ,(with-htm
			       ((:span :class "gdl-operator") "define-object") " - main operator used in GDL to create definitions."))

	       (:description ,(with-htm
			       ((:span :class "gdl-object-def") "empty-surface") " - Name of our new definition."))

	       (:description ,(with-htm
			       ((:span :class "gdl-object-def") "base-object") " - Name of single mixin (should be pre-defined)."))))

      (:title "Basic Wing Skeleton"
	      :bullet-points 
	      ((:description "Define a wing with initial slots, mixes in empty surface to start with"
			     :image-url "uml-2-5.png"
			     :examples
			     ((:define-object gdl-user::wing)))
	       (:description ,(with-htm
			       ((:span :class "gdl-object-def") "wing") " - our new definition name."))
	       (:description ,(with-htm
			       ((:span :class "gdl-object-def") "empty-surface") " - Name of single mixin (which we already defined)."))
	       (:description ,(with-htm
			       ((:span :class "gdl-section-keyword") ":computed-slots") " - constant and computed values which can be 
\"answered\" by instances of this definition."))

	       (:description 
		,(with-htm (:small 
			    (:i "Example code is "
				((:a :href "https://github.com/genworks/gendl/blob/master/documentation/training/g102-tud/examples/source/ch2-examples.lisp"
				     :target "_fresh")
				 "here")
				". Press \"Raw\" for raw downloadable form."))))))

      
      (:title ,(with-cl-who-string
			       ()
			       ((:span :class "gdl-section-keyword") ":computed-slots"))
	      :bullet-points 
	      ((:description "Each slot is given as its own list:"
			     :code
			     ((:examples
			       ((:define-object gdl-user::wing)))))
	       (:description ,(with-cl-who-string 
			       ()
			       "Each list contains "
			       (:ul (:li "its name --- e.g. "
					 ((:span :class "gdl-message-name") "c-avg")
					 " is a name, and")
				    (:li "its expression --- e.g. " 
					 ((:span :class "gdl-object-def") "(/ (+ (the c-root) (the c-tip)) 2)")
					 " is an expression."))))
	       (:description "The expression can be either a constant value, or an expression which computes a value on-demand.")))


      (:title "Making and Using Objects on Command-line (REPL)"
	      :bullet-points
	      ((:description "Set a toplevel variable to an object:"
			     :examples ((:code (setq obj (make-object 'wing)))))

	       (:description "Then ask it a question (i.e. \"send it a message\"):"
			     :examples ((:code (the-object obj taper)
					       :return-value 1/2)))

	       (:description "Note the rational number (1/2) as a return value.")))


      (:title "Using self to Avoid the Need for the-object"
	      :bullet-points 
	      ((:description "Two ways to set self to a new object:"
			     :examples
			     ((:code (setq self (make-object 'wing)))
			      (:code (make-self 'wing))))

	       (:description ,(with-htm "Now you can just use "
					((:span :class "gdl-operator") "the")
					" instead of "
					((:span :class "gdl-operator") "the-object object..."))
			     :examples
			     ((:code (the taper))
			      (:return-value 1/2)))

	       (:description ,(with-htm ((:span :class "gdl-object-def") "self")
					" is set automatically within scope of "
					((:span :class "define-object") "self") "."))

	       (:description ,(with-htm "So there is heavy use of "
					((:span :class "gdl-operator") "the")
					" inside most object definitions:")
			     :examples
			     ((:define-object gdl-user::wing)))

	       (:description 
		,(with-htm 
		  (:small
		   (:i
		    "Example code is "
		    ((:a :href "https://github.com/genworks/gendl/blob/master/documentation/training/g102-tud/examples/source/ch2-examples.lisp"
			 :target "_fresh")
		     "here")
		    ". Press \"Raw\" for raw downloadable form."))))))


      (:title "Making and Using Objects in \"tasty\""
	      :bullet-points
	      ((:description "Visit http://localhost:9000/tasty"
			     :image-url "tasty-wing-entry.png")

	       (:description "This creates an instance of wing and shows you different views of it.")))


      (:title "Components of Tasty"
	      :bullet-points
	      ((:description ,(with-htm "Tree:" (:ul (:li "expand/condense")
						     (:li "perform selected \"click-mode\" action")
						     (:li "root node is your instance")
						     (:li "select click-mode from Tree menu"))))
	       (:description ,(with-htm "Inspector:" (:ul (:li "View slots of an object")
							  (:li "Drill-down into list and sequence values")
							  (:li "Modify settable slots"))))
	       (:description ,(with-htm "Viewport:" (:ul (:li "Graphical visualization")
							 (:li "Select format from View menu")
							 (:li "x3dom currently needs browser refresh")))
			     :image-url "tasty-wing.png")

	       ))


      (:title "\"Breaking\" on an object"
	      :bullet-points
	      ((:description ,(with-htm "You can use the break icon ("
					((:img :src (format nil "~abreak-icon.png" (the image-base-url))))
					") to set "
					((:span :class "lisp-code") "self")
					" to the next-clicked object"))

	       (:description "The concept of \"break\" is from Olden Times when browser could not work at the same time as command repl.")
	       
	       (:description ,(with-htm "Click on the "
					((:img :src (format nil "~abreak-icon.png" (the image-base-url))))
					" and then on the "
					((:img :src (format nil "~awing-in-tree.png" (the image-base-url))))
					", then you can work with that wing instance as "
					((:span :class "lisp-code") "self")
					" on the command repl:")
			     :examples ((:code (the taper)
					       :return-value 1/2)))))

      (:title ,(with-htm ((:span :class "gdl-section-keyword") ":input-slots"))
	      
	      :bullet-points 
	      ((:description ,(with-htm "You can specify "
					((:span :class "gdl-section-keyword") ":input-slots")
					" to be able to give values to certain slots at the time of an object's birth.")
			     :image-url "uml-2-8.png"
			     :examples ((:define-object gdl-user::wing-with-input)))

	       (:description 
		,(with-htm 
		  (:small 
		   (:i
		    "Example code is "
		    ((:a :href "https://github.com/genworks/gendl/blob/master/documentation/training/g102-tud/examples/source/ch2-examples.lisp"
			 :target "_fresh")
		     "here")
		    ". Press \"Raw\" for raw downloadable form."))))))

      (:title ,(with-htm "Providing Values for " ((:span :class "gdl-section-keyword") ":input-slots")
			 " when an Instance is Born")
	      :bullet-points ((:description ,(with-htm ((:span :class :lisp-code) "make-object")
						       " and "
						       ((:span :class :lisp-code) "make-self")
						       " accept "
						       (:i "optional keyword arguments")
						       " corresponding to the input-slots of the object type being created."))

			      (:description ,(with-htm "What are Keyword Arguments?"
						       (:ul (:li "Keyword symbols are symbols (i.e. Lisp words) preceded by a colon (:)")
							    (:li "Keyword Arguments are keyword-value pairs")
							    (:li "They consist of a keyword symbol (the name) followed by the actual argument value.")
							    (:li "Example: " 
								 ((:span :class "lisp-code") ":b 12")
								 " is a keyword-value pair."))))

			      (:description ,(with-htm "Let's make a " ((:span :class "lisp-code") "wing-with-input")
						       " with a value of " ((:span :class "lisp-code") "12")
						       " for its input-slot " ((:span :class "gdl-message-name") "b"))
					    :examples ((:code (make-self 'wing-with-input :b 12)
							      :return-value ";; self is now set to a wing-with-input object instance.")
						       (:code (the b)
							      :return-value 12)
						       (:code (the S)
							      :return-value 54)
						       (:code (the A)
							      :return-value 8/3)))
			      (:description "Later you will see that input values are passed into Child Objects in the same manner.")))

      
      (:title ,(with-htm ((:span :class "gdl-section-keyword") ":input-slots"))
	      :bullet-points
	      ((:description ,(with-htm 
			       :br
			       ((:span :class "lisp-code") "(")
			       ((:span :class "gdl-message-name") "a")
			       ((:span :class "gdl-comment") " ;; Required input - no default value")
			       :br
			       "&nbsp;&nbsp;" ((:span :class "lisp-code") "(")
			       ((:span :class "gdl-message-name") "b")
			       ((:span :class "lisp-code") ")")
			       ((:span :class "gdl-comment") " ;; optional - default is nil")
			       :br
			       "&nbsp;&nbsp;" ((:span :class "lisp-code") "(")
			       ((:span :class "gdl-message-name") "c 10")
			       ((:span :class "lisp-code") ")")
			       ((:span :class "gdl-comment") " ;; optional - default is 10")
			       :br
			       "&nbsp;&nbsp;" ((:span :class "lisp-code") "(")
			       ((:span :class "gdl-message-name") "d 20 :settable")
			       ((:span :class "lisp-code") "))")
			       ((:span :class "gdl-comment") " ;; optional - default is 20, can be \"bashed\"")))))


      (:title "Wing with more Inputs"

	      :bullet-points
	      ((:description "This wing has two settable input-slots"
			     :image-url "uml-2-9c.png"
			     :examples ((:define-object gdl-user::wing-more-inputs)))

	       (:description 
		,(with-htm 
		  (:small 
		   "Example code is "
		   ((:a :href "https://github.com/genworks/gendl/blob/master/documentation/training/g102-tud/examples/source/ch2-examples.lisp"
			:target "_fresh")
		    "here")
		   ". Press \"Raw\" for raw downloadable form.")))))
      
      (:title "Exercise 1: make a Fuselage object"
	      :bullet-points 
	      ((:description ,(with-htm "Parameters (i.e. Inputs):"
					(:ul (:li "diameter (d)")
					     (:li "length (l)"))))

	       (:description ,(with-htm "Outputs:"
					(:ul (:li "volume (V)")
					     (:li "wetted surface (Sw)")
					     (:li "slenderness-ratio (= l / d)"))))))

      (:title "Exercise 1 Solution"
	      :bullet-points
	      (#+nil
	       (:description "Don't look at this until You Try It:"
			     :image-url "uml-2-10b.png"
			     :examples
			     ((:define-object gdl-user::fuselage)))

	       (:description ,(with-htm (:small 
					 "Solutions code will be available later.")))))

      (:title "Child Objects"
	      :bullet-points  ((:description "UML \"Has-a\" relationship"
					     :image-url "uml-2-12.png")
			       (:description "Skeleton Engine Definition:"
					     :examples ((:define-object gdl-user::engine)))

			       (:description "Wing with a single engine as Child Object:"
					     :examples ((:define-object gdl-user::wing-with-engine)))
							))
      
      (:title "Child Object in a Sequence"
	      :bullet-points  ((:description "Specify keyword :size and an expression returning an integer."
					     :image-url "uml-2-12.png"
					     :examples ((:define-object gdl-user::wing-with-engines)))))


      (:title "Working with Sequences"
	      :bullet-points ((:description ,(with-htm ((:span :class "lisp-code") "list-elements")
						       " converts a sequence into a Lisp list."))

			      (:description "Try these after \"breaking\" on a wing-with-engines:"
					    :examples
					    ((:code (list-elements (the engines))
						    :return-value (engine-x24a47d42 engine-x24a47f9a engine-x24a481c2 engine-x24a483ea))
					     (:code (list-elements (the engines) (the-element Tmax))
						    :return-value (1000 800 900 1200))))

			      (:description "Access an element with direct index referencing"
					    :examples
					    ((:code (the (engines 2))
						    :return-value engine-x24a481c2)
					     (:code (the (engines 2) Tmax)
						    :return-value 900)))

			      (:description ,(with-htm "Using " 
						       ((:span :class "lisp-code") "nth")
						       " with "
						       ((:span :class "lisp-code") "list-elements")
						       " also works, but is less efficient")
					    :examples
					    ((:code (nth 2 (list-elements (the engines)))
						    :return-value engine-x24a481c2)
					     (:code (the-object (nth 2 (list-elements (the engines))) Tmax)
						    :return-value 900)))))

      
      (:title "Lisp background: <i>Plists</i>"
	      :bullet-points ((:description "Property List, a type of <i>map</i>")
			      (:description "This is a list of pairs")
			      (:description "Maps keyword symbols (i.e. names) to values")
			      (:description ,(with-htm "Use " ((:span :class "lisp-code") "getf")
						       " to access a value for a given keyword:")
					    :examples ((:code (setq list (list :a 1 :b 2 :c 3))
							      :return-value (:a 1 :b 2 :c 3))
						       
						       (:code (getf list :b)
							      :return-value 2)
						       (:code (getf list :c)
							      :return-value 3)
						       (:code (getf list :d)
							      :return-value nil)))))
      
      (:title "Exercise 2"
	      :bullet-points ((:description "Define an object representing a fuel tank.")
			      (:description ,(with-htm "Parameters (i.e. " ((:span :class :gdl-section-keyword) "input-slots")
						       ") are:" 
						       (:ul (:li ((:span :class :gdl-message-name) "h"))
							    (:li ((:span :class :gdl-message-name) "w"))
							    (:li ((:span :class :gdl-message-name) "l")))))
			      (:description ,(with-htm "Include a method (i.e. " ((:span :class :gdl-section-keyword) "computed-slot")
						       ") to calculate the "
						       ((:span :class :gdl-message-name) "volume") "."))

			      (:description ,(with-htm "Add a sequence of "
						       ((:span :class :gdl-message-name) "fuel-tanks")
						       " in the wing-with-engines, driven by the following plist:")
					    :examples
					    ((:code (list (list :w 4 :l 3.5 :h 0.3 )
							  (list :w 2 :l 2.5 :h 0.2 )
							  (list :w 1.5 :l 2 :h 0.15)))))))
      (:title "Solution, Part 1"
	      :bullet-points ((:description "First, our fuel tank definition (code will be provided later):"
					    :image-url "uml-sol2.png"
					    
					    ;;:examples
					    #+nil
					    ((:define-object gdl-user::fuel-tank))
					    )))

      (:title "Solution, Part 2"
	      :bullet-points ((:description "Then, our wing definition (code will be provided later):"
					    :image-url "uml-sol2-contd.png"
					    ;;:examples
					    #+nil
					    ((:define-object gdl-user::wing-with-tanks)))
			      (:description "Yes, there is a shorter way to do the inputs for the fuel-tank object (but do it the long way for now).")))

      (:title "How it might look in Tasty"
	      :bullet-points ((:description "Something like this:"
					    :image-url "wing-tanks-tasty.png")))

      
      (:title "GDL Functions"
	      :bullet-points 
	      ((:description "All the fun of Lisp Functions and GDL Objects, rolled up into one!")
	       (:description "Aircraft assembly with friction force function:"
			     :image-url "uml-2-15.png"
			     :examples
			     ((:define-object gdl-user::aircraft)))))
      
      (:title "Calling GDL Functions"
	      :bullet-points 
	      ((:description "With make-object on command repl:"
			     :examples
			     ((:code (setq obj (make-object 'aircraft)))
			      (:code (the-object obj (compute-friction-force 50))
				     :return-value 5903.793955680823)
			      (:code (the-object obj (compute-friction-force 150))
				     :return-value 53134.145601127406)))
	       (:description "After break/set-self in tasty")
	       (:description "Procedurally from within another function")))


      (:title "Exercise 3"
	      :bullet-points 
	      ((:description ,(with-htm "Add a function " ((:span :class :gdl-message-name) "lift")
					" to the aircraft class."))

	       (:description ,(with-htm "Your function should accept "
					((:span :class :lisp-code) "speed")
					" and "
					((:span :class :lisp-code) "C-of-L")
					" as its arguments, and should yield the value of "
					((:span :class :gdl-message-name) "lift") "."))

	       (:description ,(with-htm "Test the function for the following values of C-of-L:" 
					((:span :class :lisp-code) "(0.15 0.18 0.20 0.22 0.25)")))
	       (:description "Compare with your neighbor.")))

      (:title "Solution" 
	      :bullet-points 
	      ((:description "Note that this could also mix-in the original aircraft, and avoid the repeated code (code will be provided later)."
			     :image-url "uml-2-17.png"
			     ;;:examples ((:define-object gdl-user::aircraft-with-lift))
			     )))

      (:title "Today's Cumulative UML object Tree" 
	      :bullet-points 
	      ((:description ,(with-htm ((:span :class :lisp-code) "My-B52") " can mix-in and extend "
					((:span :class :lisp-code) "aircraft-with-lift") ".")
			     :image-url "final-uml.png")))))))


      





      
		     


      


               
