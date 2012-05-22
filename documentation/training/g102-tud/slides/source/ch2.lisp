;;
;; Copyright 2012 Genworks International and the Delft University of
;; Technology
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GenDL).
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
  ((title "G102 Supplement: GenDL Quickstart for TU Delft")
   (slide-package (find-package :training-g102-tud))
   (image-base-url "/g102-tud/images/")
   (images-path *images-path*)
   (style-url "/static/gwl/style/top.css"))

  
  :objects
  (
   (objects :type 'objects)
   

   ))



(define-object objects (slide-show-leaf)

  :computed-slots
  ((strings-for-display "Aero Object Examples")
   
   (slide-data 
    `((:title "Empty Surface Mixin"
	      :bullet-points
	      ((:description "Define an Empty Surface - will use real built-in surface later"
			     :examples
			     ((:define-object gdl-user::empty-surface)))

	       
	       (:description ,(with-cl-who-string 
			       ()
			       ((:span :class "gdl-operator") "define-object") " - main operator used in GenDL to create definitions."))

	       (:description ,(with-cl-who-string 
			       ()
			       ((:span :class "gdl-object-def") "empty-surface") " - Name of our new definition."))

	       (:description ,(with-cl-who-string
			       ()
			       ((:span :class "gdl-object-def") "base-object") " - Name of single mixin (should be pre-defined)."))))

      (:title "Basic Wing Skeleton"
	      :bullet-points 
	      ((:description "Define a wing with initial slots, mixes in empty surface to start with"
			     :examples
			     ((:define-object gdl-user::wing)))
	       (:description ,(with-cl-who-string 
			       ()
			       ((:span :class "gdl-object-def") "wing") " - our new definition name."))
	       (:description ,(with-cl-who-string
			       ()
			       ((:span :class "gdl-object-def") "empty-surface") " - Name of single mixin (which we already defined)."))
	       (:description ,(with-cl-who-string
			       ()
			       ((:span :class "gdl-section-keyword") ":computed-slots") " - constant and computed values which can be 
\"answered\" by instances of this definition."))))

      
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

	       (:description "So there is heavy use of \"the\" inside most object definitions:"
			     :examples
			     ((:define-object gdl-user::wing)))))


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
			     :examples ((:define-object gdl-user::wing-with-input)))))


      (:title ,(with-htm ((:span :class "gdl-section-keyword") ":input-slots"))
	      :bullet-points
	      ((:description ,(with-htm 
			       :br
			       ((:span :class "lisp-code") "(")
			       ((:span :class "gdl-message-name") "a")
			       ((:span :class "gdl-comment") " ;; Required input - no default value")
			       :br
			       ((:span :class "lisp-code") "(")
			       ((:span :class "gdl-message-name") "b")
			       ((:span :class "lisp-code") ")")
			       ((:span :class "gdl-comment") " ;; optional - default is nil")
			       :br
			       ((:span :class "lisp-code") "(")
			       ((:span :class "gdl-message-name") "c 10")
			       ((:span :class "lisp-code") ")")
			       ((:span :class "gdl-comment") " ;; optional - default is 10")
			       :br
			       ((:span :class "lisp-code") "(")
			       ((:span :class "gdl-message-name") "d 20 :settable")
			       ((:span :class "lisp-code") "))")
			       ((:span :class "gdl-comment") " ;; optional - default is 20, can be \"bashed\"")))))


      (:title "Wing with more Inputs"

	      :bullet-points
	      ((:description "This wing has two settable input-slots"
			     :image-url "uml-2-9c.png"
			     :examples ((:define-object gdl-user::wing-more-inputs)))))
      
      
      (:title "Open it in Tasty"
	      :bullet-points 
	      ((:description "Visit http://localhost:9000/tasty")
	       (:description ,(with-htm "Specify " ((:span :class "lisp-code") "wing-more-inputs") " as the Class Type")
			     :image-url "wing-more-inputs-tasty.png")))

      (:title "Exercise 1: make a Fuselage object"
	      :bullet-points 
	      ((:description ,(with-htm "Parameters (i.e. Inputs):"
					(:ul (:li "diameter (d)")
					     (:li "length (l)"))))

	       (:description ,(with-htm "Outputs:"
					(:ul (:li "volume (V)")
					     (:li "wetted surface (Sw)")
					     (:li "slenderness-ratio (= l / d)"))))))

      (:title "How it should look in Tasty"
	      :bullet-points 
	      ((:description "Like this:"
			     :image-url "fuselage-tasty.png")))

      
      (:title "Exercise 1 Solution"
	      :bullet-points
	      ((:description "Don't look at this until You Try It:"
			     :image-url "uml-2-10b.png"
			     :examples
			     ((:define-object gdl-user::fuselage)))))

      
      (:title "Child Object in a Sequence"
	      :bullet-points  ((:description "UML \"Has-a\" relationship"
					     :image-url "uml-2-12.png"
					     :examples ((:define-objet gdl-user::engine)
							(:define-object gdl-user::wing-with-engines)))))

					     
			       
      (:title "How it might look in Tasty"
	      :bullet-points ((:description "Something like this:"
					    :image-url "wing-with-engines-tasty.png")))

      
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
	      :bullet-points ((:description "Property Lisp, a type of <i>map</i>")
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
	      :bullet-points ((:description "First, our fuel tank definition:"
					    :image-url "uml-sol2.png"a
					    :examples
					    ((:define-object gdl-user::fuel-tank)))))

      (:title "Solution, Part 2"
	      :bullet-points ((:description "Then, our wing definition:"
					    :image-url "uml-sol2-contd.png"
					    :examples
					    ((:define-object gdl-user::wing-with-tanks)))
			      (:description "Yes, there is a shorter way to do the inputs for the fuel-tank object (but do it the long way for now).")))

      (:title "How it might look in Tasty"
	      :bullet-points ((:description "Something like this:"
					    :image-url "wing-tanks-tasty.png")))

      
      (:title "GenDL Functions"
	      :bullet-points 
	      ((:description "All the fun of Lisp Functions and GenDL Objects, rolled up into one!")
	       (:description "Aircraft assembly with friction force function:"
			     :image-url "uml-2-15.png"
			     :examples
			     ((:define-object gdl-user::aircraft)))))
      
      (:title "Calling GenDL Functions"
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
	      ((:description "Note that this could also mix-in the original aircraft, and avoid the repeated code"
			     :image-url "uml-2-17.png"
			     :examples ((:define-object gdl-user::aircraft-with-lift)))))

      (:title "Today's Cumulative UML object Tree" 
	      :bullet-points 
	      ((:description ,(with-htm ((:span :class :lisp-code) "My-B52") " can mix-in and extend "
					((:span :class :lisp-code) "aircraft-with-lift") ".")
			     :image-url "final-uml.png")))))))


      





      
		     


      


               
