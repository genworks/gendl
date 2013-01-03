(in-package :training-g102)

(define-object session-02 (slide-show-leaf)

  :computed-slots
  ((strings-for-display "The define-object macro")
   
   (slide-data `((:title "Syntax of the define-object macro" :bullet-points
			 ((:description "definition (i.e. blueprint) of an object")
		  (:description "Can represent a physical part or assembly or any grouping of computational elements")
			  (:description 
			   ,(with-output-to-string (ss)
			      (html-stream ss
					   (:i "input-slots") ", " (:i "computed-slots") ", " (:i "objects"))))
			  (:description "Dynamic, Demand-Driven evaluation of objects and messages (order-independent)")
			  (:description "Graphical, web-based testing and tracking utility (TaTU)")
			  (:description "Edited and compiled in much the same fashion as normal CL code")))
		 (:title "defun vs. define-object" 
			 :bullet-points
			 ((:description
			   ,(with-output-to-string (ss)
			      (html-stream ss "Purpose in Life"
					   ((:table :border 1)
					    (:tr ((:th :width 0.5 :background "#BBDDFF")  "defun")
						 ((:th :width 0.5 :background "#BBFFDD") "define-object"))
					    (:tr
					     ((:td :background "#CCEEFF")
					      "Accept some " (:i "Arguments") 
					      ", possibly perform some " 
					      (:i "Side-effects") 
					      ", and compute one or more "
					      (:i "Return-values"))
					     ((:td :background "#CCEEFF")
					      "Accept some " 
					      (:i "input-slots")
					      ", possibly perform some " 
					      (:i "side-effects") 
					      ", and compute one or more "
					      (:i "Outputs.")))))) :suppress-end-dot? t)
			  (:description
			   ,(with-output-to-string (ss)
			      (html-stream ss "How to Use"
					   ((:table :border 1)
					    (:tr ((:th :width 0.5 :background "#BBDDFF")  "defun")
						 ((:th :width 0.5 :background "#BBDDFF")  "define-object"))
					    (:tr
					     ((:td :background "#CCEEFF")
					       "Generally you call it by name, by evaluating a Lisp expression")
					     ((:td :background "#CCEEFF")
					      (:ol (:li "Create an " (:i "Instance"))
						   (:li "Send " (:i "Messages") " to the Instance")))))))
			   :suppress-end-dot? t)
			  (:description
			   ,(with-output-to-string (ss)
			      (html-stream ss "How to Define"
					   ((:table :border 1)
					    (:tr ((:th :width 0.5 :background "#BBDDFF")  "defun")
						 ((:th :width 0.5 :background "#BBDDFF")  "define-object"))
					    (:tr
					     ((:td :background "#CCEEFF")
					      (:code
					       (:pre
						(:b "defun <i>name</i> <i>argument-list</i> <i>body</i>"))))
					     ((:td :background "#CCEEFF")
					      (:code
					       (:pre
						(:b "define-object <i>name</i> <i>mixin-list</i> <i>spec-plist</i>"))))))))
			   :suppress-end-dot? t)
			  (:description
			   ,(with-output-to-string (ss)
			      (html-stream ss "How to Modularize"
					   ((:table :border 1)
					    (:tr ((:th :width 0.5 :background "#BBDDFF")  "defun")
						 ((:th :width 0.5 :background "#BBDDFF")  "define-object"))
					    (:tr
					     ((:td :background "#CCEEFF")
					      (:code
					       (:pre
						(:b "Call other functions from within a function"))))
					     ((:td :background "#CCEEFF")
					      (:ol (:li "<i>Inherit</i> <i>slots</i> from other objects using <i>mixins</i>")
						   (:li "Include <i>instances</i> of other objects inside an object")))))))
			   :suppress-end-dot? t)))
		 (:title "Defun vs. Define-object, Cont'd" :bullet-points
			 ((:description
			   ,(with-output-to-string (ss)
			      (html-stream 
			       ss "Advantages"
			       ((:table :border 1)
				(:tr ((:th :width 0.5 :background "#BBDDFF")  "defun")
				     ((:th :width 0.5 :background "#BBDDFF")  "define-object"))
				(:tr
				 ((:td :background "#CCEEFF")
				  (:ol (:li "Works with Any Lisp (Including free implementations and Emacs Lisp!)")
				       (:li "Dynamic") (:li "Good for small, simple computations")))
				 ((:td :background "#CCEEFF")
				  (:ol (:li "Declarative") (:li "Dynamic") (:li "No Need for Procedural Debugging")
				       (:li "Compile-Test-Debug cycle is <i>very</i> fast (important!)")
				       (:li "Built-in Geometric Primitives, output formats, and CAD Connections")))))))
			   :suppress-end-dot? t)
			  (:description
			   ,(with-output-to-string (ss)
			      (html-stream 
			       ss "Disadvantages"
			       ((:table :border 1)
				(:tr ((:th :width 0.5 :background "#BBDDFF")  "defun")
				     ((:th :width 0.5 :background "#BBDDFF")  "define-object"))
				(:tr
				 ((:td :background "#CCEEFF")
				  (:ol (:li "Procedural (i.e. a Lisp program has a ``begin'' and ``end'')")
				       (:li "Sometimes Difficult to Debug when Long")
				       (:li "Compile-Test-Debug cycle much faster than C, but you still 
must run through procedural code")))
				 ((:td :background "#CCEEFF")
				  (:ol (:li "Requires GDL package and license")
				       (:li
					"Can be more resource-intensive (RAM, swap space, disk space) -- tradeoff between space and time")))))))
			   :suppress-end-dot? t)))
		 (:title "Basic define-object Syntax" :bullet-points
			 ((:description "<i>Define-object class-name mixin-list spec-plist</i>")
			  (:description "<i>class-name</i> is a symbol, just like a Defun name")
			  (:description "<i>Mixin-list</i> is a list of other <i>class-names</i>
from which this object definition (i.e. class) <i>inherits</i> characteristics")
			  (:description "The rest of the body of the define-object consists of a
<i>Spec-plist</i>, made up of special keywords and lists, which specify the 
computational ``blueprint'' for this object")))
		 (:title "Basic Define-object Syntax, Cont'd: <i>:slots</i>" 
			 :bullet-points
			 ((:description "Almost every Spec-plist you see will have an <i>:input-slots</i>
and/or <i>:computed-slots</i> section. Slots are symbols, whose names you specify, 
which represent intermediate and/or final <i>outputs</i> to be computed by an object." 
					:examples
					((:define-object silly-object)))
			  (:description "As seen in the above example, each slot consists of a 
<i>name</i> (i.e. a symbol specified by you), and an <i>expression</i> which can
be any valid CL expression")
			  (:description "Slots can refer to the value of other slots in the 
same object using the macro <i>the</i>")
			  (:description "When we talk about the <i>value</i> of a slot, we are
referring to the <i>return-value</i> of its expression, in a particular <i>object instance</i>")))
		 (:title "<i>Instances</i> and <i>Sending Messages</i>" 
			 :bullet-points
			 ((:description "You can make an <i>instance</i> of a GDL object with the
function <i>make-object</i>:" 
					:examples 
					((:code (make-object 'silly-object) 
						:return-value* "<silly-object 26112>")))
			  (:description "You can send <i>messages</i> to a define-object instance
 (e.g. to obtain <i>slot</i> values of the object) with 
the macro <i>the-object</i>:" 
					:examples
					((:code (setq obj (make-object 'silly-object)) :return-value* "<silly-object 26113>")
					 (:code (the-object obj b) :return-value 2500)))
			  (:description "If you only need to send one message, you can do it
 all in one step:" :examples ((:code (the-object (make-object 'silly-object) b) :return-value 2500)))))
		 (:title "Specifying a define-object with input-slots" :bullet-points
			 ((:description "Required Input-slots work similarly to Keyword Arguments to a defun, 
but they are required (i.e. not optional) and have no default 
 (i.e. their values must be specified upon object instantiation)" 
					:examples
					((:define-object silly-object-2)
					 (:define-object hello-there)))))
		 (:title "Specifying <i>input-slots</i> to an <i>object Instance</i>" 
			 :bullet-points
			 ((:description "In order for an object instance to 
behave as expected, all required <i>input-slots</i> must be supplied at the
time of instantiation:" 
					:examples
					((:code (setq obj (make-object 'hello-there :name "Jake" :fancy? nil)) 
						:return-value* "<hello-there 26120>")
					 (:code (the-object obj greeting) :return-value "Hello Jake")
					 (:code (setq obj (make-object 'hello-there :name "Jake" :fancy? t)) :return-value
						"<hello-there 26120>")
					 (:code (the-object obj :greeting) :return-value "Hello there Jake, How Are You?")))))
		 (:title "Specifying <i>input-slots</i> to  an object Instance as a plist" 
			 :bullet-points ((:description "Since the inputs to a define-object form a plist, you 
can specify them as such using <i>Apply</i>:" 
						       :examples
						       ((:code (setq myparms (list :name "Jackson" :fancy? t)) 
							       :return-value (:name "Jackson" :fancy? t))
							(:code (setq obj (apply #'make-object 'hello-there myparms)) 
							       :return-value* "<hello-there 26120>")
							(:code (the-object obj :greeting) 
							       :return-value "Hello there Jackson, How Are You?")))))
		 (:title "Using a Defun as a wrapper for demanding outputs from an Object" 
			 :bullet-points
			 ((:description "Often, it is convenient to be able to 
call a function, while the actual work is done by an object instance:" 
					:examples
					((:code
					  (defun print-greeting (&key (name "Jake") (fancy? t))
					    (format t "~a" 
						    (the-object (make-object 'hello-there :name name :fancy? fancy?) greeting)))
					  :return-value print-greeting)
					 (:code (print-greeting) :print-string "Hello there Jake, How Are You?" :return-value nil)
					 (:code (print-greeting :name "Jackson") 
						:print-string "Hello there Jackson, How Are You?" :return-value nil)))))
		 (:title "The Special Variable <i>Self</i>" :bullet-points
			 ((:description "<i>(The ...)</i> is equivalent to <i>(The-object self ...)</i>")
			  (:description "Within an object instance, the local variable <i>self</i> is
automatically set to that instance" 
					:examples
					((:code (setq obj (make-object 'silly-object)) :return-value "<silly-object 57390>")
					 (:code (the-object obj b) :return-value 100)
					 (:code (the-object obj c) :return-value 100)))
			  (:description "You can also <i>setq</i> or <i>let</i> an object instance to 
<i>self</i> at the toplevel or inside a defun, and send 
messages using <i>the</i> instead of <i>the-object</i>:" 
					:examples
					((:code (let ((self (make-object 'silly-object))) (the b)) :return-value 100)))
			  (:description "The ``<i>self</i>'' inside a GDL object instance is protected from
any other <i>self</i> bindings, so any ``<i>the</i>'' references inside
the object definition will always work properly")))
		 (:title "Exercises" :bullet-points
			 ((:description "Write a define-object <i>silly-adder</i>, which has two
input-slots, <b>a</b> and <b>b</b>, and has computed-slot <b>sum</b> which is the sum of <b>a</b> and <b>b</b>")
			  
			  (:description "
Test your object in four ways:
<ol>
<li>
<i>Setq</i> a variable to an instance at the top-level 
 (using <i>make-object</i>), then use <i>the-object</i> to send
the <b>sum</b> message to your instance.
</li>
<li>
Do it all in one nested statement, without <i>setq</i>ing any variables.
</li>
<li>
Create a wrapper function which <i>let</i>s a variable to an instance
of your object, then uses <i>the-object</i> within the body of the let.
</li>
<li>
Have the wrapper function <i>let</i> the local variable <i>self</i>
to an instance of the object, then use <i>the</i> within the body of
the let.
</li>
</ol>" 
					:suppress-end-dot? t)))))))

