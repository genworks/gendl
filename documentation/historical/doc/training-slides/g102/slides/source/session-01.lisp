(in-package :training-g102)

(define-object session-01 (slide-show-leaf)
  
  :computed-slots
  ((strings-for-display "Introduction")
   
   (slide-data `((:title "Goals" :bullet-points
			 ((:description "Get you <b>excited</b> about using General-purpose Declarative Language (GDL)")
			  (:description "Qualify all of you to read and develop GDL applications")
			  (:description "Enable you to Judge Whether GDL is an Appropriate Tool for a Given Job.")
			  (:description "Arm you with the ability to Argue the Case for using GDL when
it is the appropriate tool for the job")))

		 (:title "Topics Covered in G102" :bullet-points
		  ((:description "The define-object macro, Overview") 
		   (:description "define-object syntax")
		   (:description "Functional and Declarative Programming Style")
		   (:description "When to use objects and hidden-objects")
		   (:description "When to use object sequences and matrices")
		   (:description ,(with-output-to-string (ss)
				    (html-stream 
				     ss
				     "Basic Overview of Geometric Objects"
				     (:ul (:li "Wireframe")
					  (:li "Surfaces")
					  (:li "Solids"))
				     "And when their use is appropriate")))
		   (:description "Performance Issues")))
		 (:title "Topics not Covered in G102" :bullet-points
		  ((:description "Symbolic Positioning and Orienting of Geometry")
		   (:description "GWL Web User Interface")
		   (:description "Integrations with Specific CAD Systems")))
		 (:title "What is GDL?" :bullet-points
		  ((:description "A Dynamic, Declarative, Object-oriented 
Computational Language embedded in Common Lisp")
		   (:description "A Superset of Common Lisp (i.e. it gives you the entire 
capabilities of CL, while allowing you to work 
at a much higher level of abstraction)")
		   (:description "A technology which allows the engineer to organize 
engineering problems and their solutions
according to a structure which naturally parallels the problem domain")
		   (:description "A good way to take advantage of modern advances 
in computer processor speed and memory capacity, while freeing 
the developer from mundane tasks normally associated with 
computer programming")))
		 (:title "Dynamic, Declarative, Object-oriented" 
			 :bullet-points
			 ((:description "<i>Dynamic</i> refers to the ability to redefine objects 
or change input data and see the results immediately, without restarting 
a program from scratch. This <i><b>greatly</b></i> speeds development
when compared with traditional programming environments such as C and C++.")
			  (:description "<i>Declarative</i> refers to the ability to specify 
units of computation (i.e. ``rules'') in a <b>totally order-independent manner</b>. 
This becomes <b><i>very</i></b> important as models become large and complex")
			  (:description 
			   ,(with-output-to-string (ss)
			      (html-stream 
			       ss
			       (:i "Object-oriented")
			       " refers to all the characteristics 
normally associated with object-oriented programming, including:"
			       (:ul
				(:li "separation between the definition of an object and an instance")
				(:li "high levels of data abstraction")
				(:li "the ability for one object to inherit from others"))
			       "GDL supports both the "
			       (:i "message-passing")
			       " and "
			       (:i "generic function")
			       " paradigms of object orientation (more on this later)")))))
		 (:title "Why GDL? (i.e. What is it Good For)" 
			 :bullet-points
		  ((:description "Organizing and interrelating large amounts of information
from different types of data sources")
		   (:description "Evaluating many design alternatives and performing optimizations 
within specified design spaces")
		   (:description "Capturing the procedures and rules used to solve repetitive
tasks in engineering and other fields")
		   (:description "Applying these rules to achieve intermediate and final 
outputs, which may include virtual models of wireframe, surface, and solid geometric objects")))
		 (:title "What is GDL not?" 
			 :bullet-points
			 ((:description "A drawing program")
			  (:description "An Artificial Intelligence 
system (although it is an excellent environment for developing 
capabilities which could be considered as such)")
			  (:description "An Expert System Shell (although one could be easily embedded within it)")))
		 (:title "Why not GDL? (i.e. What is it Not Good For?)" 
			 :bullet-points
			 ((:description "Creating brand new parts with artistic content and free-form geometry")
			  (:description "Perform traditional CAE tasks such as finite-element analysis and CFD
 (although GDL is an excellent environment for pre- and post-processing
of data for such solvers, and managing batch runs of such solvers) ")))))))

