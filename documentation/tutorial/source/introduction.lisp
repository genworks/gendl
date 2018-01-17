;;
;; Copyright 2002, 2009, 2012 Genworks International
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

(in-package :gendl-doc)

(defparameter *introduction*
    `((:chapter :title "Introduction")

      ((:section :title "Welcome")
       "Congratulations on your decision to work with Genworks\\textsuperscript{\\textregistered} GDL\\texttrademark"
       (:footnote "From time to time, you will also see references to
``Gendl.'' This refers to ``The Gendl Project'' which is the name of
an open-source software project from which Genworks GDL draws for its
core technology. ``The Gendl Project'' code is free to use for any
purpose, but it is released under the Gnu Affero General Public
License, which stipulates that applications code compiled with The
Gendl Project compiler must be distributed as open-source under a
compatible license (if distributed at all). Commercial Genworks GDL,
properly licensed for development and/or runtime distribution, does
not have this ``copyleft'' open-sourcing requirement.")
       ". By investing time to learn this system you will be investing
in your future productivity and, in the process, you will be joining a
quiet revolution. Although you may have come to Genworks GDL because
of an interest in 3D modeling or mechanical engineering, you will find
that a whole new world, and a unique approach to "
       (:emph "computing")
       ", will now be at your fingertips as well.")
      
      ((:section :title "Knowledge Base Concepts According to Genworks")
       "You may have an idea about Knowledge Base Systems,
or Knowledge "
       (:emph "Based")
       " Systems, from college textbooks or corporate marketing
literature, and concluded that the concepts were too broad to be of
practical use. Or you may have heard criticisms implicit in the
pretentious-sounding name, ``Knowledge-based Engineering,'' as in:
``you mean as opposed to "
       (:indexed "Ignorance-based Engineering")
       "?'' 

To provide a clearer picture, we hope you will concur that Genworks'
concept of a KB system is straightforward, relatively uncomplicated,
and practical. In this manual our goal is to make you both comfortable
and motivated to explore the ideas we have built into our flagship
system, Genworks GDL.

Our informal definition of a "
       (:emph (:indexed "Knowledge Base System"))
       " is a hybrid " 
       (:emph "Object-Oriented") 
       (:footnote "An " (:emph "Object-Oriented")
		  " programming environment supports named collections
		  of values along with procedures to operate on those
		  values, including the possibility to
		  modify (``mutate'') the data. See
		  https://en.wikipedia.org/wiki/Object-oriented_programming")
       " and "
       (:emph "Functional")
       (:footnote "A pure " (:emph "Functional")
		  " programming environment supports only the
evaluation of Functions which work by computing results, but do not
modify (i.e. ``mutate'') the in-memory state of any objects. See
http://en.wikipedia.org/wiki/Functional_programming")
       " programming environment, which implements the features of "
       (:emph (:indexed "Caching"))
       " and "
       (:emph (:indexed "Dependency tracking"))
       ". Caching means that once the KB system has computed
something, it generally will not need to repeat that computation if
the same question is asked again. Dependency tracking is the flip side
of that coin --- it ensures that if a cached result is "
       (:emph "stale")
       ", the result will be recomputed the next time it is "
       (:emph "demanded")
       ", in order to give a fresh result.")

      ((:section :title "Classic Definition of Knowledge Based Engineering (KBE)")
       "Sections "
       (:ref "sec:classicdefinitionofknowledgebasedengineering(kbe)")
       " through "
       (:ref "sec:object-orienteddesign")
       " are sourced from "
       (:cite "LaRocca")
       "."
       (:quote
       "Knowlege based engineering (KBE) is a technology predicated on
the use of dedicated software tools called KBE systems, which are able
to capture and systematically re-use product and process engineering
knowledge, with the final goal of reducing the time and costs of
product development by means of the following:"
       (:ul (:li "Automation of repetitive and non-creative design tasks;")
	    (:li "Support of multidisciplinary design optimization in all  
phases of the design process"))))


      ((:section :title "Runtime Value Caching and Dependency Tracking")
       (:quote
	(:p "Caching refers to the ability of the KBE system to memorize at
runtime the results of computed values (e.g. computed slots and
instantiated objects), so that they can be reused when required,
without the need to re-compute them again and again, unless necessary.
The dependency tracking mechanism serves to keep track of the current
validity of the cached values.  As soon as these values are no longer
valid (stale), they are set to unbound and recomputed if and only at
the very moment they are again demanded.")

	(:p "This dependency tracking mechanism is at the base of
associative modeling, which is of extreme interest for engineering
design applications. For example, the shape of a wing rib can be
defined according to the shape of the wing aerodynamic surface. In
case the latter is modified, the dependency tracking mechanism will
notify the system that the given rib instance is no longer valid and
will be eliminated from the product tree, together with all the
information (objects and attributes) depending on it. The new rib
object, including its attributes and the rest of the affected
information, will not be re-instantiated/updated/re-evaluated
automatically, but only when and if needed (see demand driven
instantiation in the next section)")))

      ((:section :title "Demand-Driven Evaluation")
       (:quote
	"KBE systems use the "
	(:emph "demand-driven")
	" approach. That is, they evaluate only those chains of
expressions required to satisfy a direct request of the user (i.e. the
evaluation of certain attributes for the instantiation of an object),
or the indirect requests of another object, which is trying to satisfy
a user demand. For example, the system will create an instance of the
rib object only when the weight of the abovementioned wing rib is
required. The reference wing surface will be generated only when the
generation of the rib object is required, and so on, until all the
information required to respond to the user request will be made
available.

It should be recognized that a typical object tree can be structured
in hundreds of branches and include thousands of attributes. Hence,
the ability to evaluate "
	(:emph "specific")
	" attributes and product model branches at demand, without the
need to evaluate the entire model from its root, prevents waste of
computational resources and in many cases brings seemingly intractible
problems to a rapid solution."))

      ((:section :title "Object-oriented Systems")
       (:quote "An object-oriented system is composed of
       objects (i.e. concrete instantiations of "
	       (:emph "named")
	       " classes), and the behavior of the system results from
       the collaboration of those objects. Collaboration between
       objects involves them sending messages to each other. Sending a
       message differs from calling a function in the sense that when
       a target object receives a message, it decides on its own what
       function to carry out to service that message. The same message
       may be implemented by many different functions, the one
       selected depending on the current state of the target
       object."))
      
      ((:section :title "Object-oriented Analysis")
       (:quote
	(:p "Object-oriented analysis (OOA) is the process of analyzing
       a task (also known as a problem domain) to develop a conceptual
       model that can then be used to complete that task. A typical OOA
       model would describe computer software that could be used to
       satisfy a set of customer-defined requirements. During the
       analysis phase of problem-solving, the analyst might consider a
       Written Requirements Statement, a formal vision document, or
       interviews with stakeholders or other interested parties. The
       task to be addressed might be divided into several subtasks (or
       domains), each representing a different business,
       technological, or other area of interest. Each subtask would be
       analyzed separately. Implementation
       constraints (e.g. concurrency, distribution, persistence, or
       how the system is to be built) are not considered during the
       analysis phase; rather, they are addressed during
       the object-oriented design (OOD) phase.")

	(:p "The conceptual model that results from OOA will typically consist of a
set of use cases, one or more UML class diagrams, and a number of
interaction diagrams. It may also include some form of user interface.")))

      ((:section :title "Object-oriented Design")
       (:quote 
	"During the object-oriented design (OOD) phase, a developer
applies implementation constraints to the conceptual model produced in
the object-oriented analysis. Such constraints could include not only
those imposed by the chosen architecture but also any non-functional
--- technological or environmental --- constraints, such as data
processing capacity, response time, run-time platform, development
environment, or those inherent in the programming language. Concepts
in the analysis model are mapped onto implementation classes and
interfaces resulting in a model of the solution domain, i.e., a
detailed description of "
	(:emph "how") 
	" the system is to be built."))

      ((:section :title "The Object-Oriented Paradigm meets the Functional paradigm")
       "In order to model very complex products and efficiently manage
large bodies of knowledge, KBE systems tap the potential of the object
oriented nature of their underlying language (e.g. Common
Lisp). ``Object'' in this context refers to an instantiated data
structure "
       (:emph "of a particular assigned data type")
       ". As is well-known in the computing community, unrestricted
modification of the state of objects leads to unmaintainable systems
which are difficult to debug. KBE systems manage this drawback by
strictly controlling and constraining any ability to modify or
``change state'' of objects.

In essence, a KBE system generates a tree of inspectable objects which
is analogous to the function call tree of pure functional-language
systems.")

      ((:section :title "Goals for this Manual")
       "This manual is designed as a companion to a live two-hour
GDL/GWL tutorial, but you may also be relying on it independently of
the tutorial. Portions of the live tutorial are available in
``screencast'' video form, in the Documentation section of "
       (:texttt "http://genworks.com")
       " In any case, our fundamental goals of this Manual are:"
       ((:list :style :itemize)
	(:item "To get you motivated about using Genworks GDL")
	(:item "Enable you to ascertain whether Genworks GDL is an appropriate tool for a given job")
	(:item "Equip you with the ability to state the case for using GDL/GWL when appropriate")
	(:item "Prepare you to begin authoring and maintaining GDL
applications, or porting apps from similar KB systems into GDL."))
       
       "The manual will begin with an introduction to the "
       (:indexed "Common Lisp")
       " programming language. If you are new to Common Lisp: welcome!
You are about to be introduced to a powerful tool backed by a
rock-solid body of standard specifications, which will protect your
development investment for decades to come. In addition to the
overview provided in this manual, many resources are available to get
you started in CL --- for starters, we recommend "
       (:underline (:indexed "Basic Lisp Techniques") )
       (:footnote (:underline "BLT")
		  " is available at "
		  (:texttt "http://www.franz.com/resources/educational_resources/cooper.book.pdf"))
       ", which was written by the author. ")
      
      ((:section :title "What is GDL?")
       "GDL is an acronym for
``General-purpose Declarative Language.''"
       (:ul

	(:li "In a nutshell, GDL is a system for creating custom,
	targeted CAD systems and other applications. By ``targeted,''
	we mean these end-user CAD systems and applications are
	designed to complete a specific, rule-based set of design
	tasks, as distinct from a general-purpose CAD system which
	works as an electronic drafting board or interactive 3D
	modeling system.")
	(:li
	 "GDL is a superset of ANSI Common Lisp, and consists largely of
automatic code-expanding extensions to Common Lisp implemented in the
form of macros. When you write, for example, 20 lines in GDL, you
might be writing the equivalent of 200 lines of Common Lisp. Given
that GDL is a superset of Common Lisp, you of course retain the
full power of the CL language at your disposal whenever you are
working in GDL.")

       (:index "compiled language!benefits of")
       (:index "macros!code-expanding")
       
       (:li
	"Since GDL expands into CL, everything you write in GDL will
be compiled ``down to the metal'' to machine code with all the
optimizations and safety that the tested-and-true CL compiler provides
[this is an important distinction from some other so-called KB systems
on the market, which are essentially nothing more than interpreted "
	(:emph "scripting languages") 
	" which often impose arbitrary limits on
the size and complexity of the application].")

       (:li "GDL is also a "
	    (:emph (:indexed "declarative"))
	    " language in the fullest sense. When you put together a
GDL application, you think and write mainly in terms of "
	    (:emph "objects")
	    " and their properties, and how they depend on one another
in a direct sense. You do not have to track in your mind explicitly
how one object or property will ``call'' another object or propery, in
what order this will happen, and so forth. Those details are managed
automatically by the embedded language.")

       (:li "Because GDL is object-oriented, you have all the features you would normally expect
from an object-oriented language, such as "
       ((:list :style :itemize)
	(:item "Separation between the " (:emph "definition")
	       " of an object and an " (:emph "instance") " of an object")
	(:item "High levels of data abstraction")
	(:item "The ability for one object to ``inherit'' from others")
	(:item "The ability to ``use'' an object without concern for
	its ``under-the-hood'' complexities"))
       
       (:index "object-orientation!message-passing")
       (:index "object-orientation!generic-function"))

       (:li "GDL supports the ``message-passing'' paradigm of object
orientation, with some extensions. Since full-blown ANSI CLOS (Common
Lisp Object System) is always available as well, you are free to use
the Generic Function paradigm. Do not be concerned at this point if
you are not fully conversant with the differences between Message
Passing and Generic Function models of object-orientation."
       (:footnote "See Paul Graham's "
		  (:underline "ANSI Common Lisp")
		  ", page 192, for an excellent discussion of the Two Models 
of Object-oriented Programming. Peter Siebel's "
		  (:underline "Practical Common Lisp")
		  " also covers the topic; see http://www.gigamonkeys.com/book/object-reorientation-generic-functions.html.")
       ".")))
      
      ((:section :title "Why GDL (i.e., what is GDL good for?)")
       ((:list :style :itemize)
	(:item "Organizing and integrating large amounts of
information in ways which are impossible or impractical using
conventional languages, CAD systems, and/or database technology
alone;")
	(:item "Evaluating many design or engineering alternatives and 
performing various kinds of optimizations within specified design
spaces, and doing so "
	  (:emph "very rapidly;"))
	(:item
	 "Capturing, i.e., implementing, the procedures and rules used
to solve repetitive tasks in engineering and other fields;")
	
	(:item
	 "Applying rules you have specified to achieve intermediate
and final outputs, which may include virtual models of wireframe,
surface, and solid geometric objects.")))
      
      ((:section :title "What GDL is not")
       ((:list :style :itemize)
	(:item "A CAD system (although it may operate on and/or generate geometric entities, and it can generate "
	       (:emph "applications")
	       " which can be considered as special-purpose CAD systems);")
	(:item "A drawing program (although it may operate on and/or generate geometric entities);")
	(:item "An Artificial Intelligence system (although it is an
excellent environment for developing capabilities which could qualify
as such);")
	(:item "An Expert System Shell (although one could be easily embedded within it).")))
      
      "Without further description, let's turn the page and get
      started with hands-on GDL..."))
      
