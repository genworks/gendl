;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: training-g101; Base: 10 -*-

(in-package :training-g101)

(define-object symbols (slide-show-leaf)
  
  :computed-slots
  ((strings-for-display "Symbols")
   (:slide-data 
    '((:title "Namespace"
	      :bullet-points
	      ((:description
		"Packages represent <i>namespaces</i> for Lisp symbols")
	       (:description
		"A <i>namespace</i> is like an ``area code'' for symbols")
	       (:description
		"Packages allow us to avoid ``name collisions''
<ul>
<li>when working in a team on a large project</li>
<li>when loading third-party code into our Lisp image (``Lisp world'')</li>
</ul>

Packages also provide an aspect of modularity and ``object-orientedness'' to a Lisp project")))

      (:title "Using Symbols for Comparison"
       :bullet-points
       ((:description
	 "Nonkeyword Symbols live in a certain Package")
	(:description
	 "These will fail <i>eql</i> equality test if in different packages")
	(:description
	 "For Comparisons, use <i>Keyword Symbols</i>")
	(:description
	 "<i>Keyword Symbols</i> are all in a special package called the ``Keyword'' package."
	 :examples ((:code (eql :foo :foo)
			   :return-value t)))))

     
      (:title "Working with Packages"
       :bullet-points
       ((:description
	 "Make a new package for each major project"
	 :examples
	 ((:code (gwl:define-package :mypackage (:export #:assembly)))))

	(:description 
	 "Make sure to put an in-package at the top of every file you work with. 
<i>Every</i> Lisp file should have <i>some</i> ``in-package'' statement in its header.")
       
	(:description
	 "In Emacs, you can use <tt>M-x fi:parse-mode-line-and-package</tt> to synchronize the package
in the Emacs buffer with the in-package of the file (only necessary for newly created files).")))))))
