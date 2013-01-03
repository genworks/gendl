;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: training-g101; Base: 10 -*-

(in-package :training-g101)

(define-object data-structures (slide-show-leaf)
  
  :computed-slots
  ((strings-for-display "Other Data Structures")
   (slide-data 
    '((:title
       "Hash Tables"
       :bullet-points
       ((:description
	 "Allow very fast retrieval of data based on keys:"
	 :examples
	 ((:code
	   (setq os-ratings (make-hash-table))
	   :return-value "<eql hash-table with 0 entries @ #x9309caa>")
	  (:code
	   (setf (gethash :windows os-ratings) :horrible)
	   :return-value :horrible)
	  (:code
	   (gethash :windows os-ratings)
	   :return-value :horrible)))
	(:description
	 "Can store database tables locally and temporarily 
as hash tables for very fast repeated access.")
	(:description
	 "Can store a hash table in the symbol-plist of a function, 
to ``memoize'' return values.")))
      
      (:title
       "Arrays"
       :bullet-points
       ((:description
	 "Arrays allow you to represent an indexed vector or multidimensional matrix of values")
	(:description
	 "I find they are rarely necessary when working with Lisp in the context of The GDL System
 (you would generally use quantified objects instead)")
	(:description "If you might have a use for them, see the Lisp Resources at the end of this presentation for further information.")))
      
      (:title
       "Structures and Objects"
       :bullet-points
       ((:description
	 "Common Lisp defines the <i>defstruct</i> macro for defining 
a data structure with slots")
	(:description
	 "Common Lisp also contains a complete object-oriented
system called CLOS (Common Lisp Object System).")
	(:description
	 "Generally, the use of GDL <i>define-object</i> obviates the
need for these features of the language, thus they are
beyond the scope of this course (``Common Lisp for GDL Users'')")
	(:description
	 "However, they are provided in the language and
there is nothing which says you cannot use them, either 
with or without GDL's <i>define-object</i> (for example, you might
program a slot of a GDL <i>define-object</i> to actually be
a <i>CLOS</i> object")
	(:description
	 "See the Lisp Resources at the end of this presentation for more information if you are interested")))

      
      (:title "Exercises for Session 9 : Other Data Structures"
       :bullet-points
       ((:description 
	 "Create a ``memoized'' version of <i>factorial</i>,
which stores all the values it has computed so far in a hash 
table. Note that you can wrap a <i>let</i> binding <b>around</b>
a <i>defun</i> definition, so, for example, you can ``let''
a variable to a call to <i>(make-hash-table)</i>, so that variable
 (and thus the hash table), is limited to the lexical scope
of that let binding, which means only that particular <i>defun</i>
can see it.")))))))
