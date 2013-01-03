;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: training-g101; Base: 10 -*-
(in-package :training-g101)


(define-object macros (slide-show-leaf)
  
  :computed-slots
  ((strings-for-display "Macros")
   (:slide-data 
    `((:title "Programs which write Programs"
	      :bullet-points
	      ((:description "Using <i>macros</i> is the 
most common way to write code which generates
other code")
	       (:description "In everyday CL and GDL development, you almost never have to 
write your own macros (more on this later).")
	       (:description "A <i>macro</i> will convert
one Lisp expression into another Lisp expression,
according to its <i>arguments</i>, before the 
final expression is actually evaluated or compiled")
	       (:description 
		"Macros can contain other 
macros, and potentially an expression can be transformed
many, many times before it actually gets evaluated
or compiled")))
     
      (:title "Common pre-defined Macros"
	      :bullet-points
	      ((:description "Pre-defined macros often start with ``def'' or ``with-''")
	       (:description
		,(with-output-to-string(ss)
		   (net.html.generator:html-stream 
		    ss
		    ((:table :border 1)
		     ((:tr :bgcolor "yellow")
		      (:td ((:font :size 7) (:b "defun")))
		      ((:td :bgcolor "white")
		       ((:font :size 7) "Binds a symbol's " (:i "function-slot") " to a " (:i "lambda expression (function object)"))))
		     ((:tr :bgcolor "yellow")
		      (:td ((:font :size 7) (:b "defparameter")))
		      ((:td :bgcolor "white")
		       ((:font :size 7) "Binds a symbol's " (:i "value-slot") " to a " (:i "value"))))
		     ((:tr :bgcolor "yellow")
		      (:td 
		       ((:font :size 7)(:b "define-object") " (GDL)"))
		      ((:td :bgcolor "white")
		       ((:font :size 7) "Creates a named GDL object definition (and CLOS class definition)")))))))
	       (:description
		,(with-output-to-string(ss)
		   (net.html.generator:html-stream 
		    ss
		    ((:table :border 1)
		     ((:tr :bgcolor "yellow")
		      (:td ((:font :size 7) (:b "with-open-file")))
		      ((:td :bgcolor "white")
		       ((:font :size 7) "Wraps a body with automatic opening and closing of a file")))
		     ((:tr :bgcolor "yellow")
		      (:td ((:font :size 7) (:b "with-output-to-string")))
		      ((:td :bgcolor "white")
		       ((:font :size 7) "Captures stream output into a string")))))))))
     
     
      (:title "Backquote (`)"
	      :bullet-points
	      ((:description
		"Backquote provides a convenient mechanism for generating
Lisp expressions")
	       (:description
		"By itself, it behaves just like <i>quote</i>,
but used in conjunction with comma (,) it allows you to
create templates of lists")
	       (:description
		"This way, macros look very similar to the forms they
will produce"
		:examples
		((:code 
		  (let ((guy "bob"))
		    `(format nil "Hello, ~a" ,guy))
		  :return-value (format nil "Hello, ~a" "bob"))))))
     
      (:title "A Simple Example"
	      :bullet-points
	      ((:description
		"Often, macros are used to automatically ``wrap'' a 
<i>body</i> of Lisp code with some other code:"
		:examples
		((:code 
		  (defmacro with-html-tag (tag &rest body)
		    `(progn
		       (format t "~&&lt;~(~A~)&gt;~%" ',tag)
		       ,@body
		       (format t "~&&lt;/~(~A~)&gt;~%" ',tag)))
		  :return-value with-html-tag)
	 
		 (:code 
		  (with-html-tag b 
		    (format t "hello"))
		  :print-string "<pre><code>
<b>
hello
</b><code></pre>"
		  :return-value nil)))))
     
     
      (:title "When to use Macros"
	      :bullet-points
	      ((:description
		"In normal GDL development, you almost never use macros")
	       (:description
		"The use of macros is not appropriate for ``inlining'' code")
	       (:description
		"Use macros <i>only</i> when true source code transformation is required.")))))))
