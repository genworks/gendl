;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: training-g101; Base: 10 -*-

(in-package :training-g101)

(define-object input-output (slide-show-leaf)
  
  :computed-slots
  ((strings-for-display "Input/Output")
   (:slide-data 
    `((:title
       "Input/Output and Streams"
       :bullet-points
       ((:description 
	 "The most basic functions for input and output 
are <i>read</i> and <i>print</i>.")
	(:description "both <i>read</i> and <i>print</i>
take an optional <i>stream</i> argument, which defaults to
<i>*standard-input*</i> and <i>*standard-output*</i>, respectively.")
	(:description "A <i>stream</i> is a theoretically infinite
source or destination for data (text or binary).")
	(:description "By default, the global system parameters
<i>*standard-input*</i> and <i>*standard-output*</i> will both
be bound to <i>*terminal-io*</i>, which is an input/output stream
bound to your Lisp ``toplevel'' (i.e. the <i>*common-lisp*</i>
buffer in Emacs)")
	(:description "In general, these system parameters should
<i>never</i> be explicitly changed (e.g. with <i>setq</i> or <i>setf</i>)")))
      
      
      (:title "Read"
       :bullet-points
       ((:description 
	 "<i>Read</i> by itself, typed into the toplevel, just sits there and waits 
for you to type something in (because <i>stream</i> defaults to the toplevel ``<i>*standard-input*</i>'').")
	
	(:description
	 "<i>read &amp;optional stream eof-error-p eof-value</i>"
	 :examples
	 ((:code
	   (read)
	   :input-string "1"
	   :return-value 1)
	  (:code
	   (read)
	   :input-string "(a b c)"
	   :return-value (a b c))))
	  
       
	(:description
	 "You can set a variable to the result of a call to read"
	 :examples
	 ((:code 
	   (setq myvar (read))
	   :input-string "Hello"
	   :return-value Hello)
	  (:code 
	   myvar
	   :return-value hello)))))
		
		

      (:title "Print"
       :bullet-points
       ((:description "<i>Print</i> will print exactly one Lisp expression
to the optional <i>stream</i> (default is <i>*standard-output*</i>), preceded
by a new line and followed by a space")
	
	(:description
	 "<i>print item &amp;optional stream</i>")
       
	(:description "By default, <i>stream</i> is the toplevel <i>*standard-output*</i>, so 
it just prints to the console, or <i>*common-lisp*</i> buffer"
	 :examples
	 ((:code
	   (print 'hello)
	   :print-string "hello"
	   :return-value hello)))

	(:description "Notice that <i>print</i> has the <i>side-effect</i> of 
printing <i>item</i>, and it also <i>returns</i> the <i>item</i> as its <i>return value</i>")
	
	
	(:description "This <i>return value</i> is <i>not</i> being printed to the <i>stream</i>,
it is only showing up in the toplevel, as is always the case for functions typed into the
toplevel")
	
	(:description "For both <i>read</i> and <i>print</i>, you can specify the 
stream to be some destination or source other than the default. Often this would
be a file in the computer filesystem.")
	
	(:description "If the <i>stream</i> is set to some other destination (e.g. a file),
only the printed data will go there, not the return value")
       
	(:description "More on this later")))

      
      (:title
       "The Printer"
       :bullet-points
       ((:description
	 "Lisp's facility for emitting characters to an output stream 
is known as ``<i>The Printer</i>.''")
	(:description "The most common functions used with the <i>printer</i> are:
<ul>
<li><i>print</i></li>
<li><i>princ</i></li>
<li><i>prin1</i></li>
<li><i>format</i></li>
</ul>"
	 :suppress-end-dot? t)
	(:description "<i>Print</i>, <i>prin1</i>, and <i>princ</i> 
will print exactly one Lisp expression (e.g. a List, a Symbol, a String, a 
Number, etc.) to an optional <i>stream</i>, which defaults to *standard-output*")
	
	
	(:description "<i>Princ</i> generates output fit for a human reader, while
<i>print</i> and <i>prin1</i> generate output fit for the Lisp reader (i.e. the <i>read</i> function).")
	
	(:description "<i>Print</i> and <i>prin1</i> are similar, but <i>print</i>
prints a newline first and a space afterward, while <i>prin1</i> does not.")
	
	(:description "<i>Format</i> is a very powerful generalization of both <i>princ</i> and
<i>prin1</i>, and is what you will use most often")))
      
      
      (:title "<i>Format</i>"
       
       :bullet-points
       ((:description
	 "<i>Format</i> is very general and can be used for almost all output")
	(:description
	 "<i>format stream format-string arg1 arg2 ... argN</i>")
	
	(:description
	 "<i>format</i> processes the <i>format-string</i> according to the <i>args</i>,
and emits the result to the specified destination stream")
	
	(:description
	 "The <i>format-string</i> can contain <i>format directives</i> which often
act as placeholders for the <i>args</i>")
	
	(:description
	 "The most common <i>format-string</i> is <i>~a</i> which processes its argument
as if by <i>princ</i>")
	
	(:description "<i>Stream</i> is a required argument, which is often specified as <i>t</i> or <i>nil</i>")
	
	(:description "If <i>stream</i> is specified as <i>T</i>, <i>format</i> will emit its result to
<i>*standard-output*</i> and return <i>nil</i>"
	 :examples
	 ((:code 
	   (format t "Hello There ~a" 'bob)
	   :print-string "Hello There bob"
	   :return-value nil)))
	
	(:description "If <i>stream</i> is specified as <i>nil</i>, 
<i>format</i> does not print anything through side-effect, but rather 
it returns, in the form of a string,  what it would have printed"
	 :examples
	 ((:code 
	   (format nil "Hello There ~a" 'bob)
	   :return-value "Hello There bob")))
	
	))
      
      
      (:title "The <i>Format String</i>"
       :bullet-points
       ((:description
	 "The <i>format-string</i> is a template which may contain <i>format directives</i>,
which are preceded by a <i>~</i> (<i>tilde</i>)")
	(:description "The <i>format directives</i> form a powerful language in its own right")
	(:description "The most commonly used <i>format directives</i> are <i>~a</i> and <i>~s</i>,
which process their arguments as if by <i>princ</i> and <i>prin1</i>, respectively"
	 :examples
	 ((:code 
	   (format t "~s" "some string")
	   :print-string "\"some string\""
	   :return-value nil)
	  (:code 
	   (format t "~a" "some string")
	   :print-string "some string"
	   :return-value nil)
	  (:code
	   (format t "~s" :michigan)
	   :print-string ":michigan"
	   :return-value nil)
	  (:code
	   (format t "~a" :michigan)
	   :print-string "michigan"
	   :return-value nil)))))

      
      
      (:title 
       "The <i>Format String</i>, Cont'd"
       :bullet-points
       ((:description ,(string-append
		       "See the ANSI specification or other "
		       (with-output-to-string (ss)
			 (html-stream ss ((:a :href "http://psg.com/~dlamkins/sl/chapter24.html")
					  "online")))
				      
		       " resources for a complete treatment of the 
syntax for <i>format directives</i>."))
	(:description "<i>~F</i> is another common format directive, which 
processes its output as a floating-point number according to many options"
	 :examples
	 ((:code
	   (format nil "~,4f" 34.433434323443)
	   :return-value "34.4334")
	  (:code
	   (format nil "Hello, ~a, your rating is ~,3f" 'bill 34.433434323443)
	   :return-value "Hello, bill, your rating is 34.433")))
	
	(:description "Note that <i>~f</i> does not mathematically ``round'' the
number in any predictable manner; if you are concerned with rounding precision,
round the number explicitly (using a function such as <i>round</i>) before
passing it to <i>format</i>")))
	        
      
      (:title "File Input and Output"
       :bullet-points
       ((:description "To read and write from and to a file,
we must read and write to a <i>stream</i> which is connected
to the file")
	(:description "The basic mechanism for connecting
a stream to a file is the function <i>open</i>"
	 :examples
	 ((:code
	   (defparameter *my-stream*
	       (open "~/readme.txt"))
	   :return-value
	   *my-stream*)
	  
	  (:code 
	   *my-stream*
	   :return-value "#<excl::character-input-file-stream ....>")))
		 
	
	(:description 
	 "However, when you use <i>open</i> explicitly like this, you have to 
remember to do a corresponding <i>close</i> later:"
	 :examples
	 ((:code 
	   (close *my-stream*)
	   :return-value t)))
	
	(:description
	 "If you print something to the file and forget to close the stream,
nothing is guaranteed about the contents of the file")
	
	(:description
	 "Plus, if your program throws an error in between the <i>open</i>
and the <i>close</i>, and you cannot continue gracefully from the error,
it is possible that the file will be left ``hanging'' open indefinitely")
	
	(:description
	 "For these reasons, you will generally want to use the macro
<i>with-open-file</i> rather than using <i>open</i> and <i>close</i> explicitly")))
      
      (:title
       "With-open-file"
       :bullet-points
       ((:description
	 "<i>with-open-file (stream pathname &key arguments) body</i>"
	 :examples
	 ((:code
	   (let ((readme-pathname "~/readme.txt"))
	     (with-open-file (input-stream readme-pathname) 
	       (read input-stream)))
	   :return-value hello)))
	(:description
	 "<i>With-open-file</i> automatically closes the <i>stream</i>, even if
an error is thrown somewhere in the middle of the <i>body</i>")
	(:description
	 "In order to write to a file, additional keyword arguments must be given<br>
 <i>Note</i>: in this example, the output shown with ``<i>==&gt;''</i> is what is 
written to the <i>file</i>, not the console or <i>*common-lisp*</i> buffer"
	 
	 :examples
	 ((:code
	   (let ((readme-pathname "~/readme.txt"))
	     (with-open-file (output-stream readme-pathname
			      :direction :output 
			      :if-exists :supersede 
			      :if-does-not-exist :create)
	       (format output-stream  "Hello There")))
	   
	   :print-string "Hello There"
	   
	   :return-value nil)))))
      
      
      (:title
       "Exercises for Session 6 : Input and Output"
       :bullet-points
       ((:description 
	 "Create a file (using Emacs) which contains a simple plist,
mapping keywords to values. The keywords should be actual keywords
 (beginning with colon), and the values can be any valid Lisp
expression, such as string, number, or list")
	(:description
	 "Create a function <i>read-plist</i> which takes a pathname
as an argument, and reads the first valid Lisp form from the file
corresponding to the pathname. For the file you just created, it
should read the entire plist from the file. If the call to <i>read</i>
is the final thing which happens in your program, the plist
will be the return value of the function and will thus be printed
in the console automatically.")
	(:description "Extend the above function to read the
plist (into a <i>let</i> variable), then print a prompt to 
standard output which says ``please enter a keyword.'' It
should then read an expression from standard input, and
return the plist value corresponding to the expression read
 (if any). Note: <i>getf</i> will simply return <i>nil</i>
if the keyword is not found in the plist, which is fine.")
	(:description "Create a function ``plist2table,''
which takes a pathname as an argument. The file corresponding 
to the pathname should contain a plist (same as above). Plist2table
should read the plist (into a let variable). Plist2table should
then emit an HTML table to standard output. The table should
contain two columns and as many rows as there are entries in the
plist. I will provide some macros to help with creating html tables.")))))))
