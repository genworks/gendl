;;
;; Copyright 2002, 2009 Genworks International and Genworks BV 
;;
;; This source file is part of the General-purpose Declarative
;; Language project (Gendl).
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

(in-package :training-g102)

(define-object objects (slide-show-leaf)

  :computed-slots
  ((strings-for-display "Functions as Objects")
   
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
       ((:description "Function Call is expressed as a List expression (a.k.a. \"Symbolic EXPression\" or \"S-exp\")")
	(:description "Function name first, then arguments, separated by spaces")
	(:description "For example, we feed <span class=lisp-code>2</span> and <span class=lisp-code>2</span>
into the function <span class=lisp-code>+</span> to yield the result which is <span class=lisp-code>4</span>."
		      :examples
		      ((:code 
			(+ 2 2)
			:return-value 4)))

	(:description 
	 "The multiply function (<span class=lisp-code>*</span>) applied to 
<span class=lisp-code>3</span> and <span class=lisp-code>3</span>"
	 :examples
	 ((:code 
	   (* 3 3)
	   :return-value 9)))
	(:description 
	 "The <span class=lisp-code>string-append</span> function applied to three strings"
	 :examples
	 ((:code 
	   (string-append "hey" " " "now")
	   :return-value "hey now")))

	))



      (:title 
       "Defining and Using Custom Functions in Common Lisp"
       :bullet-points 
       ((:description 
	 "Use the operator <span class=lisp-code>defun</span>, followed by name, argument list, and body."
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
		,(with-output-to-string (ss)
					(html-stream 
					 ss "Purpose in Life"
					 ((:table :border 1)
					  (:tr ((:th :width 0.5 :background "#BBDDFF")  "<br>")
					       ((:th :width 0.5 :background "#BBFFDD") 
						"Function (Common Lisp)")
					       ((:th :width 0.5 :background "#BBFFDD") 
						"Object (Gendl)"))
					  (:tr
					   (:td (:b "Typical Purpose"))
					   ((:td :background "#CCEEFF")
					    "Accepts some " (:i "arguments") 
					    ", possibly performs some " 
					    (:i "side-effects") 
					    ", and computes a "
					    (:i "return-value"))
					   ((:td :background "#CCEEFF")
					    "Accepts some " 
					    (:i "inputs")
					    ", possibly performs some " 
					    (:i "side-effects") 
					    ", and computes a set of "
					    (:i "outputs")))
					  (:tr
					   (:td (:b "How to Use"))
					   ((:td :background "#CCEEFF")
					    "You call it by name, by evaluating a Lisp expression")
					   ((:td :background "#CCEEFF")
					    (:ol (:li "You create or retrieve an " (:i "object"))
						 (:li "You send " (:i "messages") 
						      " to the object to get the outputs"))))
					  (:tr
					   (:td (:b "How to Define"))
					   ((:td :background "#CCEEFF")
					    (:pre
					     (:b "<span class=lisp-code>defun <i>name</i> <i>argument-list</i> <i>body</i></span>")))
					   ((:td :background "#CCEEFF")
					    (:code
					     (:pre (:b "<span class=gdl-operator>define-object</span>&nbsp;
<span class=gdl-object-def><i>name</i></span> &nbsp;
<span class=lisp-code><i>mixin-list</i></span> &nbsp;
<span class=lisp-code><i>specifications</i></span>")))))
					  (:tr
					   (:td (:b "How to Decompose Complexity"))
					   ((:td :background "#CCEEFF")
					    "Call other function definitions from within a function definition")
					   ((:td :background "#CCEEFF")
					    (:ol (:li "<i>Inherit</i> <i>slots</i> from other definitions using <i>mixins</i>")
						 (:li "Include <i>objects</i> of other types inside an object")))))))
		:suppress-end-dot? t)))

      
      (:title "Basic <span class=gdl-operator style=\"font-size: 1em;\">define-object</span> Syntax" :bullet-points
	      ((:description "(<span class=gdl-operator>define-object</span>&nbsp;
<span class=gdl-object-def><i>definition-name</i></span> (<span class=lisp-code><i>mixins</i></span>) 
<span class=lisp-code><i>specifications</i></span>)")
	       (:description "<span class=gdl-object-def><i>definition-name</i></span> is a symbol, just like a defun name.")
	       (:description "<span class=lisp-code><i>mixins</i></span> are zero or more names of other <span class=gdl-object-def><i>definition-names</i></span>
from which this definition will <i>inherit</i> characteristics.")
	       (:description "The body of the object definition consists of the
<span class=lisp-code><i>specifications</i></span>, which is a set of keyword/value pairs controlling the 
computational ``blueprint'' for this object.")))
      (:title "<span class=gdl-operator style=\"font-size: 1em;\">define-object</span> Syntax, Cont'd: 
<span class=gdl-section-keyword style=\"font-size: 1em;\"><i>:computed-slots</i></span>" 
	      :bullet-points
	      ((:description "<span class=gdl-section-keyword>:computed-slots</span> can represent known values, intermediate 
results, or the final <i>outputs</i> which can be computed by an object."
			     :examples
			     ((:define-object speed-of-sound)))
	       (:description "Each slot is defined with a list consisting of 
a <i>name</i> (a symbol specified by you) followed by an <i>expression</i> which can
be any valid CL value or expression.")
	       (:description "<span class=gdl-section-keyword>:computed-slots</span> can refer to the return-value of other slots in the 
same object using the special Gendl operator <span class = \"gdl-operator\">the</span>.")
	       (:description "In OO terminology, <span class=gdl-section-keyword>:computed-slots</span> are one mechanism which provides <i>message-passing</i> behavior.")
	       ))
      (:title "Making Objects and Sending Messages to Them" 
	      :bullet-points
	      ((:description "After a definition is saved, compiled, and loaded into memory, you can make an 
actual object with the operator <span class=lisp-code>make-object</span> (here, we are setting the object to 
a toplevel variable named <span class=lisp-code>obj</span>):" 
			     :examples 
			     ((:code (setq obj (make-object 'speed-of-sound))
				     :return-value* "<speed-of-sound 26112>")))
	       (:description "You can then send <i>messages</i> to the object held in the variable 
<span class=lisp-code>obj</span> with the operator <span class=lisp-code>the-object</span>:" 
			     :examples
			     ((:code (the-object obj speed) :return-value 9.5939274091766)))
	       (:description "<i>Note:</i> in normal Gendl usage, most objects are created on-the-fly, on-demand, automatically (as child objects of other objects), 
so you will not usually be calling <span class=lisp-code>make-object</span> explicitly by yourself, except for testing or batch computations." )))

      
      (:title "<span class=gdl-operator style=\"font-size: 1em;\">define-object</span> Syntax, Cont'd: 
<span class=gdl-section-keyword style=\"font-size: 1em;\"><i>:input-slots</i></span>" 
	      :bullet-points
	      ((:description "The <span class=gdl-section-keyword>:input-slots</span> section is where you put names for values which can or must be \"passed in\" to an object when 
the object is created.")
	       (:description "<span class=gdl-section-keyword>:input-slots</span> can be <i>required</i> or <i>optional</i> (We will discuss <i>optional</i> ones later).")
	       (:description "<i>Required</i> <span class=gdl-section-keyword>:input-slots</span> are specified with a symbol (not inside its own list):"
			     :examples
			     ((:define-object speed-of-sound-at-temperature)))))
                              
      (:title "Making an Object and passing in Inputs</i>" 
	      :bullet-points
	      ((:description "The values for the <span class=gdl-section-keyword>:input-slots</span> can be passed
into <span class=lisp-code>make-object</span> as keyword arguments:"
			     :examples
			     ((:code (setq obj (make-object 'speed-of-sound-at-temperature :temperature 288))
				     :return-value* "<speed-of-sound-at-temperature 26120>")
			      (:code (the-object obj speed) :return-value 9.853972557948394)
			      (:code (setq obj (make-object 'speed-of-sound-at-temperature :temperature 242))
				     :return-value* "<speed-of-sound-at-temperature 26343>")
			      (:code (the-object obj speed) :return-value 9.03280817811936)
			      ))))
      
      
      (:title "<i>Exercises</i> 1" :bullet-points
	      ((:description "1. Write an object definition to compute the Lift (force) on an airfoil. Your
definition should contain the following <span class=gdl-section-keyword>:input-slots</span>:

<dl>
<dt><span class=gdl-message-name>rho</span><dd>for the air density
<dt><span class=gdl-message-name>v</span><dd>for the velocity
<dt><span class=gdl-message-name>A</span><dd>for the surface area
<dt><span class=gdl-message-name>C-of-L</span><dd>for the coefficient of lift
</dl>

and should compute <span class=gdl-message-name>lift-force</span> as a <span class=gdl-section-keyword>:computed-slot</span>.")
               
	       (:description "Test your definition by making objects with the following sets of values, and sending each one the 
 <span class=gdl-message-name>lift-force</span> message:"
                             
			     :examples
			     ((:code (make-object 'lift-calculator :rho 1.22 :v 108 :A 25 :C-of-L 0.20))
			      (:code (make-object 'lift-calculator :rho 1.22 :v 123 :A 38 :C-of-L 0.20))
			      (:code (make-object 'lift-calculator :rho 1.22 :v 142 :A 42 :C-of-L 0.20))))))
      
      
      (:title "Child <span class=gdl-section-keyword style=\"font-size: 1em;\">:objects</span>" 
	      :bullet-points
	      ((:description "The <span class=gdl-section-keyword>:objects</span> section 
is where you put the names, types, and specify the input values for <i>child</i> objects")
	       (:description "The type is specified with the keyword <span class=lisp-code>:type</span> 
followed by a literal (i.e. quoted) or computed symbol.")
	       (:description "The rest of the inputs are given as pairs of keyword/expression:"
			     :examples
			     ((:define-object speeds-of-sound)))))
      
      (:title "Child objects in a <span class=lisp-code-keyword style=\"font-size: 1em;\">:sequence</span>" 
	      :bullet-points
	      ((:description "You can make a sequence by giving the special input keyword <span class=lisp-code>:sequence</span>.")
               
	       (:description "A standard (simple) sequence then takes a list with <span class=lisp-code>(:size &lt;<i>positive-integer</i>&gt;)</span>.")
               
               
	       (:description "Individual child elements can be accessed from within the object specification with the 
special operator <span class=gdl-operator>the-child</span>.")
               
	       (:description "You can produce a standard CL list from a sequence with the special Gendl operator <span class=lisp-code>list-elements</code>:"
			     :examples
			     ((:define-object speeds-of-sound-sequence)))))
      
      (:title "Referring to an element in a <span class=lisp-code-keyword style=\"font-size: 1em;\">:sequence</span>" 
	      :bullet-points
	      ((:description "The sequenced object itself is of a special type called an <i>aggregate</i>")
               
	       (:description "To fetch an element, you wrap parentheses around the message for the aggregate, and give the index number as an argument"
			     :examples
			     ((:code (setq object (make-object 'speeds-of-sound-sequence))
				     :return-value "<speeds-of-sound-sequence @ #x74ed72ca>")
			      (:code (the-object object (speeds 0) speed)
				     :return-value 9.03280817811936)
			      (:code (the-object object (speeds 1) speed)
				     :return-value 9.853972557948394)
			      (:code (the-object object (speeds 2) speed)
				     :return-value 10.386999094637488)))))
      
      
      (:title "<i>Exercises</i> 2" :bullet-points
	      ((:description "2. Write an object definition to compute the average lift force given a list of input plists, for example:"
			     :examples
			     ((:code (list (list :rho 1.22 :v 108 :A 25 :C-of-L 0.20)
					   (list :rho 1.22 :v 123 :A 38 :C-of-L 0.20)
					   (list :rho 1.22 :v 142 :A 42 :C-of-L 0.20)))))
	       (:description "The length of the sequence should be driven by the length of the input list.")
	       (:description "You can use some combination of <span class=lisp-code>nth</span>, <span class=lisp-code>getf</span>, 
and <span class=gdl-operator>the-child</span> to retrieve the correct values for each input in the child sequence")
	       (:description "You might already have the definition for the child objects from the first Exercise.")))
      
      (:title "Gendl <span class=gdl-section-keyword style=\"font-size: 1em;\">:functions</span>" 
	      :bullet-points
	      ((:description "The <span class=gdl-section-keyword>:functions</span> section 
is where you put the names, argument lists, and bodies for <i>Functions</i> which can operate 
within the context of a Gendl object.")
	       (:description "Not to be confused with normal CL functions defined with <span class=lisp-code>defun</span> (although syntax is nearly the same).")
	       (:description "The big difference is that Gendl <span class=gdl-section-keyword>:functions</span> can make use of 
<span class=lisp-code>the</span> to refer to messages within the same object"
			     :examples
			     ((:define-object speed-of-sound-at-temperature-func)))))
      
      (:title "Calling Gendl <span class=gdl-section-keyword style=\"font-size: 1em;\">:functions</span>"
	      :bullet-points
	      ((:description "To refer to the function with <span class=lisp-code>the</span> 
or <span class=lisp-code>the-object</span>, you wrap its name with parentheses and give the arguments, 
as with a normal CL function"
			     :examples
			     ((:code (let ((object (make-object 'speed-of-sound-at-temperature-func)))
				       (the-object object (speed 280)))
				     :return-value 9.71614797221615)))))
      
      
      (:title "<i>Exercises</i> 3" :bullet-points
	      ((:description "3. Add a Gendl function to your lift calculating object which accepts <span class=lisp-code>C-of-L</span> as an argument,
instead of using the <span class=lisp-code>:input-slot</span> from the definition as it does now.")
	       (:description "Test your definition by making an object from it and calling the function with the following values for coefficient-of-lift:
<ol>
<li><span class=lisp-code>0.15</span>
<li><span class=lisp-code>0.18</span>
<li><span class=lisp-code>0.20</span>
<li><span class=lisp-code>0.22</span>
<li><span class=lisp-code>0.25</span>
</ol>")
	       (:description "Compare your results with your neighbor.")))))))

      


               
