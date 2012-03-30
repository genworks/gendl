;;
;; Copyright 2002, 2009 Genworks International and Genworks BV 
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


(in-package :training-g101)

(define-object functions (slide-show-leaf)
  
  :computed-slots
  ((strings-for-display "Functions" :uncached)
  
   (slide-data 
    '((:title "Functions"
       :bullet-points
       ((:description
	 "Understanding functions is one of the keys to understanding Lisp - <i>Paul Graham</i>")
	(:description 
	 "Named Functions are defined with <i>Defun</i> (``Definition of a Function'')")
	(:description
	 "Technically, a named function is a <i>Symbol</i> with a <i>Function object</i> stored in its ``function-slot''"
	 :examples
	 ((:code
	   (defun add-3 (num)
	     (+ num 3))
	   :return-value add-3)
	  (:code 
	   (symbol-function 'add-3)
	   :return-value "#<Interpreted Function add-3>")
	  (:code 
	   (function add-3)
	   :return-value "#<Interpreted Function add-3>")))
	
	(:description 
	 "<i>#'</i> is to <i>function</i> as <i>'</i> is to <i>quote</i>"
	 :examples
	 ((:code 
	   "(quote (+ 1 2))"
	   :return-value (+ 1 2))
	  
	  (:code 
	   '(+ 1 2)
	   :return-value (+ 1 2))
	  
	  (:code 
	   "(function add-3)"
	   :return-value "#<Interpreted Function add-3>")
	 
	  (:code 
	   #'add-3
	   :return-value "#<Interpreted Function add-3>")))))

      
      
      (:title "Passing a function as an argument to another function"
       :bullet-points
       ((:description "When passing a function as an argument, 
we must pass the <i>function object</i>")
	(:description "The <i>Function object</i> for a named function 
can be obtained using the function <i>Function</i>"
		      :examples
		      ((:code 
			"(mapcar (function add-3) '(2 3 4))"
			:return-value (5 6 7))))
	
	(:description "Or with the shorthand version <i>#'</i>:"
		      :examples
		      ((:code
			(mapcar #'add-3 '(2 3 4))
			:return-value (5 6 7))
	  
		       (:code
			(sort (list 6 4 3 9) #'<)
			:return-value (3 4 6 9))
	  
		       (:code
			(sort (list 6 4 3 9) #'>)
			:return-value (9 6 4 3))))))
      
      
      (:title "Anonymous (Unnamed) Functions"
       :bullet-points
       ((:description "<i>Unnamed</i> functions can be defined using <i>lambda</i>")
	(:description "You can substitue a <i>Lambda Expression</i> anywhere you 
use a <i>Function Name</i>"
		      :examples
		      ((:code 
			(mapcar #'add-3 '(2 3 4))
			:return-value (5 6 7))
		       (:code 
			(mapcar #'(lambda(num) (+ num 3))
				'(2 3 4))
			:return-value (5 6 7))))))
      
      
      (:title "Using an anonymous predicate function with <i>Sort</i>"
       :bullet-points
       ((:description "<i>Sort</i> takes a <i>predicate</i> function 
 (a function returning <i>T</i> or <i>NIL</i>), which must take 2 arguments")
	(:description "<i>Strings</i> can be compared lexically using <i>String&lt;</i>
or <i>String&gt;</i>. More on <i>Strings</i> later."
		      :examples
		      ((:code
			(sort (list (list 4 "Buffy") (list 2 "Keiko") 
				    (list 1 "Judy") (list 3 "Aruna"))
			      #'(lambda(pair1 pair2)
				  (< (first pair1) (first pair2))))
			:return-value ((1 "Judy") (2 "Keiko") (3 "Aruna") (4 "Buffy")))
		       (:code
			(sort (list (list 1 "Judy") (list 2 "Keiko") 
				    (list 3 "Aruna") (list 4 "Buffy"))
			      #'(lambda(pair1 pair2)
				  (string< (second pair1) (second pair2))))
			:return-value ((3 "Aruna") (4 "Buffy") (1 "Judy") (2 "Keiko")))))))
	  
      
      (:title "Function Arguments, revisited"
       :bullet-points
       ((:description "Functions can take optional arguments using <i>&optional</i> in the argument list")
	(:description "Functions can take optional keyword arguments using <i>&key</i> in the argument list")
	(:description "Functions can take any number of arguments using <i>&rest</i>  in the argument list")))
      
      (:title "Optional Arguments"
       :bullet-points
       ((:description "
<pre><i>
defun function-name (&optional
                      (arg1-name arg1-default) 
                      (arg2-name arg2-default) 
                      ... 
                      (arg3-name arg3-value))</i></pre>"
		      :examples
		      ((:code 
			(defun greeting (&optional (username "Jake"))
			  (list 'hello 'there username))
			:return-value greeting)
		       (:code
			(greeting)
			:return-value (hello there "Jake"))
		       (:code
			(greeting "Joe")
			:return-value (hello there "Joe"))))))
      
      (:title "Keyword Arguments"
       :bullet-points
       ((:description "
<pre><i>
defun function-name (&key 
                      (arg1-name arg1-default) 
                      (arg2-name arg2-default) 
                      ... 
                      (arg3-name arg3-value))</i></pre>")

	(:description "Keyword arguments are order-independent and improve readability of code."
		      :examples
		      ((:code 
			(defun greeting (&key (username "Jake") (greeting "how are you?"))
			  (format nil "Hello there ~a, ~a" username greeting))
			:return-value greeting)
		       (:code 
			(greeting)
			:return-value "Hello there Jake, how are you?")
		       (:code (greeting :greeting "how have you been?") 
			      :return-value "Hello there Jake, how have you been?")
		       (:code (greeting :greeting "how have you been?" :username "Joe") 
			      :return-value "Hello there Joe, how have you been?")))))
      
      (:title "Keyword Arguments, Cont'd"
       :bullet-points
       ((:description "The Keyword Argument <i>:test</i> is often used to pass 
an optional function-object to a function")
	(:description "Generally, the <i>:test</i> keyword argument will
allow you to specify a match function other than the default of <i>eql</i>."
		      :examples
		      ((:code 
			(member "a" '("a" "b" "c" "d"))
			:return-value nil)
		       (:code 
			(member "a" '("a" "b" "c" "d") :test #'string-equal)
			:return-value ("a" "b" "c" "d"))
		       (:code 
			(member "a" '("a" "b" "c" "d") :test #'string-equal)
			:return-value ("a" "b" "c" "d"))
		       (:code 
			(remove "a" '("a" "b" "c" "d"))
			:return-value ("a" "b" "c" "d"))
		       (:code 
			(remove "a" '("a" "b" "c" "d") :test #'string-equal)
			:return-value ("b" "c" "d"))
		       (:code 
			(remove 3  '(("a" 1) ("b" 2) ("c" 3) ("d" 4)))
			:return-value (("a" 1) ("b" 2) ("c" 3) ("d" 4)))
		       (:code 
			(remove 3  '(("a" 1) ("b" 2) ("c" 3) ("d" 4))
				:test #'(lambda(x y) (eql x (second y))))
			:return-value (("a" 1) ("b" 2) ("d" 4)))))
	
	(:description "The keyword argument <i>:key</i> can also sometimes be
used to apply a function to elements of a list before trying to 
match (compare the following with last example above, with same result:"
		      :examples
		      ((:code
			(remove 3  '(("a" 1) ("b" 2) ("c" 3) ("d" 4))
				:key #'second)
			:return-value (("a" 1) ("b" 2) ("d" 4)))))))
      
      (:title
       "Recursion"
       :bullet-points
       ((:description
	 "Recursion is sometimes an alternative to explicit looping:"
	 :examples
	 ((:code
	   (defun factorial (n)
	     (if (or (= n 0)
		     (= n 1))
		 1
		 (* n (factorial (- n 1)))))
	   :return-value
	   factorial)
	  (:code
	   (factorial 4)
	   :return-value
	   24)))
	(:description
	 "As development iterates, recursion can be made more efficient
with tail recursion and function memoization.")))
      
      
      (:title
       "Formulating a Recursion"
       :bullet-points
       ((:description
	 "Show how to solve the smallest version of the problem - 
the <i>base case</i> - by some finite number of operations.")
	(:description
	 "Show how to solve the problem in the general case by
breaking it down into a finite number of similar, but
smaller problems.")
	(:description
	 "Example: compute the length of a list")
	(:description
	 "The length of an empty list is 0.")
	(:description
	 "In the general case, the length of a proper list
is the length of its rest plus 1.")
	(:description
	 "Code:"
	 :examples
	 ((:code
	   (defun g-length (lst)
	     (if (null lst) 
		 0 
		 (+ (g-length (rest lst)) 1)))
	   :return-value
	   g-length)
	  (:code
	   (g-length nil)
	   :return-value
	   0)
	  (:code
	   (g-length '(lisp rules))
	   :return-value
	   2)))))
      
      
      (:title
       "Exercises for Session 5 : Functions"
       :bullet-points
       (
	
	(:description
	 "Write a function, <i>sort-numbers</i> using <i>safe-sort</i>, which will sort a list of numbers in either ascending or descending
order, depending on a keyword argument <i>:ordering</i>. The value of <i>:Ordering</i> should be a keyword, 
either <i>:ascending</i> or <i>:descending</i>, default to <i>:ascending</i>, and the program should throw an 
error if an argument other than one of these two is given."
	 :examples
	 ((:code 
	   (sort-numbers '(4 3 2 6 5 7 6))
	   :return-value (2 3 4 5 6 6 7))
	  (:code 
	   (sort-numbers '(4 3 2 6 5 7 6) :ordering :ascending)
	   :return-value (2 3 4 5 6 6 7))
	  (:code 
	   (sort-numbers '(4 3 2 6 5 7 6) :ordering :descending)
	   :return-value (7 6 6 5 4 3 2))
	  (:code
	   (sort-numbers '(4 3 2 6 5 7 6) :ordering :up)
	   :return-value some-error)))
	
	(:description
	 "Extend the above <i>sort-numbers</i> function to accept <i>:ordering</i> values of <i>:up</i> 
 (synonymous with <i>:ascending</i>) and <i>:down</i> (synonymous with <i>:descending</i>)"
	 :examples
	 ((:code 
	   (sort-numbers '(4 3 2 6 5 7 6) :ordering :ascending)
	   :return-value (2 3 4 5 6 6 7))
	  (:code 
	   (sort-numbers '(4 3 2 6 5 7 6) :ordering :descending)
	   :return-value (7 6 6 5 4 3 2))
	  (:code
	   (sort-numbers '(4 3 2 6 5 7 6) :ordering :big-to-small)
	   :return-value some-error)))
	
	
	(:description
	 "Evolve the above function to a new function <i>sort-pairs</i>, which accepts a list of lists, 
where each internal list is a pair with a number and a string. By default, <i>sort-pairs</i>
should sort the lists numerically according to the numbers, but it should take a keyword argument
<i>:sort-by</i> which has valid values of <i>:number</i> or <i>:string</i>. If <i>:string</i> is specified,
the lists should be sorted according to the string rather than the number.
<p>
The function must still accept the :ordering keyword as above.
<p>
<ul>
<b><i>Hints:</i></b>
<li>Remember to use <i>string&lt;</i> and <i>string&gt;</i> to compare strings.</li>
<li>Of course, you can just use &lt; and &gt; to compare numbers.</li>
</ul>
"
	 
	 :examples
	 ((:code 
	   (sort-pairs '((4 "Buffy") (2 "Keiko") (1 "Judy") (3 "Aruna")))
	   :return-value ((1 "Judy") (2 "Keiko") (3 "Aruna") (4 "Buffy")))
	  (:code 
	   (sort-pairs '((4 "Buffy") (2 "Keiko") (1 "Judy") (3 "Aruna")) :sort-by :string)
	   :return-value ((3 "Aruna") (4 "Buffy") (1 "Judy") (2 "Keiko")))))

	
	(:description "Extend the above function to handle the number-string pairs in either order.

<i>Hints:</i>
<ul>
<li>You can find out if something is a number with <i>numberp</i></li>
<li>You can find out if something is a string with <i>stringp</i></li>
</ul>
"
		      :examples
		      ((:code 
			(sort-pairs-2 '((4 "Buffy") ("Keiko" 2) (1 "Judy") ("Aruna" 3)))
			:return-value ((1 "Judy") ("Keiko" 2) ("Aruna" 3) (4 "Buffy")))
		       (:code 
			(sort-pairs-2 '(("Buffy" 4) (2 "Keiko") ("Judy" 1) (3 "Aruna")) :sort-by :string)
			:return-value ((3 "Aruna") ("Buffy" 4) ("Judy" 1) (2 "Keiko")))))
	 	
	(:description
	 "Revisit <i>set-difference-o</i> and <i>intersection-o</i>, and allow them to 
accept a <i>:test</i> keyword argument.")))))))
