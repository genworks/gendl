;;
;; Copyright 2002, 2009 Genworks International
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

(define-object welcome (slide-show-leaf)
  :computed-slots
  ((strings-for-display "Welcome to Lisp")
   (:slide-data 
    '((:title
       "S-expressions"
       :bullet-points
       ((:description
         "
Lisp source code is made up of Symbolic Expressions, or S-Expressions.

Each S-expressions is either:
<ul>
<li>a list</li>
<li>an atom</li>
</ul>"
         :examples ((:code 
                     '(2 3 4)
                     :return-value (2 3 4))
                    (:code 
                     '(("red" 0) ("white" 1) ("blue" 2))
                     :return-value (("red" 0) ("white" 1) ("blue" 2)))
                    (:code 
                     '(a "b" :c (1 2 3 "four"))
                     :return-value (a "b" :c (1 2 3 "four")))
                    
                    (:code 
                     'a 
                     :return-value a)
                    
                    (:code 
                     "b"
                     :return-value "b")
                    
                    (:code 
                     :c
                     :return-value :c))

         :suppress-end-dot? t)))
      
      (:title 
       "S-Expression Syntax"
       :bullet-points
       ((:description 
         "Prefix notation."
         :examples
         ((:code 
           (fn arg1 arg2 arg3 "..." argN)
           :return-value some-return-value)
          (:code 
           (+ 2 3 4)
           :return-value 9)
          (:code 
           (fn1 (fn2 arg1 arg2) (fn3 arg3 arg4) arg5)
           :return-value some-return-value)
          (:code
           (* (- 7 1) (- 4 2) 2)
           :return-value 24)))))
      
      (:title 
       "Lisp Syntax Simplicity"
       :bullet-points
       ((:description "That's really all there is to know about the 
syntax of the language")
        (:description "It is as clean and simple as you can get.")
        (:description "No ambiguities, no special cases")
        (:description "A welcome relief from the syntax of other 
popular languages such as Perl, C++, etc...")))


      (:title 
       "S-expressions to be Evaluated as Code"
       :bullet-points
       ((:description  
         "When S-exps are to be evaluated at runtime, certain rules apply:")
        
        (:description
         "Symbols evaluate as variables"
         :examples ((:code *print-length*
                           :return-value nil)))
        (:description 
         "Other atoms (numbers, strings, keyword symbols, 
quoted symbols) evaluate to themselves"
         :examples ((:code 42
                           :return-value 42)
                    (:code :key-1
                           :return-value :key-1)
                    (:code 'a
                           :return-value a)))
        (:description
         "Unquoted lists evaluate as function calls or 
macro-expansions, driven by the symbol at the beginning 
of the list"
         :examples ((:code (+ 1 2)
                           :return-value 3)
                    (:code (string-upcase "hey now")
                           :return-value "HEY NOW")
                    (:code (string-upcase 
                            (with-open-file (in "/tmp/data.in")
                              (read in)))
                           :return-value 
                           "[first S-exp in text of file]")))))
        
      
      (:title 
       "When Arguments are S-Expressions"
       :bullet-points
       ((:description  
         "If any of the arguments are themselves expressions they 
are evaluated in the same manner")
        (:description
         "The sub-expression (i.e. expression nested within another expression) 
is evaluated, and its result is passed as an argument")
        (:description
         "(fn1 (fn2 arg1 arg2) (fn3 arg3 arg4) arg5)"
         :examples
         ((:code 
           (* (- 7 1) (- 4 2) 2)
           :return-value 24)))))


      (:title
       "Variable Number of Arguments"
       :bullet-points
       (
        (:description
         "In Lisp - because of prefix notation"
         :examples
         ((:code
           (+ 1 2 3 4 5 6 7)
           :return-value
           28)))
        (:description
         "No ambiguity or precedence rules to remember regarding Order of Operations")
        (:description
         "Functions potentially can take any number of arguments (including zero)"
         :examples
         ((:code
           (+)
           :return-value
           0)
          (:code
           (+ 1)
           :return-value
           1)))
	
	(:description 
	 "Functions can also have certain required arguments"
	 :examples 
	 ((:code
           (/ 2)
           :return-value
           1/2)
	  (:code
           (- 2)
           :return-value
           -2)))
	
	(:description "Most modern CL development
	environments (e.g. Slime) will tell you what are the required
	arguments (if any) when you insert a function name.")))

      
      (:title
       "Turning off Evaluation"
       :bullet-points
       ((:description
         "<tt>(+ 1 2)</tt> evaluates to 3")
        (:description
         "Sometimes you want to turn off expression evaluation")
        (:description
         "<tt>(quote (+ 1 2))</tt> evaluates to <tt>(+ 1 2)</tt>")
        (:description
         "Common Lisp defines ' as an abbreviation for quote")
        (:description
         "<tt>'(+ 1 2)</tt> evaluates to <tt>(+ 1 2)</tt>")
	(:description 
	 "Note that <tt>quote</tt> is one of a few <i>special
operators</i> in Common Lisp (you can see that it is not an ordinary
function, otherwise it would evaluate its arguments).")
	(:description 
	 "Note also that lists returned by <tt>quote</tt> (unlike
lists returned by <tt>list</tt>) may be assumed to be <i>immutable</i>
-- that is, you should never modify them in place using any
destructive operators (no need to worry about this for now).")))
      
      
      (:title
       "Lisp Data Types"
       :bullet-points
       ((:description
         "
Usual Data Types in Other Languages
<ul>
<li>Numbers</li>
<li>Strings</li>
</ul>"
         :suppress-end-dot? t)

        (:description
         "
Fundamental Lisp Data Types
<ul>
<li>Symbols</li>
<li>Lists</li>
</ul>"
         :suppress-end-dot? t)))
      
      
      

      (:title 
       "What are Symbols?"
       :bullet-points
       ((:description
         "Symbols are OBJECTS")
        (:description
         "They are NOT simply strings.")
        (:description
         "They have a name, and can have a value, function and property-list")
        (:description
         "'Red evaluates to Red, the <i>Symbol Name</i> of the symbol named ``Red''")
        (:description
         "When the <i>Lisp Reader</i> encounters a Symbol for the first time, 
it <i>interns</i> (creates) this symbol in its internal <i>Symbol Table</i>.")))
      
      (:title
       "Why Symbols?"
       :bullet-points
       ((:description
         "Two references to \"MyBigLongString\" and \"MyBigLongString\"
refer to different strings")
        (:description
         "'MySymbol and 'MySymbol are both represented by the same
symbol")

        (:description
         "<i>eql</i> is the most basic equality function in Lisp, testing whether its arguments refer to the same 
actual <i>Object</i> (memory location), while <i>string-equal</i> compares two strings for equality, character-by-character.")
        
        (:description
         "Comparing symbols for equality is much faster than comparing strings, 
since simple <i>eql</i> can be used instead of <i>string-equal</i>"
         :examples
         ((:code
           (string-equal "Mybiglongstring" "Mybiglongstring")
           :return-value t)
          (:code
           (eql 'MySymbol 'MySymbol)
           :return-value t)))
        (:description
         "Symbols turn out to be a very useful concept, and one of the distinguishing features of the Lisp language.")
        (:description
         "More on this later")))


      (:title
       "Lists"
       :bullet-points
       ((:description
         "Lists are fundamental to LISP - LISt Processing")
        (:description
         "Lists are zero or more elements enclosed by parentheses")
        (:description
         "You have to quote a literal list for it to be evaluated
 as such; otherwise Lisp will assume you are trying to call 
a function"
         :examples
         ((:code 
           '(red green blue)
           :return-value (red green blue))))))
      
      (:title
       "Lisp Programs"
       :bullet-points
       ((:description
         "Lisp programs are themselves Lists")
        (:description
         "It is very easy for Lisp programs to generate and execute
Lisp code")
        (:description
         "This is NOT true of most other languages"
         :examples
         ((:code
           (defun hey-now ()
             (print "Hey Now"))
           :return-value hey-now)))))

      (:title
       "Overview of Lists - Creation"
       :bullet-points
       ((:description
         "A literal list with single-quote (elements are <i>not</i> evaluated)"
         :examples
         ((:code
           '(this is a list)
           :return-value
           (this is a list))))
        (:description
         "Using the ``List'' function (evaluates all arguments)"
         :examples
         ((:code
           (list '(+ 1 2) 'is (+ 1 2))
           :return-value
           ((+ 1 2) is 3))))))
             
      (:title
       "Simple List Operations"
       :bullet-points
       ((:description
         "first list"
         :examples
         ((:code
           (first '(a b c))
           :return-value a)))
        (:description
         "second list"
         :examples
         ((:code
           (second '(a b c))
           :return-value b)))
        (:description
         "third list"
         :examples
         ((:code
           (third '(a b c))
           :return-value c)))
        (:description
         "rest list"
         :examples
         ((:code
           (rest '(a b c))
           ;;
           ;;Corrected by Joost at TUD training.
           ;;
           :return-value (b c))))
        (:description
         "nth idx list"
         :examples
         ((:code
           (nth 2 '(a b c))
           :return-value  c)))))
                    
      (:title
       "The Empty List"
       :bullet-points
       ((:description "nil represents falsehood,"
         :examples ((:code nil :return-value nil))
         :suppress-end-dot? t)
        
        (:description "as well as the empty list."
         :examples ((:code (list) :return-value nil))
         :suppress-end-dot? t)
        
        (:description
         "nil evaluates to itself")))


      (:title
       "Testing for Listness"
       :bullet-points
       ((:description
         "listp item"
         :examples
         ((:code
           (listp '(pontiac cadillac chevrolet)) :return-value t)
          (:code (listp 99) :return-value nil)))
        (:description
         "nil represents FALSE"
         :examples
         ((:code (listp nil) :return-value "???")))))
      
      (:title
       "Functions"
       :bullet-points
       ((:description
         "defun name arg-list forms"
         :examples
         ((:code
           (defun hey-now () (print "Hey Now")) :return-value  hey-now)
             
          (:code
           (hey-now)
           :return-value "[prints] Hey Now")
             
          (:code (defun square(x) (* x x)) :return-value  square)
          
          (:code (square 4) :return-value  16)))))
      
      
      
      (:title 
       "Reading Lisp Code"
       :bullet-points
       ((:description "Read using indentation.")
        (:description "You can generally let your eyes skip over the parenthesis"
         :examples
         ((:code
           (defun my-first-lisp-fn () (list 'hello 'world))
           :return-value my-first-lisp-fn)))
        (:description "Try to read the preceding example as:
<pre>
 (defun 
my-first-lisp-fn (
)(list 'hello world)
  )
</pre>"
         :suppress-end-dot? t)
        (:description "Now try to read it as:
<pre>
  defun my-first-lisp-fn ()
    list 'hello 'world
</pre>"
         :suppress-end-dot? t)
        
        (:description "Close all parentheses together on the line.
Dangling parenthesis are generally frowned upon in finished code."
         :examples 
         ((:code
           '(united-states 
             (ohio 
              (toledo)
              indiana
              michigan
              (detroit
               (rennaissance-center
                (200-tower
                 (fifth-floor
                  (suite-20))))))))))
        
        (:description "Compare the above example to:
<pre>
 '(united-states 
   (ohio 
     (toledo)
     indiana
     michigan
      (detroit
       (rennaissance-center
        (200-tower
         (fifth-floor
          (suite-20)
          )
         )
        )
       )
      )
     )</pre>"
         :suppress-end-dot? t)
        
        (:description "(no need for these dangling parentheses)")))
          
      
      (:title "Local Variables"
       :bullet-points 
       ((:description "New local variables are bound (introduced) using Let.")
        (:description "<i>let variable-assignments body</i>"
         :examples
         ((:code (let ((quantity 20)
                       (excess 10))
                   (+ quantity excess))
                 :return-value 30)))))
      
      (:title
       "Variable Assignment"
       :bullet-points
       ((:description
         "In Lisp we use setq instead of =, :=, etc."
         :examples
         ((:code 
           (let ((quantity 20)
                 (excess 10))
             (setq quantity 30)
             (+ quantity excess))
           :return-value 40)))
        
        (:description "Global variables can be set with defparameter and changed with setq."
         :examples
         ((:code (defparameter *todays-temp* 101.5) :return-value *todays-temp*)
          (:code (setq *todays-temp* 99) :return-value 99)
          (:code *todays-temp* :return-value 99)))
        
        (:description "It is a long-standing Lisp convention to use Asterisks (``*'') 
to signify Global Variables (parameters). This is convention only, and the Asterisks 
have no meaning to Lisp itself.")
        
        (:description "Setf is used to set ``locations'' resulting from a function call:"
         :examples
         ((:code (setq *operating-systems* (list "Solaris" "Windows" "FreeBSD" "iOS" "Android"))
                 :return-value ("Solaris" "Windows" "FreeBSD" "iOS" "Android"))
          
          (:code (setf (first *operating-systems*) "Linux") :return-value "Linux")
          
          (:code *operating-systems* :return-value ("Linux" "Windows" "FreeBSD" "iOS" "Android"))))))
      
      
      (:title "Iteration"
       :bullet-points
       ((:description "dolist, dotimes")
        (:description "<i>dolist (variable-name list &optional return-value)</i>"
         :examples
         ((:code (let ((result 0))
                   (dolist (elem '(4 5 6 7) result)
                     (setq result (+ result elem)))) :return-value 22)))
        (:description "<i>dotimes (variable-name upper-limit &optional return-value)</i>"
         :examples ((:code 
                     (let ((result 1))
                       (dotimes (n 10 result)
                         (setq result (+ result n)))) :return-value 46)))))))))
