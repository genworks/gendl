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

(in-package :gdl-tutorial)

(defparameter *gdl-syntax*
    `((:chapter :title "GDL Syntax")
      ((:section :title "Define-Object")
       (:index "objects!defining")
       (:emph (:indexed "Define-object")) " is the basic macro for defining objects in GDL. An object 
definition maps directly into a Lisp (CLOS) class definition. 

The "
       (:texttt "define-object")
       " macro takes three basic arguments:"
       
       ((:list :style :itemize)
         (:item "a "
                (:emph "name")
                ", which is a symbol;")
         (:item "a "
                (:emph (:indexed "mixin-list"))
                ", which is a list of symbols naming other objects from which the current object 
will inherit characteristics;")
         (:item "a "
                (:emph (:indexed "specification-plist"))
                ", which is spliced in (i.e.\\ doesn't have its own surrounding 
parentheses) after the mixin-list, and describes
the object model by specifying properties of the object (messages, contained objects, etc.)
The specification-plist typically makes up the bulk of the object definition."))
       
               "

Here are descriptions of the most common keywords making up the specification-plist:"
        
        ((:list :style :description)
         ((:item :word (:indexed "input-slots"))
          "specify information to be passed into the object instance when it is created.")
         ((:item :word (:indexed "computed-slots"))
          "are really cached methods, with expressions to compute and return a value.")
         ((:item :word (:indexed "objects"))
          "specify other instances to be ``contained'' within this instance.")
         ((:item :word (:indexed "functions"))
          "are (uncached) functions ``of'' the object, i.e.\\ they are actually methods which
discriminate on their first argument, which is the object instance upon which they are operating. 
GDL functions can also take other non-specialized arguments, just like a normal CL function."))

	"Figure "
        (:ref "fig:object-hello")
        " shows a simple example, which contains two input-slots, "
        (:texttt "first-name")
        " and "
        (:texttt "last-name")
        ", and a single computed-slot, "
        (:texttt "greeting")
        "."
        ((:boxed-figure  :caption "Example of Simple Object Definition"
                         :label "fig:object-hello")
         (:verbatim "


 (define-object hello ()
   :input-slots (first-name last-name)

   :computed-slots 
   ((greeting (format nil \"Hello, ~a ~a!!\" 
                     (the first-name) 
                     (the last-name)))))
"))
        "As you can see, a GDL Object is analogous in some ways to a "
        (:texttt "defun")
        ", where the input-slots are like arguments to the function, and the computed-slots
are like return-values. But seen another way, each attribute in a GDL object is like a function in its own right.

The referencing macro "
        (:texttt (:indexed "the"))
        " shadows CL's "
        (:texttt "the")
        " (which is a seldom-used type declaration operator). "
        (:texttt "The")
        " in GDL is a macro which is used to reference the value of other messages 
within the same object or within contained objects. In the above example, we are using "
        (:texttt "the")
        " to refer to the values of the messages (input-slots) named "
        (:texttt "first-name")
        " and "
        (:texttt "last-name")
        ". 

Note that messages used with "
	(:texttt "the")
	" are given as symbols. These symbols are unaffected by the current Lisp "
	(:texttt "*package*")
	", so they can be specified either as plain unquoted symbols or as keyword
symbols (i.e.\\ preceded by a colon), and the "
	(:texttt "the")
	" macro will process them appropriately.")

      ((:section :title "Making Instances and Sending Messages")
        "Once we have defined an object such as the example above, we can use
the constructor function "
        (:texttt (:indexed "make-object"))
        " in order to create an "
        (:emph "instance")
        " of it. This function is very similar to the CLOS "
        (:texttt (:indexed "make-instance"))
        " function. Here we create an instance of "
        (:texttt "hello")
        " with specified values for "
        (:texttt "first-name")
        " and "
        (:texttt "last-name")
        " (the required input-slots), and assign this instance as the value of the symbol "
        (:texttt "my-instance")
        ":"
        (:verbatim "
 GDL-USER(16): (setq my-instance
                 (make-object 'hello :first-name \"John\" 
                                     :last-name \"Doe\"))
 #<HELLO @ #x218f39c2>")
        "As you can see, keyword symbols are used to ``tag'' the input values, and the return value is an instance of class "
        (:texttt "hello")
        ". Now that we have an instance, we can use the macro "
        (:texttt (:indexed "the-object"))
        " to send messages to this instance:"
        (:verbatim "
 GDL-USER(17): (the-object my-instance greeting)
 \"Hello, John Doe!!\"")
        (:texttt "The-object")
        " is similar to "
        (:texttt "the")
        ", but as its first argument it takes an expression which evaluates to an
object instance. "
        (:texttt "The")
        ", by contrast, assumes that the object instance is the lexical variable "
        (:texttt (:indexed "self"))
        ", which is automatically set within the lexical context of a "
	(:texttt "define-object")
	".

Like "
	(:texttt "the")
	", "
	(:texttt "the-object")
	" evaluates all but the first of its arguments as package-immune symbols,
so although keyword symbols may be used, this is not a requirement, and plain,
unquoted symbols will work just fine.

For convenience, you can also set "
        (:texttt "self")
        " manually at the CL Command Prompt, and use "
        (:texttt "the")
        " instead of "
        (:texttt "the-object")
        " for referencing:"
        (:verbatim "
 GDL-USER(18): (setq self 
                 (make-object 'hello :first-name \"John\" 
                                     :last-name \"Doe\"))
 #<HELLO @ #x218f406a>

 GDL-USER(19): (the greeting)
 \"Hello, John Doe!!\"")
        
        "In actual fact, "
        (:texttt "(the ...)")
        " simply expands into "
        (:texttt "(the-object self ...)")
        ".")
      
      ((:section :title "Objects")
       (:index "objects")
       (:index "containment!object")
       (:index "objects!child")
       (:index "objects!contained")
       "The "
       (:texttt ":objects")
       " keyword specifies a list of ``contained'' instances,
where each instance is considered to be a ``child'' object of the current
object. Each child object is of a specified type, which itself must be defined
with "
       (:texttt "define-object")
       " before the child object can be instantiated.

Inputs to each instance are specified as a plist of keywords and
value expressions, spliced in after the object's name and type
specification. These inputs must match the inputs protocol (i.e.\\ the input-slots)
of the object being instantiated. Figure "
       (:ref "fig:object-city")
       " shows an example of an object which contains some child objects."
       ((:boxed-figure :caption "Object Containing Child Objects"
		       :label "fig:object-city")
	(:verbatim "


 (define-object city ()
   :computed-slots
   ((total-water-usage (+ (the hotel water-usage)
                          (the bank water-usage))))
   :objects
   ((hotel :type 'hotel
           :size :large)
    (bank  :type 'bank
           :size :medium)))
"))

       "In this example, "
       (:texttt "hotel")
       " and "
       (:texttt "bank")
       " are presumed to be already (or soon to be) defined as objects themselves, 
which each answer the "
       (:texttt "water-usage")
       " message. The "
       (:emph (:indexed "reference chains"))
       ":"
       (:verbatim "(the hotel water-usage)")
       " and "
       (:verbatim "(the bank water-usage)")
       " provide the mechanism to access messages within the child object instances.

These child objects become instantiated "
       (:emph "on demand")
       ", meaning that the first time these instances or any of their messages
are referenced, the actual instance will be created "
       (:emph "and")
       " cached for future reference.")
      
      
      ((:boxed-figure :caption "Sample Data and Object Definition to Contain U.S. Presidents"
		      :label "fig:object-presidents-container")
       (:verbatim "


 (defparameter *presidents-data*
     '((:name 
        \"Carter\"
        :term 1976)
       (:name \"Reagan\"
        :term 1980)
       (:name \"Bush\"
        :term 1988)
       (:name \"Clinton\"
        :term 1992)))
       
 (define-object presidents-container ()

   :input-slots
   ((data *presidents-data*))

   :objects
   ((presidents :type 'president
                :sequence (:size (length (the data)))
                :name (getf (nth (the-child index) (the data)) :name)
                :term (getf (nth (the-child index) (the data)) :term))))
"))
      
      ((:section :title "Sequences of Objects and Input-slots with a Default Expression")
        "Objects may be "
        (:emph "sequenced")
        (:index "Objects!sequenced")
	(:index "sequences")
	(:index "object sequences")
        ", to specify, in effect, an array or list of object instances. The most
common type of sequence is called a "
        (:emph "fixed size")
        " sequence. See Figure "
        (:ref "fig:object-presidents-container")
        " for an example of an object which contains a sequenced set of 
instances representing U.S. presidents. Each member of the sequenced set 
is fed inputs from a list of plists, which simulates a relational database 
table (essentially a ``list of rows'').
        
Note the following from this example:"
        
        ((:list :style :itemize)
         (:item "In order to sequence an object, the input keyword "
                (:texttt ":sequence")
                " is added, with a list consisting of the keyword "
                (:texttt (:indexed ":size"))
                " followed by an expression which must evaluate to a number.")
         (:item "In the input-slots, "
		(:texttt "data")
		" is specified together with a default expression. Used this way, 
input-slots function as a hybrid of computed-slots and input-slots, allowing a "
                (:emph "default expression")
                " as with computed-slots, but allowing a value to be passed in on 
instantiation or from the parent, as with an input-slot which has no default expression. 
A passed-in value will override the default expression.")))
      
      ((:section :title "Summary")
       "This GDL syntax overview has been kept purposely brief, covering the fundamentals of the language
in a dense manner. On one hand, it is not meant to be a comprehensive language reference; on the other
hand, do not be concerned if you are still unsure about some of the terminology. The upcoming examples 
will revisit and further expand many of the topics covered here, and at some point a coherent picture 
should begin to emerge.

At that point it will be like riding a bicycle, and there will be no going back.

")))

