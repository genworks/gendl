(in-package :training-g102)

(define-object session-04 (slide-show-leaf)

  :computed-slots
  ((slide-data `((:title "Objects" :bullet-points
			 ((:description "Child instances")
			  (:description "Roughly analogous to calls of one defun from  within another Defun")
			  (:description 
			   "Break complicated computation into smaller pieces and ``distribute'' complexity"
			   :examples ((:define-object city)))))

		 
		 (:title "Passing <i>:inputs</i> to a child object using a plist" 
			 :bullet-points
			 ((:description "Except for its <i>:Type</i>, all inputs to a child can
be passed in using a plist and the special keyword <i>:Parameters</i> 
 (assuming you have available a plist of the child's parameters).
<p>
Note: <i>The </i>:Type<i> should always be specified explicitly 
 (although it will default to the child object's name if not given)</i>" :examples
 ((:define-object city-2)))))

		 (:title "Sequences of Objects" 
			 :bullet-points
			 ((:description "You can specify an <i>Array</i> of child object using <i>:sequence
quantification</i>")
			  
			  (:description "Example" 
					:examples
					((:define-object city-3)))))
		 
		 )))

  :functions
  ((strings-for-display
nil
"Child Objects")))
