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

(define-object control (slide-show-leaf)
  
  :computed-slots
  ((strings-for-display "Control")
   (:slide-data 
    '((:title "<i>If</i> revisited"
	      :bullet-points
	      ((:description "<i>If test then-form else-form</i>")
	       (:description "<i>If</i> can take only a single <i>then-form</i> and <i>else-form</i>")
	       (:description 
		"Use <i>progn</i> to group several expressions within a form<br>
<i>Note: a progn form returns the return-value of the last form.<br>
prog1 is also available, which returns the return-value of the first form</i>."
		:examples
		((:code 
		  (if (eql status :all-systems-go)
		      (progn (broadcast-countdown)
			     (flash-colored-lights)
			     (play-eerie-music)
			     (launch-rocket))
		    (progn (broadcast-apology)
			   (shut-down-rocket)))
		  :return-value "Rocket has been launched")
		 (:code 
		  (if (eql status :all-systems-go)
		      (prog1 (broadcast-countdown)
			(flash-colored-lights)
			(play-eerie-music)
			(launch-rocket))
		    (prog1 (broadcast-apology)
		      (shut-down-rocket)))
		  :return-value "Countdown has been broadcast")))))
	  
      
      (:title "When"
       :bullet-points
       ((:description "If there is no <i>else-form</i>, Use <i>When</i> instead of <i>If</i>")
	
	(:description "Unlike <i>If</i>, <i>When</i> can take any number of forms (like an implicit <i>progn</i>)")
	(:description "The return-value from the last form is returned from the <i>When</i> form (as with <i>progn</i>."
	 
	 :examples
	 ((:code
	   (when (eql database-connection :active)
	     (read-record-from-database)
	     (chew-on-data-from-database)
	     (calculate-final-result))
	   :return-value 999)))

	(:description "<i>When</i> simply returns nil if the condition is not met</i>"
	 :examples
	 ((:code
	   (when (> 3 4) (princ "I don't think so..."))
	   :return-value nil)))))
      
      
      (:title
       "Logical Operators"
       :bullet-points
       ((:description
	 "The symbol t is the default representation for True.")
	(:description
	 "The symbol nil is the default representation for False."
	 :examples
	 ((:code
	   (listp '(ti intel motorola))
	   :return-value
	   t)
	  (:code
	   (listp 3.1415)
	   :return-value
	   nil)
	  (:code
	   (> 3 4)
	   :return-value
	   nil)))))
      
      
      (:title
       "Logical Operators, cont'd"
       :bullet-points
       ((:description
	 "Any non-<i>nil</i> value is ``as good as'' <i>t</i>."
	 :examples
	 ((:code
	   (member 'boston '(chicago detroit boston new-york))
	   :return-value (boston new-york))
	  (:code
	   (when (member 'boston '(chicago detroit boston new-york))
	     '(yes it is))
	   :return-value (yes it is))))))
      
     
      (:title
       "And, Or, Not"
       :bullet-points
       ((:description
	 "and form1 form2 ... formn"
	    
	 :examples
	 ((:code 
	   (listp '(chicago detroit boston new-york))
	   :return-value t)
	     
	  (:code 
	   (listp 3.1415)
	   :return-value nil)
	     
	  (:code
	   (and (listp '(chicago detroit boston new-york)) (listp 3.1415))
	   :return-value
	   nil)))
	    
	(:description
	 "or form1 form2 ... formn"
	 :examples
	 ((:code
	   (or (listp '(chicago detroit boston new-york)) (listp 3.1415))
	   :return-value
	   t)))
	   
	(:description
	 "not form1"
	 :examples
	 ((:code
	   (not nil)
	   :return-value
	   t)
	     
	  (:code
	   (not (listp 3.1415))
	   :return-value t)))))
     

      
      (:title "Cond"
       :bullet-points
       ((:description "The use of nested <i>If</i>'s is not appropriate")
	(:description "Use <i>cond</i> instead.")
	(:description "<i>cond test-result-list-1 test-result-list-2 ... test-result-list-n</i>"
	 :examples
	 ((:code
	   (let ((n 4))
	     (cond ((> n 10) "It's a lot")
		   ((= n 10) "It's kind of a lot")
		   ((< n 10) "It's not a lot")))
	   :return-value "It's not a lot")))
	(:description "If you want the last condition do default if all else fails, use <i>t</i>"
	 :examples
	 ((:code
	   (let ((n 4))
	     (cond ((> n 10) "It's a lot")
		   ((= n 10) "It's kind of a lot")
		   (t "It's not a lot")))
	   :return-value "It's not a lot")))))
      
      
      (:title "Case"
       :bullet-points
       ((:description "If you want to conditionalize based on comparing a value against some series of constants, use <i>case</i>")
	(:description "Case only works if the value will match with <i>eql</i>, i.e. 
use it for keywords, symbols, integers, but not for strings, lists, or floating-point numbers. "
	 :examples 
	 ((:code
	   (let ((color :red))
	     (case color
	       (:blue "Blue is okay")
	       ((:red :orange)  "Red or orange are actually her favorite colors")
	       (:green "Are you nuts?")))
	   :return-value "Red is actually her favorite color")))
	
	
	(:description "For matching strings, you  have to use <i>cond</i> "
	 :examples 
	 ((:code
	   (let ((color "red"))
	     (cond ((string-equal color "blue")
		    "Blue is okay")
		   ((string-equal color "red")
		    "Red is actually her favorite color")
		   ((string-equal color "green")
		    "Are you nuts?")))
	   
	   :return-value  "Red is actually her favorite color")))
	
	
	(:description "Use <i>otherwise</i> as a catch-all for <i>case</i>"
	 :examples 
	 ((:code
	   (let ((color :orange))
	     (case color
	       (:blue "Blue is okay")
	       (:red  "Red is actually her favorite color")
	       (:green "Are you nuts?")
	       (otherwise "We have never heard of that!")))
	   :return-value "We have never heard of that!")))
	
	(:description "Use <i>ecase</i> to throw an error for an invalid case</i>"
	 :examples 
	 ((:code
	   (let ((color :orange))
	     (ecase color
	       (:blue "Blue is okay")
	       (:red  "Red is actually her favorite color")
	       (:green "Are you nuts?")))
	   :return-value "Error: :orange fell through a ecase form.  
The valid cases were 
:blue, :red, and :green.")))))
      
      
      (:title "Iteration revisited"
       :bullet-points
       ((:description "Dolist, Dotimes, Mapcar, Mapc")
	(:description "I tend to stay away from Loop and Do")
	(:description "Mapcar is often much less verbose than Dolist"
	 :examples
	 ((:code
	   (defun twice (num) (* num 2))
	   :return-value twice)
	  
	  (:code
	   (let ((result-list nil))
	     (dolist (elem '(3 4 8 2) result-list)
	       (setq result-list (append result-list (list (twice elem))))))
	   :return-value (6 8 16 4))
	  
	  (:code
	   (mapcar #'twice '(3 4 8 2))
	   :return-value (6 8 16 4))))
	
	(:description "You can return early from <i>dolist</i> or <i>dotimes</i> with <i>return</i>"
	 :examples
	 ((:code
	   (dolist (elem '(3 4 55 2) result-list)
	     (if (< elem 50)
		 (setq result-list (append result-list (list (twice elem))))
	       (return "we cannot handle numbers bigger than 50")))
	   :return-value "we cannot handle numbers bigger than 50")))))
      
      (:title "Exercises"
       :bullet-points
       ((:description "We will hold off on exercises until we have learned more about <i>Functions</i> in the next session.")))))))
