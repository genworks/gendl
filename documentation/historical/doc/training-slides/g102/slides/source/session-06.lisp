(in-package :training-g102)

(define-object session-06 (slide-show-leaf)

  :computed-slots
  ((slide-data '((:title "Exercises" :bullet-points
		  ((:description "Complete the provided <i>city</i> object
to accept a list of <i>zoning ordinances</i> which 
contain <i>proximity rules</i>, as in the skeleton code." )
		   (:description "Skeleton code is provided for download")
		   
		   (:description "<i>Extra Credit</i>:<br>

Plot a large <b>:red</b> sphere at the center of each violating neighbor")
		   
		   (:description "<i>Extra Credit 3</i>:<br>
Prepare a textual report listing the coordinates of the offending buildings and
other details of the violated rule."))))))

  :functions
  ((strings-for-display
    nil
    "Exercise with Proximity Rules")))
