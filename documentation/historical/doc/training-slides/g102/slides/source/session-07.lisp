(in-package :training-g102)

(define-object session-07 (slide-show-leaf)

  :computed-slots
  ((slide-data 
    `(
      (:title "Face-normal vectors" :bullet-points
	      ((:description "A box or any derivative thereof 
will answer the message <b>:face-normal-vector</b>.")
	       (:description "The <b>:face-normal-vector</b>
message takes an argument of one of the following keywords:
<ul>
<b>
<li>:front</li>
<li>:rear</li>
<li>:left</li>
<li>:right</li>
<li>:top</li>
<li>:bottom</li>
</b>
</li>")
	       (:description "Since the <b>:face-normal-vector</b> takes
an argument, you have to wrap it with parenthesis like a function
call:" :examples ((:code (the (face-normal-vector :top)))))
	       (:description "Normally, <b>:right</b>, <b>:rear</b>, and
<b>:top</b> correspond to the X, Y, and Z axes")))
      (:title "Orientation" 
	      :bullet-points
	      ((:description "You can redefine an object's idea of <b>:right</b>, <b>:rear</b>,
<b>:top</b>, etc, by defining the <b>orientation</b> slot
within the object's definition:" 
			     :examples
			     ((:define-object self-aligning-box)))

	       (:description "<b>:orientation</b> requires a transformation matrix, which is
generally computed by calling the function <i>alignment</i> with the appropriate
keyword arguments")
	       (:description "Note that aligning two (2) axes of a part is sufficient 
to define its orientation, since the third axis will always be the 
<i>cross-vectors</i> of the first two")))
      
      (:title "Exercises" 
	      :bullet-points
	      ((:description "Exercise -- find intersection for Ackermann Assembly")
	       (:description "Skeleton code is available for download"))))))
	      
  :functions
  ((strings-for-display
    nil
    "Orientation")))
