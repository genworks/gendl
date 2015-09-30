(in-package :tasty)

(#+allegro 
 excl:without-package-locks #-allegro progn
 (#+allegro 
  excl:without-redefinition-warnings #-allegro progn
 
  (define-object-amendment value-inspector ()
  
    :input-slots ((value (with-error-handling () 
			   (let (gdl::*notify-cons*)
			     (the parent-node (evaluate (the message)))))))
    :functions
    ((get-value-element
      (index)
      (case (the value-type)
	(:list (nth index (the value)))
	(:gdl-sequence (let (gdl::*notify-cons*) (the value (get-member index))))))))))
  
