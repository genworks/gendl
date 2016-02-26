(in-package :gdl)

(excl:without-package-locks
  (excl:without-redefinition-warnings
    (defun chase-up-trickle-down (slot-symbol self args)
      (let ((keyword (make-keyword slot-symbol)))
	(let ((parent (the-object self parent)))
	  (if (null parent) (not-handled self keyword)
	      (let ((result
		     (let (*error-on-not-handled?*)
		       (funcall (symbol-function (glisp:intern slot-symbol :gdl-inputs)) parent (the %name%) self))))
		(if (eql result 'gdl-rule:%not-handled%)
		    (let ((result
			   (let (*error-on-not-handled?*)
			     (cond ( ;;(member keyword (the-object parent %trickle-down-slots%))
				    (gethash keyword (the-object parent %trickle-down-slots%))
				    (apply (symbol-function slot-symbol) parent args))
				   (t (apply (symbol-function (glisp:intern slot-symbol :gdl-trickle-downs)) parent args))))))
		      (if (eql result 'gdl-rule:%not-handled%) (not-handled self keyword args) result))
		    result))))))

    (defun trickle-down-basis (self slot args)
      (let ((keyword (make-keyword slot)))
	(let (*error-on-not-handled?* (parent (the parent)))
	  (cond ((null parent) (not-handled self keyword args))
		( ;;(member keyword (the-object parent %trickle-down-slots%))
		 (gethash keyword (the-object parent %trickle-down-slots%))
		 (apply (symbol-function (glisp:intern slot :gdl-slots)) parent args))
		(t (apply (symbol-function (glisp:intern slot :gdl-trickle-downs)) parent args))))))))

	    
