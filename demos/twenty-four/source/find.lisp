(in-package :www.genworks.com)

(defparameter *operators* '(+ - * /))


(defun all-permutations (lst &optional (remain lst))
  (cond ((null remain) nil)
        ((null (rest lst)) (list lst))
        (t (append
            (mapcar (lambda (l) (cons (first lst) l))
                    (all-permutations (rest lst)))
            (all-permutations (append (rest lst) (list (first lst))) (rest remain))))))



(defparameter *a* nil)
(defparameter *b* nil)
(defparameter *c* nil)
(defparameter *d* nil)

(defparameter *all-exprs* nil)

(defun twenty-four (a b c d &key (target 24))
  
  (let (all-results (all-perms (all-permutations (list a b c d))))
    (dolist (perm all-perms)
      (dolist (expression *all-exprs*)
	(let ((*a* (first perm))
	      (*b* (second perm))
	      (*c* (third perm))
	      (*d* (fourth perm)))
	  (let ((subst-plist (list '*a* *a* '*b* *b* '*c* *c* '*d* *d*)))
	    (when (eql (ignore-errors (eval expression)) target)
	      (pushnew 
		(subst-recursive expression subst-plist) all-results :test #'equalp))))))
    
    (let (whole-plus whole-minus fractions)
      (dolist (result all-results)
	(if (whole-intermediates result)
	    (if (plus-minus result)
		(push result whole-plus)
		(push result whole-minus))
	    (push result fractions)))
      (list :whole-plus (nreverse
			 (mapcar #'(lambda(expression)
				     (infpre::prefix->infix expression '(+ - * /))) whole-plus))
	    :whole-minus (nreverse
			  (mapcar #'(lambda(expression)
				      (infpre::prefix->infix expression '(+ - * /))) whole-minus))
	    :fractions (nreverse
			(mapcar #'(lambda(expression)
				    (infpre::prefix->infix expression '(+ - * /))) fractions))))))



(defun plus-minus (expression)
  (or (null expression)
      (and (plusp (eval expression))
	   (or (not (consp expression))
	       (not (consp (rest expression)))
	       (every #'plus-minus (rest expression))

	       ))))

(defun whole-intermediates (expression)
  (or (null expression)
      (and (integerp (eval expression))
	   (or (not (consp expression))
	       (not (consp (rest expression)))
	       (every #'whole-intermediates (rest expression))))))
    
	    

(defun flat-exprs ()
  (let (result)
    (dolist (operator-1 *operators* result)
      (dolist (operator-2 *operators*)
	(dolist (operator-3 *operators*)
	  (push (list operator-1 
		      (list operator-2 '*a* '*b*)
		      (list operator-3 '*c* '*d*)) result))))))
		      


(define-object twenty-four (base-object)

  :input-slots ((nums (list '*a* '*b* '*c* '*d*))
		(operator nil)
		(so-far nil))

  :computed-slots ((strings-for-display (format nil "~a" (the so-far)))
		   (all-exprs (mapsend (the leaves) :so-far))
		   

		   (all-24s (remove-if-not #'(lambda(expr)
					       (eql (ignore-errors (eval expr)) 24))
					   (the all-exprs))))
  
  :objects
  ((next-exprs :type 'twenty-four
	       :sequence (:size (if (the nums) (length *operators*) 0))
	       :operator (nth (the-child index) *operators*)
	       :so-far (if (null (the so-far))
			   (first (the nums))
			   (append (list (the-child operator)
					 (first (the nums)))
				   (when (the so-far) (list (the so-far)))))
	       :nums (rest (the nums)))))


(setq *all-exprs* (append (the-object (make-object 'twenty-four) all-exprs) (flat-exprs) ))



(defun subst-recursive (list subst-plist)
  (if (atom list)
      (transform list subst-plist)
      (cons (subst-recursive (first list) subst-plist)
	    (mapcar #'(lambda(item)
			(subst-recursive item subst-plist)) (rest list)))))


(defun transform (atom subst-plist)
  (getf subst-plist atom atom))


#+nil
(defun twenty-four (a b c d)
  
  (let (all-results (all-perms (all-permutations (list a b c d))))
    (dolist (perm all-perms)
      (let ((result
	     (let ((self (make-object 'twenty-four :nums perm)))
	       (the all-24s))))
	(setq all-results (append all-results result))))

    (setq all-results (remove-duplicates all-results :test #'equalp))
    
    ;;(mapcar #'(lambda(result) (infpre::prefix->infix result '(+ - * /))) all-results)

    all-results

    #+nil
    (dolist (result (mapcar #'(lambda(result) (infpre::prefix->infix result '(+ - * /))) all-results))
      (format t "~s~%" result))))

#+nil
(define-object twenty-four (base-object)

  :input-slots ((nums (list 10 9 9 2)) 
		(operator nil)
		(so-far nil))

  :computed-slots ((strings-for-display (format nil "~s" (the so-far)))
		   (all-exprs (mapsend (the leaves) :so-far))
		   

		   (all-24s (remove-if-not #'(lambda(expr)
					       (eql (ignore-errors (eval expr)) 24))
					   (the all-exprs))))
  
  :objects
  ((next-exprs :type 'twenty-four
	       :sequence (:size (if (the nums) (length *operators*) 0))
	       :operator (nth (the-child index) *operators*)
	       :so-far (append (list (the-child operator)
				     (first (the nums)))
			       (when (the so-far) (list (the so-far))))
	       :nums (rest (the nums)))))
	       


    
