(in-package :gdl-user)


(setq *run-with-dependency-recording?* t)


(defparameter *pairs-hash* nil)


(define-object dep-test ()
  
  :computed-slots ((a (+ (the b) (the c)))
		   
		   (b (+ (the c) (the d)))
		   
		   (c (* (the d) (random 10)))
		   
		   (d (the (kid-1 0) num)))
  
  
  :objects ((kid-1 :type 'kid
		   :sequence (:size 1)
		   :num 42)))


(define-object kid ()
  
  :input-slots (num))




(define-object dep (base-object)
  
  :input-slots ((instance (make-object 'dep-test))
		(message :a)
		
		(force-evaluation? t))
  
  
  :computed-slots 
  ((strings-for-display (format nil "~s" 
				(cons 'the (append (reverse (the instance root-path))
							    (list (the message))))))
		   
		   
   (dependencies-list (progn (when (the force-evaluation?)
			       (the instance (evaluate (the message))))
			     (gdl::find-dependencies (the instance) (the message))))
		   
   (dependants-list (progn (when (the force-evaluation?)
			     (the instance (evaluate (the message))))
			   (gdl::find-dependants (the instance) (the message))))
		   
		   
   (dependency-pairs 
    (let ((*pairs-hash* (make-hash-table :test #'equalp :values nil)))
      (the %dependency-pairs%)))
      
   (%dependency-pairs%
    (append (mapcar #'(lambda(dependency)
			(list (the strings-for-display)
			      (the-object dependency 
					  strings-for-display)))
		    (list-elements (the dependencies)))
	    (let (result (new-children 
			  (list-elements (the dependencies))))
	      (setq result
		(dolist (child new-children (nreverse result))
		    
		    
		  (print-variables (the strings-for-display))
		    
		  (unless (gethash (the strings-for-display) *pairs-hash*)
		    (setf (gethash (the strings-for-display) *pairs-hash*) t)
		    (push child result))))
	      (apply #'append (mapsend result :%dependency-pairs%))))))

  
  :objects ((dependencies :type 'dep
			  :display-controls (list :color :green)
			  :sequence (:size (length (the dependencies-list)))
			  :instance (first (nth (the-child index) (the dependencies-list)))
			  :message (second (nth (the-child index) (the dependencies-list))))

	    (dependants :type 'dep
			:hidden? t
			:display-controls (list :color :red)
			:sequence (:size (length (the dependants-list)))
			:instance (first (nth (the-child index) (the dependants-list)))
			:message (second (nth (the-child index) (the dependants-list))))))



(define-format dot (geom-base::2d-output))



(define-lens (dot dep)()
  :output-functions
  ((digraph
    ()
    
    (format *stream* "digraph G {
")
    
    (dolist (pair (the dependency-pairs))
      (format *stream* "\"~a\" -> \"~a\"~%" (first pair) (second pair)))
    
    (format *stream* "}
"))))
        
    
