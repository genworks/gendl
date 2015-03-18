;;
;; Copyright 2002-2011 Genworks International 
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

(in-package :gdl)


(defun object-inputs-generics-section (cooked-data &key special-keys)
  
  (let ((object-input-keys (remove-duplicates
			    (cons :%parameters%
				  (apply #'append (mapcar #'(lambda(data)
							      (let ((plist-keys (mapcar #'make-keyword (append (plist-keys (getf data :plist))
													       (getf data :with-attribute-keys)))))
								(set-difference plist-keys special-keys))) cooked-data))))))
    `((eval-when (:compile-toplevel :load-toplevel :execute)    
	,@(append 
	   (mapcar #'(lambda(key)
		       `(unless (fboundp ',(glisp:intern key :gdl-inputs))
			  (defgeneric ,(glisp:intern key :gdl-inputs) (,parent-arg ,val-arg ,self-arg)))) object-input-keys)
	   (mapcar #'(lambda(key)
		       `(unless (fboundp ',(glisp:intern key :gdl-slots))
			  (defgeneric ,(glisp:intern key :gdl-slots) (,self-arg &rest ,args-arg)))) object-input-keys)))
      ,@(mapcar #'(lambda(key)
		    `(unless (find-method (symbol-function ',(glisp:intern key :gdl-slots))
					  nil (list (find-class 'gdl-basis)) nil)
		       (defmethod ,(glisp:intern key :gdl-slots) ((,self-arg gdl-basis) &rest ,args-arg)
			 (declare (ignore ,args-arg))
			 (let ((,parent-arg (the-object ,self-arg %parent%)))
			   (if (null ,parent-arg) (not-handled ,self-arg ,(make-keyword key))
			       (,(glisp:intern key :gdl-inputs) 
				 ,parent-arg (the-object ,self-arg :%name%) ,self-arg)))))) object-input-keys))))



(defun objects-section (name objects)
  (let ((special-keys (list  :type :sequence :parameters :pass-down :pseudo-inputs)))
    (let ((cooked-data 
	   (mapcar
	    #'(lambda(part)
          
		(let* ((attr-remarks (message-strings part))
		       (attr-sym (glisp:intern (symbol-name (message-symbol part)) :gdl-acc))
		       (plist (rest (strip-strings part)))
		       (plist-orig plist)
		       (plist (process-radial plist))
		       (plist (process-matrix plist))
		       (part-parameters-expression (getf plist :parameters))
		       (part-type-expr (or (getf plist :type)
					   `',(glisp:intern (symbol-name attr-sym) (symbol-package name))))
		       (part-quantify-expr (getf plist :sequence))
		       (pseudo-inputs (getf plist :pseudo-inputs))
		       (with-attribute-keys (getf plist :pass-down))
		       (part-type-symbol 
			(cond ((and (listp part-type-expr)
				    (eql (first part-type-expr) 'quote)) 
			       (second part-type-expr))
			      ((and (listp part-type-expr)
				    (eql (first part-type-expr) :sequence)
				    (listp (second part-type-expr))
				    (eql (first (second part-type-expr)) 'quote))
			       (second (second part-type-expr)))
			      (t nil))))
		  (list :attr-remarks attr-remarks 
			:attr-sym attr-sym
			:plist plist
			:plist-orig plist-orig
			:part-parameters-expression part-parameters-expression
			:part-type-expr part-type-expr
			:part-quantify-expr part-quantify-expr
			:pseudo-inputs pseudo-inputs
			:with-attribute-keys with-attribute-keys
			:part-type-symbol part-type-symbol))) objects)))
      
      (apply #'append
	     (object-inputs-generics-section cooked-data :special-keys special-keys)
	     (mapcar #'(lambda(data)
			 (apply #'object-specific-methods-section (append data (list :name name :special-keys special-keys)))) cooked-data)))))

(defun object-specific-methods-section (&key  
					attr-remarks 
					attr-sym
					plist
					plist-orig
					part-parameters-expression
					part-type-expr
					part-quantify-expr
					pseudo-inputs
					with-attribute-keys
					part-type-symbol
					name
					special-keys)

  (when (and part-type-symbol 
	     (atom part-type-symbol)
	     (eql part-type-symbol 'remote-object)
	     (not *compile-for-dgdl?*))
    
    (warn "You are compiling an object of type 'remote-object, 
and you have *compile-for-dgdl?* set to nil.

You probably want to do 

   (setq gdl:*compile-for-dgdl?* t)

and then compile this again.~%"))
    
    
  (when (and part-type-symbol 
	     (atom part-type-symbol)
	     (not (eql part-type-symbol 'remote-object)))
    (let* ((all-inputs (all-inputs part-type-symbol))
	   (accepted-inputs (if (listp all-inputs)
				(append all-inputs pseudo-inputs)
				all-inputs)))
      (when (listp accepted-inputs)
	(let* ((accepted-inputs (mapcar #'make-keyword accepted-inputs))
	       (attempted-inputs (mapcar #'make-keyword 
					 (set-difference
					  (append (plist-keys plist-orig) with-attribute-keys)
					  special-keys)))
	       (unrecognized-inputs 
		(safe-sort (set-difference attempted-inputs accepted-inputs)
			   #'string-lessp :key #'string)))
                      
	  
	  ;;
	  ;; FLAG -- do we really need to do this here? We are already
	  ;; checking for non-inputs, and the reserved words will all
	  ;; be non-inputs.
	  ;;
	  (reserved-word-warning-or-error part-type-symbol 
					  (mapcar #'(lambda(symbol)
						      (glisp:intern symbol :gdl-acc))
						  attempted-inputs) (list part-type-symbol))
                      
	  (when unrecognized-inputs 
	    (let* ((singular? (= (length unrecognized-inputs) 1))
		   (s-or-blank (if singular? "" "s"))
		   (a-or-blank (if singular? "a" "")))
	      (warn " (from object specification for ~s)

The following ~a not recognized as any category of defined input-slot for 
the object type '~s'. ~a will be assumed to be ~a pseudo-input~a.

~{ ~s~^~%~}

You can suppress this warning by including ~a name~a in the :pseudo-inputs 
special input keyword for the child object, i.e:

 ...
 :pseudo-inputs (~{~a~^ ~})
 ...


"
		    (make-keyword attr-sym)
		    (if singular? "is" "are")
		    part-type-symbol
		    (if singular? "This" "These")
		    a-or-blank
		    s-or-blank
		    unrecognized-inputs
		    (if singular? "this" "these")
		    s-or-blank
		    unrecognized-inputs
		    )))))))
                       
            
  (append 
   (input-methods name
		  attr-sym
		  (append (cons :%parameters% (plist-keys plist))
			  with-attribute-keys)
		  (append (cons part-parameters-expression 
				(plist-values plist))
			  (mapcar #'(lambda(key)
				      `(the ,key))
				  with-attribute-keys))
		  special-keys)

   (remove
    nil
    (list 
     (when (and *compile-documentation-database?* attr-remarks)
       `(when *load-documentation-database?*
	  (let ((ht (or (message-documentation (find-class ',name))
			(setf (message-documentation (find-class ',name)) (make-hash-table)))))
	    (setf (gethash (make-keyword ',attr-sym) ht) ,attr-remarks))))
     (when *compile-source-code-database?*
       `(when *load-source-code-database?*
	  (let ((ht (or (message-source (find-class ',name))
			(setf (message-source (find-class ',name)) (make-hash-table)))))
	    (setf (gethash (make-keyword ',attr-sym) ht) ',plist))))
                    
     `(defmethod ,(glisp:intern (symbol-name attr-sym) :gdl-slots) ((self ,name) &rest ,args-arg)
	(cond ((null ,args-arg)
	       (with-dependency-tracking (,attr-sym)
		 ,(cond 
		   ((null part-quantify-expr)
                              
		    `(if *retain-object-identity-on-type-change?*
			 (instantiate-part-on-demand self ,part-type-expr ',attr-sym)
			 (make-object-internal ,part-type-expr 
					       :%name% (list ',attr-sym nil t)
					       :%parent% (list self nil t)
					       :%root% (gdl-acc::%root% self))))

		   ((eql (first part-quantify-expr) 
			 :size)
		    `(make-object-internal 'standard-sequence
					   :%name% (list ',attr-sym nil t)
					   :%parent% (list self nil t)
					   :%root% (gdl-acc::%root% self)))
		   ((eql (first part-quantify-expr) :radial)
		    `(make-object-internal 'radial-sequence
					   :%name% (list ',attr-sym nil t)
					   :%parent% (list self nil t)
					   :%root% (gdl-acc::%root% self)))
                            
		   ((eql (first part-quantify-expr) :matrix)
		    `(make-object-internal 'matrix-sequence
					   :%name% (list ',attr-sym nil t)
					   :%parent% (list self nil t)
					   :%root% (gdl-acc::%root% self)))
                            
		   ((eql (first part-quantify-expr) 
			 :indices)
		    `(make-object-internal 'variable-sequence
					   :%name% (list ',attr-sym nil t)
					   :%parent% (list self nil t)
					   :%root% (gdl-acc::%root% self)))
                             
		   (t (error "For a sequence object, you must specify one of :size, :radial, :matrix, or :indices as the first
item in the list following the :sequence keyword")))))

	      ((null (rest ,args-arg))
	       (the-object self ,attr-sym (:get-member (first ,args-arg))))
                        
	      (t
	       (the-object self ,attr-sym (:get-member (first ,args-arg)
						       (second ,args-arg))))))
               
     (when (and *compile-for-dgdl?* (not (string-equal (symbol-name name) "remote-object")))
       `(when (or (not (fboundp ',(glisp:intern attr-sym :gdl-slots)))
		  (not (find-method (symbol-function ',(glisp:intern attr-sym :gdl-slots))
				    nil (list (find-class 'gdl-remote)) nil)))
	  (defmethod ,(glisp:intern attr-sym :gdl-slots) 
	      ((,self-arg gdl-remote) &rest ,args-arg)
	    (the-object ,self-arg (send (:apply (cons ,(make-keyword (symbol-name attr-sym)) 
						      ,args-arg)))))))
               
     (when (member (first part-quantify-expr) '(:size :radial))
       `(defmethod gdl-inputs::number-of-elements ((self ,name) (,part-arg (eql ',attr-sym)) (child t))
	  ,(second part-quantify-expr)))

     (when (member (first part-quantify-expr) '(:size :radial :matrix))
       `(defmethod gdl-inputs::child-types ((self ,name) (,part-arg (eql ',attr-sym)) (child t))
	  ,(if (eql (first part-type-expr) :sequence)
	       (second part-type-expr)
	       `(list ,part-type-expr))))
               
               
     (when (eql (first part-quantify-expr) :indices)
       `(defmethod gdl-inputs::element-index-list ((self ,name) (,part-arg (eql ',attr-sym)) (child t))
	  ,(second part-quantify-expr)))

     (when (eql (first part-quantify-expr) :indices)
       `(defmethod gdl-inputs::child-types ((self ,name) (,part-arg (eql ',attr-sym)) (child t))
	  ,part-type-expr))

               
     (when (eql (first part-quantify-expr) :matrix)
       `(defmethod gdl-inputs::number-of-elements ((self ,name) (,part-arg (eql ',attr-sym)) (child t))
	  (list ,@(rest part-quantify-expr))))
               
     ))))


(defun process-radial (plist)
  (if (and (getf plist :sequence) (eql (first (getf plist :sequence)) :radial))
      (convert-to-radial plist)
    plist))

(defun convert-to-radial (plist)
  (declare (ignore plist))
  (error "Radial sequence is not supported in this GDL distribution."))


(defun process-matrix (plist)
  (if (and (getf plist :sequence) (eql (first (getf plist :sequence)) :matrix))
      (convert-to-matrix plist)
    plist))

(defun convert-to-matrix (plist)
  (declare (ignore plist))
  (error "Matrix sequence is not supported in this GDL distribution."))






                                                
        
        
                          
        
                            
  
  
           
