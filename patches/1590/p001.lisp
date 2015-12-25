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


(excl:without-package-locks
  (excl:without-redefinition-warnings
    (defmacro the-object (object &rest reference-chain)
      "Lisp object. Sends the <tt>reference-chain</tt> to <tt>object</tt>, which must be specified
as a Lisp expression (e.g. a variable) which evaluates to a GDL object.

:arguments (reference-chain \"(&rest). A spliced-in list of symbols naming messages, which can 
be slots or objects starting from <tt>object</tt>. For referring to elements of a quantified set,
or for passing arguments to GDL functions which take arguments, use parentheses around the
message name and enclose the quantified element index or function arguments after the message
name.\")

:example 
This example sends the <tt>length</tt> message to the ``zeroth'' element of the quantified
set of arms contained in the body which is contained in the robot which is contained in <tt>object</tt>:
<pre>
 (the-object object robot body (arms 0) length)
</pre>"
      (if reference-chain
	  (let ((first (first reference-chain)) evaluate? apply?)
	    (let ((message (cond ((atom first) (glisp:intern (symbol-name first) :gdl-slots))
				 ((and (symbolp (first first))
				       (not (eql (first first) 'evaluate))) (glisp:intern (symbol-name (first first)) :gdl-slots))
				 ((eql (first first) 'evaluate) (setq evaluate? t) (second first))
				 ((and (consp (first first))
				       (eql (first (first first)) 'evaluate))
				  (setq evaluate? t)
				  (second (first first)))))
		  (args (cond ((and (listp first) (eql (first first) 'evaluate)) nil)
			      ((and (listp first) (listp (first (rest first)))
				    (eql (first (first (rest first))) :apply))
			       (setq apply? t)(second (first (rest first))))
			      ((listp first) (rest first))))
		  (new-object (gensym)))
          
	      `(the-object ,(if *undeclared-parameters-enabled?*
				(if evaluate?
				    `(if (fboundp (glisp:intern (symbol-name ,message) :gdl-slots))
					 (funcall (symbol-function (glisp:intern (symbol-name ,message) :gdl-slots)) ,object ,@args)
					 (let ((,new-object (getf (gdl-slots::%parameters% ,object) 
								  (make-keyword ,message) 'gdl-rule:%not-handled%)))
					   (if (eql ,new-object 'gdl-rule:%not-handled%)
					       (not-handled-error ,object ',message ',args) ,new-object)))
				    `(if (fboundp ',message) (,message ,object ,@args)
					 (let ((,new-object (getf (gdl-slots::%parameters% ,object) 
								  ,(make-keyword message) 'gdl-rule:%not-handled%)))
					   (if (eql ,new-object 'gdl-rule:%not-handled%)
					       (not-handled-error ,object ',message ',args) ,new-object))))
				(cond ((and evaluate? apply?)
				       `(apply (symbol-function (glisp:intern (symbol-name ,message) :gdl-slots)) ,object 
					       ,args))
				      (evaluate?
				       `(funcall (symbol-function (glisp:intern (symbol-name ,message) :gdl-slots)) ,object 
						 ,@args))
				      (apply?
				       `(apply ',message ,object ,args))
                                
				      (t `(,message ,object ,@args))))
                       
			   ,@(rest reference-chain))))
	  object))


    (defun not-handled-error (object message &optional args)
      (if (the-object object root?)
	  (error "~s, which is the root object, could not handle the ~s message~a." 
		 object (make-keyword message)
		 (if args (format nil "~%with args: ~s " args) ""))
	  (error "Neither ~s nor any of its ancestor instances could handle the ~s message~a
The path to the containing object is: ~s" 
		 object (make-keyword message) 
		 (if args (format nil "~%with args: ~s " args) "")
		 (append '(the :root) (symbols-as-keywords
				       (reverse (the-object object :root-path)))))))


    (defun not-handled (object message &optional args)
      (if *error-on-not-handled?*
	  (not-handled-error object message args)
	  'gdl-rule:%not-handled%))

    (defun computed-slots-section (name computed-slots &key query?)
      (mapcan 
       #'(lambda(attribute)
	   (let ((attr-remarks (message-strings attribute))
		 (attr-sym (intern (symbol-name (message-symbol attribute)) :gdl-acc))
		 (attr-expr (message-source-code attribute)))
	     (remove
	      nil
	      (list
	       (when (and name *compile-documentation-database?* attr-remarks)
		 `(when *load-documentation-database?*
		    (let ((ht (or (message-documentation (find-class ',name))
				  (setf (message-documentation (find-class ',name)) (make-hash-table)))))
		      (setf (gethash (make-keyword ',attr-sym) ht) ,attr-remarks))))
	       (when (and name *compile-source-code-database?*)
		 `(when *load-source-code-database?*
		    (let ((ht (or (message-source (find-class ',name))
				  (setf (message-source (find-class ',name)) (make-hash-table)))))
		      (setf (gethash (make-keyword ',attr-sym) ht) ',attr-expr))))

           
	       (when name
		 `(defmethod ,(glisp:intern (symbol-name attr-sym) :gdl-slots) ((self ,name) &rest ,args-arg)
                (declare (ignore ,args-arg))
                (let ((*error-on-not-handled?* t))
		  (with-dependency-tracking (,attr-sym)
		    ,(if query? `(iq:qlet (()) ,attr-expr) attr-expr))
		  #+nil
                  (with-dependency-tracking (,attr-sym)
                    ,(if query? (error "Query-slots are not supported in this distribution of Genworks GDL.")
                       attr-expr)))))

           (when (and *compile-for-dgdl?* (not (string-equal (symbol-name name) "remote-object")))
             `(when (or (not (fboundp ',(glisp:intern attr-sym :gdl-slots)))
                        (not (find-method (symbol-function ',(glisp:intern attr-sym :gdl-slots))
                                          nil (list (find-class 'gdl-remote)) nil)))
                (defmethod ,(glisp:intern attr-sym :gdl-slots) ((,self-arg gdl-remote) &rest ,args-arg)
                  (the-object ,self-arg (send (:apply (cons ,(make-keyword (symbol-name attr-sym)) 
                                                            ,args-arg)))))))
           
	   `(unless (find-method (symbol-function ',(glisp:intern (symbol-name attr-sym) :gdl-slots))
				 nil (list (find-class 'gdl-basis)) nil)
	      (defmethod ,(glisp:intern (symbol-name attr-sym) :gdl-slots) ((,self-arg gdl-basis) &rest ,args-arg)
		;;(declare (ignore ,args-arg))
		(let ((,parent-arg (the-object ,self-arg %parent%)))
		  (if (null ,parent-arg) (not-handled ,self-arg ,(make-keyword attr-sym) ,args-arg)
		      (let ((,val-arg (let (*error-on-not-handled?*)
					(,(glisp:intern (symbol-name attr-sym) :gdl-inputs) 
					  ,parent-arg (the-object ,self-arg :%name%) ,self-arg))))
			(if (eql ,val-arg 'gdl-rule:%not-handled%) (not-handled ,self-arg ,(make-keyword attr-sym) ,args-arg) ,val-arg)))))))))) 
   computed-slots))


    (defun input-slots-section (name input-slots)

      (mapcan 
       #'(lambda(input)
	   (let* ((attr-sym (glisp:intern (symbol-name (if (symbolp input) 
							   input 
							   (message-symbol input))) :gdl-acc))
		  (attr-remarks (when (consp input) (message-strings input))))
	     (remove 
	      nil
	      (list
	       (when (and *compile-documentation-database?* attr-remarks)
		 `(when *load-documentation-database?*
		    (let ((ht (or (message-documentation (find-class ',name))
				  (setf (message-documentation (find-class ',name)) (make-hash-table)))))
		      (setf (gethash (make-keyword ',attr-sym) ht) ,attr-remarks))))
           
           
	       (when (and *compile-for-dgdl?* (not (eql attr-sym 'gdl-acc::type)))
		 `(unless (find-method (symbol-function ',(glisp:intern attr-sym :gdl-inputs))
				       nil (list (find-class 'gdl-remote) t (find-class 'gdl-basis)) nil)
		    (defmethod ,(glisp:intern attr-sym :gdl-inputs) ((,parent-arg gdl-remote) 
								     ,part-arg 
								     (,self-arg gdl-basis))
		  (the-object ,parent-arg (fetch-input ,(make-keyword attr-sym) ,part-arg ,self-arg)))))

           `(defmethod ,(glisp:intern (symbol-name attr-sym) :gdl-slots) ((self ,name) &rest ,args-arg)
              ;;(declare (ignore ,args-arg))
              (let ((*error-on-not-handled?* t))
                (with-dependency-tracking (,attr-sym self)
                  (let ((,parent-arg (the-object self %parent%)))
                    (let ((,val-arg
                           (if ,parent-arg
                               (let (*error-on-not-handled?*)
                                 (,(glisp:intern (symbol-name attr-sym) :gdl-inputs)
                                  ,parent-arg (the-object self %name%) self))
                             'gdl-rule:%not-handled%)))
                      (cond ((not (eql ,val-arg 'gdl-rule:%not-handled%)) ,val-arg)
                            ((and ,parent-arg (fboundp ',(glisp:intern (symbol-name attr-sym) :gdl-trickle-downs)))
                             (if ;;(member ,(make-keyword attr-sym) (the-object ,parent-arg :%trickle-down-slots%))
			      (gethash ,(make-keyword attr-sym) (the-object ,parent-arg :%trickle-down-slots%))
			      (,(glisp:intern (symbol-name attr-sym) :gdl-slots) ,parent-arg)
			      (funcall (symbol-function ',(glisp:intern (symbol-name attr-sym) :gdl-trickle-downs)) ,parent-arg)))
                            (t (not-handled self ,(make-keyword attr-sym) ,args-arg)))))))))))) input-slots))

    
    (defun uncached-function (name function)
      (let* ((attr-remarks (message-strings function))
	     (attr-sym (glisp:intern (symbol-name (message-symbol function)) :gdl-acc))
	     (lambda-list (second (strip-strings function)))
	     (body (rest (rest (strip-strings function))))
	     (has-declare? (and (consp body)
				(consp (first body))
				(eql (first (first body)) 'declare))))
	`(progn
	   ,@(remove 
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
		      (setf (gethash (make-keyword ',attr-sym) ht) ',(cons lambda-list body)))))
           
           
           
	       `(defmethod ,(glisp:intern (symbol-name attr-sym) :gdl-slots) ((self ,name) &rest ,args-arg)
		  (apply #'(lambda ,lambda-list
			     ,(if has-declare? (first body) `(declare))
                         
			     ,(when *compile-circular-reference-detection?*
				    `(when (and *run-with-circular-reference-detection?*
						(member (list self ',attr-sym) *till-now* :test #'equalp))
				       (error "Circular reference detected")))
                         
			     (let ,(remove nil
					   (list (when *compile-circular-reference-detection?*
						   `(*till-now* (when *run-with-circular-reference-detection?*
								  (cons (list self ',attr-sym) *till-now*))))))
			       (block ,(make-keyword attr-sym) 
				 (let ((*error-on-not-handled?* t))
				   ,@(if has-declare? (rest body) body))))) ,args-arg))
           

	       (when (and *compile-for-dgdl?* (not (string-equal (symbol-name name) "remote-object")))
		 `(when (or (not (fboundp ',(glisp:intern attr-sym :gdl-slots)))
			    (not (find-method (symbol-function ',(glisp:intern attr-sym :gdl-slots))
					      nil (list (find-class 'gdl-remote)) nil)))
		    (defmethod ,(glisp:intern attr-sym :gdl-slots) ((,self-arg gdl-remote) &rest ,args-arg)
		      (the-object ,self-arg (send (:apply (cons ,(make-keyword (symbol-name attr-sym)) ,args-arg)))))))
           
	       ;;
	       ;; FLAG -- why is this needed?? 
	       ;;
	       `(when (or (not (fboundp ',(glisp:intern (symbol-name attr-sym) :gdl-slots)))
			  (not (find-method (symbol-function ',(glisp:intern (symbol-name attr-sym) :gdl-slots))
					    nil (list (find-class 'gdl-basis)) nil)))
		  (defmethod ,(glisp:intern (symbol-name attr-sym) :gdl-slots) ((,self-arg gdl-basis) &rest ,args-arg)
		    ;;(declare (ignore ,args-arg))
		    (let ((,parent-arg (the-object ,self-arg %parent%)))
		      (if (null ,parent-arg) (not-handled ,self-arg ,(make-keyword attr-sym) ,args-arg)
			  (,(glisp:intern (symbol-name attr-sym) :gdl-inputs) 
			    ,parent-arg (the-object ,self-arg :%name%) ,self-arg))))))))))

    

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
			     ;;(declare (ignore ,args-arg))
			     (let ((,parent-arg (the-object ,self-arg %parent%)))
			       (if (null ,parent-arg) (not-handled ,self-arg ,(make-keyword key) ,args-arg)
				   (,(glisp:intern key :gdl-inputs) 
				     ,parent-arg (the-object ,self-arg :%name%) ,self-arg)))))) object-input-keys))))))
