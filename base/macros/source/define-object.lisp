;;
;; Copyright 2012 Genworks International
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

(defun make-rule-symbol (symbol) (glisp:intern (symbol-name symbol) :gdl-acc))

(defun compose (&rest functions)
  (destructuring-bind (function-1 . rest) (reverse functions)
    #'(lambda(&rest args)
        (reduce #'(lambda(v f) (funcall f v))
                rest
                :initial-value (apply function-1 args)))))

(defun make-accessible-slots (keys &key inputs?)
  (mapcar #'(lambda(key) `(,key :accessor ,key :initform 'gdl-rule::%unbound%
                                ,@(when inputs? `(:initarg ,(make-keyword key)))))
          keys))

(defmacro with-gdl-message-symbols ((&rest specs) &rest body)
  `(let ((messages (mapcan #'(lambda(section)
                               (mapcar #'(lambda(item)
                                           (make-rule-symbol (if (symbolp item) item (first-symbol item))))
                                       (remove-if-not #'(lambda(item)
                                                          (or (consp item) (symbolp item))) section)))
                           (list input-slots computed-slots objects hidden-objects functions methods)))

         (required-input-slot-syms (mapcar #'make-rule-symbol (remove-if-not #'symbolp input-slots)))
        
         (optional-input-slot-syms (mapcar (compose #'make-rule-symbol #'first-symbol)
                                           (remove-if-not #'(lambda(slot) 
                                                              (and (consp slot) (null (rest (rest (strip-strings slot)))))) 
                                                          input-slots)))
        
         (defaulted-input-slot-syms (mapcar (compose #'make-rule-symbol #'first-symbol)
                                            (remove-if-not 
                                             #'(lambda(slot) 
                                                 (and (consp slot) 
                                                      (member :defaulting (rest (rest (strip-strings slot))))
                                                      (not (member :settable (rest (rest (strip-strings slot)))))))
                                             input-slots)))
        
         (settable-input-slot-syms (mapcar (compose #'make-rule-symbol #'first-symbol)
                                           (remove-if-not 
                                            #'(lambda(slot) 
                                                (and (consp slot) 
                                                     (member :settable (rest (rest (strip-strings slot))))
                                                     (not (member :defaulting (rest (rest (strip-strings slot)))))))
                                            input-slots)))
        
        
         (settable-defaulted-input-slot-syms (mapcar (compose #'make-rule-symbol #'first-symbol)
                                                     (remove-if-not 
                                                      #'(lambda(slot) 
                                                          (and (consp slot) 
                                                               (member :settable (rest (rest (strip-strings slot))))
                                                               (member :defaulting (rest (rest (strip-strings slot))))))
                                                      input-slots)))
        
         (computed-slot-syms (mapcar (compose #'make-rule-symbol #'first-symbol)
                                     (remove-if-not #'(lambda(slot) (and (consp slot) (null (rest (rest (strip-strings slot)))))) 
                                                    computed-slots)))
        
         (query-slot-syms (mapcar (compose #'make-rule-symbol #'first-symbol) query-slots))
        
         (trickle-down-slot-syms (mapcar #'make-rule-symbol (remove-if-not #'symbolp trickle-down-slots)))
        
         (settable-computed-slot-syms (mapcar (compose #'make-rule-symbol #'first-symbol)
                                              (remove-if-not #'(lambda(slot)
                                                                 (and (consp slot) 
                                                                      (member :settable (rest (rest (strip-strings slot))))))
                                                             computed-slots)))
         
         (uncached-computed-slot-syms (mapcar (compose #'make-rule-symbol #'first-symbol)
                                              (remove-if-not #'(lambda(slot)
                                                                 (and (consp slot) 
                                                                      (member :uncached (rest (rest (strip-strings slot))))))
                                                             computed-slots)))
         
         
        
         (function-syms (mapcar (compose #'make-rule-symbol #'first-symbol) 
                                (remove-if #'(lambda(function)
                                               (member (second (strip-strings function)) 
                                                       (list :cached :cached-eql :cached-= :cached-eq 
                                                             :cached-equal :cached-equalp)))
                                           functions)))

         (method-syms (mapcar (compose #'make-rule-symbol #'first-symbol) 
                              (remove-if #'(lambda(method)
                                             (member (second (strip-strings method)) 
                                                     (list :cached :cached-eql :cached-= :cached-eq 
                                                           :cached-equal :cached-equalp))) methods)))

        
         (cached-function-syms (mapcar (compose #'make-rule-symbol #'first-symbol)
                                       (remove-if-not #'(lambda(function)
                                                          (member (second (strip-strings function)) 
                                                                  (list :cached :cached-eql :cached-= :cached-eq 
                                                                        :cached-equal :cached-equalp)))
                                                      functions)))

         (cached-method-syms (mapcar (compose #'make-rule-symbol #'first-symbol)
                                     (remove-if-not #'(lambda(method)
                                                        (member (second (strip-strings method)) 
                                                                (list :cached :cached-eql :cached-= :cached-eq 
                                                                      :cached-equal :cached-equalp)))
                                                    methods)))
         
         
         (object-syms 
          (mapcar (compose #'make-rule-symbol #'first-symbol) 
                  (remove-if #'(lambda(part) 
                                 (intersection (list :quantify :sequence)
                                               (plist-keys (rest (strip-strings part))))
                                 ) 
                             objects)))
        
         (quantified-object-syms 
          (mapcar (compose #'make-rule-symbol #'first-symbol)
                  (remove-if-not #'(lambda(part) 
                                     (intersection (list :quantify :sequence)
                                               (plist-keys (rest (strip-strings part))))
                                     ) 
                                 objects)))
        
         (hidden-object-syms 
          (mapcar (compose #'make-rule-symbol #'first-symbol) 
                  (remove-if #'(lambda(part) 
                                 (intersection (list :quantify :sequence)
                                               (plist-keys (rest (strip-strings part))))
                                 ) 
                             hidden-objects)))
        
         (quantified-hidden-object-syms 
          (mapcar (compose #'make-rule-symbol #'first-symbol)
                  (remove-if-not #'(lambda(part) 
                                     (intersection (list :quantify :sequence)
                                               (plist-keys (rest (strip-strings part))))
                                     )
                                 hidden-objects)))
         

	 ,@(when (getf specs :new)
             `((old-message-keys '+old-message-keys+)
               (new-message-keys '+new-message-keys+)
               (symbol-arg '+symbol-arg+)
               (symbols-arg '+symbols-arg+)
               (non-settables-arg '+non-settables-arg+)
               (ht-arg '+ht-arg+)
               (message-arg '+message-arg+)
               (depth-arg '+depth-arg+)
               (category-arg '+category-arg+)
               (message-type-arg '+message-type-arg+)
               (base-part-type-arg '+base-part-type-arg+)
               (class-arg '+class-arg+)))


	 
         ;;,@
	 #+nil
	 (when (getf specs :new)
             `((old-message-keys (gensym))
               (new-message-keys (gensym))
               (symbol-arg (gensym))
               (symbols-arg (gensym))
               (non-settables-arg (gensym))
               (ht-arg (gensym))
               (message-arg (gensym))
               (depth-arg (gensym))
               (category-arg (gensym))
               (message-type-arg (gensym))
               (base-part-type-arg (gensym))
               (class-arg (gensym)))))
     ,@body))
  


(defmacro define-object-amendment (name mixin-list &rest spec-plist)
  "Supplements or alters an existing GDL object definition. Syntax is similar to that for <tt>define-object</tt>.
Note that there is currently no way to undefine messages defined with this macro, other than redefining the 
original object or restarting the GDL session. Support for surgically removing messages will be added in a
future GenDL release."  
  `(%define-object-amendment% ,name ,mixin-list ,@(merge-common-keys spec-plist)))


(defparameter *class-used-by-hash* (make-hash-table :size 100))

(defparameter *message-list-hashes* (make-hash-table :size 100))

(defun evaluate-if-handled (object message &rest args)
  (let* ((type (the-object object type))
	 (message-hash (or (gethash type *message-list-hashes*)
			   (setf (gethash type *message-list-hashes*)
				 (let* ((messages (the-object object message-list))
					(hash (glisp:make-sans-value-hash-table :size (length messages))))
				   (dolist (message messages hash) (setf (gethash message hash) t)))))))
    (let ((handled? (gethash (gdl::make-key message) message-hash)))
      (cond ((and handled? args) (the-object object ((evaluate message) (:apply args))))
	    (handled? (the-object object (evaluate message)))
	    (t (values nil :not-handled))))))


(defmacro %define-object-amendment% (name mixin-list 
                                     &key input-slots 
                                       computed-slots
                                       objects
                                       hidden-objects
                                       functions
                                       methods
                                       documentation
                                       trickle-down-slots
                                       query-slots)

  (check-syntax name input-slots computed-slots objects hidden-objects 
                functions methods documentation trickle-down-slots query-slots)

  (with-gdl-message-symbols ()
    
    
    (let ((message-ht (make-hash-table :size (length messages))))
      (dolist (message messages)
        (push message (gethash message message-ht)))
      (let ((duplicates (let (duplicates)
                          (maphash #'(lambda(key val)
                                       (if (consp (rest val)) 
                                           (push key duplicates))) message-ht) duplicates)))
        (let ((duplicates (remove-if #'(lambda(dup) (member dup method-syms)) duplicates)))
          (when duplicates
            (error "duplicate slot name~p: ~{~a~^, ~}" 
                   (length duplicates) (sort duplicates #'string-lessp :key #'symbol-name))))))


    (let* ((class (find-class name))
           (new-messages (remove-duplicates (append (messages class) messages)))
           (new-computed-slots (remove-duplicates (append (computed-slots class) computed-slot-syms)))
           (new-required-input-slots (remove-duplicates (append (required-input-slots class) required-input-slot-syms)))
           (new-optional-input-slots (remove-duplicates (append (optional-input-slots class) optional-input-slot-syms)))
           (new-defaulted-input-slots (remove-duplicates (append (defaulted-input-slots class) defaulted-input-slot-syms)))
           (new-query-slots (remove-duplicates (append (query-slots class) query-slot-syms)))
           (new-settable-computed-slots 
            (remove-duplicates (append (settable-computed-slots class) settable-computed-slot-syms)))
           (new-uncached-computed-slots 
            (remove-duplicates (append (uncached-computed-slots class) uncached-computed-slot-syms)))
           (new-settable-optional-input-slots
            (remove-duplicates (append (settable-optional-input-slots class) settable-input-slot-syms)))
           (new-settable-defaulted-input-slots 
            (remove-duplicates (append (settable-defaulted-input-slots class) settable-defaulted-input-slot-syms)))
           (new-functions (remove-duplicates (append (functions class) function-syms)))
           (new-methods (remove-duplicates (append (methods class) method-syms)))
           (new-cached-methods (remove-duplicates (append (cached-methods class) cached-method-syms)))
           (new-cached-functions (remove-duplicates (append (cached-functions class) cached-function-syms)))
           (new-objects (remove-duplicates (append (objects class) object-syms)))
           (new-quantified-objects (remove-duplicates (append (quantified-objects class) quantified-object-syms)))
           (new-hidden-objects (remove-duplicates (append (hidden-objects class) hidden-object-syms)))
           (new-quantified-hidden-objects 
            (remove-duplicates (append (quantified-hidden-objects class) quantified-hidden-object-syms)))
           (new-trickle-down-slots
            (remove-duplicates 
             (append (trickle-down-slots class)
                     (append object-syms quantified-object-syms hidden-object-syms 
                             quantified-hidden-object-syms trickle-down-slot-syms))))
           (new-settable-slots
            (remove-duplicates 
             (append (settable-slots class)
                     (list (append required-input-slot-syms settable-input-slot-syms 
                                   settable-defaulted-input-slot-syms settable-computed-slot-syms)))))
           
           (supers (remove-duplicates (append (glisp:direct-superclass-names class) mixin-list)
                                      :from-end t))
           
           (class (gensym)))
      
      (reserved-word-warning-or-error name messages supers)

      `(progn 
         ,(when (and *compile-documentation-database?* documentation)
                `(when *load-documentation-database?*
                   (setf (gdl-documentation ,class) ',documentation)))
         
         (let* ((,class (find-class ',name)))
           (setf (messages ,class) ',new-messages)
           (setf (required-input-slots ,class)  ',new-required-input-slots)
           (setf (optional-input-slots ,class)  ',new-optional-input-slots)
           (setf (defaulted-input-slots ,class)  ',new-defaulted-input-slots)
           (setf (computed-slots ,class) ',new-computed-slots)
           (setf (query-slots ,class) ',new-query-slots)
           (setf (settable-computed-slots ,class) ',new-settable-computed-slots)
           (setf (uncached-computed-slots ,class) ',new-uncached-computed-slots)
           (setf (settable-optional-input-slots ,class) ',new-settable-optional-input-slots)
           (setf (settable-defaulted-input-slots ,class) ',new-settable-defaulted-input-slots)
           (setf (functions ,class)  ',new-functions)
           (setf (methods ,class)  ',new-methods)
           (setf (cached-functions ,class) ',new-cached-functions)
           (setf (cached-methods ,class) ',new-cached-methods)
           (setf (objects ,class)  ',new-objects)
           (setf (quantified-objects ,class) ',new-quantified-objects)
           (setf (hidden-objects ,class)  ',new-hidden-objects)
           (setf (quantified-hidden-objects ,class) ',new-quantified-hidden-objects)
           (setf (trickle-down-slots ,class) ',new-trickle-down-slots)
           (setf (settable-slots ,class) ',new-settable-slots)
         
           (eval-when (compile load) (glisp:begin-redefinitions-ok))
           (defclass ,name ,supers
             (,@(remove-duplicates 
                 (append (make-standard-slots)
                         (make-accessible-slots new-computed-slots)
                         (make-accessible-slots new-query-slots)
                         (make-accessible-slots new-settable-computed-slots)
                         (make-accessible-slots new-uncached-computed-slots)
                         (make-accessible-slots new-objects)
                         (make-accessible-slots new-quantified-objects)
                         (make-accessible-slots new-hidden-objects)
                         (make-accessible-slots new-quantified-hidden-objects)
                         (make-accessible-slots new-cached-functions)
                         (make-accessible-slots new-cached-methods)
                         (make-accessible-slots new-required-input-slots :inputs? t)
                         (make-accessible-slots new-optional-input-slots :inputs? t)
                         (make-accessible-slots new-settable-optional-input-slots :inputs? t)
                         (make-accessible-slots new-defaulted-input-slots :inputs? t)
                         (make-accessible-slots new-settable-defaulted-input-slots :inputs? t)) :key #'first))
             (:metaclass gdl-class)
             
             
             )
           (eval-when (compile load) (glisp:end-redefinitions-ok))
       

           
           
           
           ;;
           ;; FLAG -- make sure these next two are ok. 
           ;;
           ,@(message-generics (set-difference messages (append method-syms cached-method-syms)))
                 
           ,(input-slots-generics (append (group-remark-strings (remove-if-not #'(lambda(item)
                                                                                   (or (symbolp item) (stringp item)))
                                                                               input-slots))
                                          (remove-if-not #'consp input-slots)))


           ;; FLAG -- consider pre-cooking these expression lists
           ;;
       
           ,@(input-slots-section name (group-remark-strings (remove-if-not #'(lambda(item)
                                                                                (or (symbolp item) (stringp item)))
                                                                            input-slots)))

       
           ,@(optional-input-slots-section 
              name (remove-if-not #'(lambda(slot) (and (consp slot) (null (rest (rest (strip-strings slot))))))
                                  input-slots))

           ,@(optional-input-slots-section 
              name (remove-if-not 
                    #'(lambda(slot) 
                        (and (consp slot) 
                             (member :settable (rest (rest (strip-strings slot))))
                             (not (member :defaulting (rest (rest (strip-strings slot)))))))
                    input-slots))

           ,@(optional-input-slots-section 
              name (remove-if-not #'(lambda(slot) 
                                      (and (consp slot) 
                                           (member :defaulting (rest (rest (strip-strings slot))))
                                           (not (member :settable (rest (rest (strip-strings slot)))))))
                                  input-slots) t)
       
           ,@(optional-input-slots-section
              name (remove-if-not 
                    #'(lambda(slot) 
                        (and (consp slot) 
                             (member :settable (rest (rest (strip-strings slot))))
                             (member :defaulting (rest (rest (strip-strings slot))))))
                    input-slots) t)
              
           ,@(computed-slots-section 
              name (remove-if-not #'(lambda(slot) (and (consp slot) (null (rest (rest (strip-strings slot))))))
                                  computed-slots))
           
           ,@(functions-section 
              name (mapcar #'(lambda(slot)
                               (if (stringp (first slot))
                                   (list (first slot) (second slot) nil (third slot))
                                   (list (first slot) nil (second slot))))
                           (remove-if-not #'(lambda(slot) (and (consp slot) (eql (first (rest (rest (strip-strings slot))))
                                                                                 :uncached)))
                                          computed-slots)))
           
           ,@(computed-slots-section 
              name (remove-if-not #'(lambda(slot)
                                      (and (consp slot) (eql (first (rest (rest (strip-strings slot)))) 
                                                             :settable)))
                                  computed-slots))
       
           ,@(computed-slots-section name query-slots :query? t)
       
           ,@(objects-section name objects) ,@(objects-section name hidden-objects) 
       
           ,@(functions-section name functions)
       
           ,@(methods-section name methods)
       
           ,@(trickle-down-slots-section 
              (append object-syms quantified-object-syms hidden-object-syms 
                      quantified-hidden-object-syms trickle-down-slot-syms))

	   (eval-when (:load-toplevel)
	       (let ((class (find-class ',name)))
		 (dolist (super (cons class (all-superclasses class)))
		   (let ((ht (or (gethash super *class-used-by-hash*)
				 (setf (gethash super *class-used-by-hash*)
				       (glisp:make-sans-value-hash-table)))))
		     (setf (gethash class ht) t)))
		 (maphash #'(lambda(key value)
			      (declare (ignore value))
			      (setf (gethash key *message-list-hashes*) nil))
			  (gethash class *class-used-by-hash*))))

	   
           )))))



(defmacro define-object (name mixin-list &rest spec-plist)
  "Defines a standard GDL object. Please see the document USAGE.TXT for an 
overview of <tt>define-object</tt> syntax."
  `(%define-object% ,name ,mixin-list ,@(merge-common-keys spec-plist)))


(defmacro %define-object% (name mixin-list 
                           &key input-slots
                             computed-slots
                             objects
                             hidden-objects
                             functions
                             methods
                             (no-vanilla-mixin? nil)
                             documentation
                             trickle-down-slots
                             query-slots
                             )

  (check-syntax name input-slots computed-slots objects hidden-objects 
                functions methods documentation trickle-down-slots query-slots)


  (remhash name *reserved-words-hash*)
  
  (with-gdl-message-symbols (:new t)
    
    (let ((mixins (if (or no-vanilla-mixin? 
                          (some #'(lambda(mixin)
                                    (let ((class (find-class mixin nil)))
                                      (and class
                                           (member (find-class 'vanilla-mixin)
                                                   (all-superclasses class))))) mixin-list))
                      mixin-list (append mixin-list (list 'vanilla-mixin)))))
    
      (reserved-word-warning-or-error name messages mixins)

      
    
      (let ((message-ht (make-hash-table :size (length messages))))
        (dolist (message messages)
          (push message (gethash message message-ht)))
        (let ((duplicates (let (duplicates)
                            (maphash #'(lambda(key val)
                                         (if (consp (rest val)) 
                                             (push key duplicates))) message-ht) duplicates)))
          (let ((duplicates (remove-if #'(lambda(dup) (member dup method-syms)) duplicates)))
            (when duplicates
              (error "duplicate slot name~p: ~{~a~^, ~}" 
                     (length duplicates) (sort duplicates #'string-lessp :key #'symbol-name))))))

      `(progn 
         (defclass ,name ,mixins
           (,@(append (make-standard-slots) 
                      (make-accessible-slots computed-slot-syms)
                      (make-accessible-slots settable-computed-slot-syms)
                      (make-accessible-slots uncached-computed-slot-syms)
                      (make-accessible-slots query-slot-syms)                 
                      (make-accessible-slots object-syms) 
                      (make-accessible-slots quantified-object-syms)
                      (make-accessible-slots hidden-object-syms)
                      (make-accessible-slots quantified-hidden-object-syms)
                      (make-accessible-slots cached-function-syms)
                      (make-accessible-slots cached-method-syms)
                      (make-accessible-slots required-input-slot-syms :inputs? t)
                      (make-accessible-slots optional-input-slot-syms :inputs? t)
                      (make-accessible-slots settable-input-slot-syms :inputs? t)
                      (make-accessible-slots defaulted-input-slot-syms :inputs? t) 
                      (make-accessible-slots settable-defaulted-input-slot-syms :inputs? t)))  (:metaclass gdl-class))
       
         (let ((,class-arg (find-class ',name)))
         
           (let ((,old-message-keys (messages ,class-arg))
                 (,new-message-keys ',messages))
             (dolist (key (set-difference ,old-message-keys ,new-message-keys))
               (let ((method (ignore-errors
                               (find-method (symbol-function (glisp:intern (symbol-name key) :gdl-slots)) nil
                                            (list ,class-arg) nil))))
                 (when method 
                   (when *report-gdl-redefinitions?*
                     (format t "~%Removing slot: ~a for object definition: ~s~%" key ',name))
                   (remove-method (symbol-function (glisp:intern (symbol-name key) :gdl-slots)) method)))))
       
           ,(when (and *compile-documentation-database?* documentation)
                  `(when *load-documentation-database?*
                     (setf (gdl-documentation ,class-arg) ',documentation))))

         
         (when (message-documentation (find-class ',name)) (clrhash (message-documentation (find-class ',name))))
         (when (message-source (find-class ',name))(clrhash (message-source (find-class ',name))))
         (setf (messages (find-class ',name)) ',messages)
         (setf (required-input-slots (find-class ',name)) ',required-input-slot-syms)
         (setf (optional-input-slots (find-class ',name)) ',optional-input-slot-syms)
         (setf (defaulted-input-slots (find-class ',name))  ',defaulted-input-slot-syms)
         (setf (computed-slots (find-class ',name))  ',computed-slot-syms)
         (setf (query-slots (find-class ',name))  ',query-slot-syms)
         (setf (settable-computed-slots (find-class ',name)) ',settable-computed-slot-syms)
         (setf (uncached-computed-slots (find-class ',name)) ',uncached-computed-slot-syms)
         (setf (settable-optional-input-slots (find-class ',name)) ',settable-input-slot-syms)
         (setf (settable-defaulted-input-slots (find-class ',name)) ',settable-defaulted-input-slot-syms)
         (setf (functions (find-class ',name))  ',function-syms)
         (setf (methods (find-class ',name))  ',method-syms)
         (setf (cached-functions (find-class ',name))  ',cached-function-syms)
         (setf (cached-methods (find-class ',name))  ',cached-method-syms)
         (setf (objects (find-class ',name))  ',object-syms)
         (setf (quantified-objects (find-class ',name)) ',quantified-object-syms)
         (setf (hidden-objects (find-class ',name))  ',hidden-object-syms)
         (setf (quantified-hidden-objects (find-class ',name)) ',quantified-hidden-object-syms)
         (setf (trickle-down-slots (find-class ',name))
               ',(append object-syms quantified-object-syms hidden-object-syms quantified-hidden-object-syms trickle-down-slot-syms))
         (setf (settable-slots (find-class ',name)) ',(append required-input-slot-syms settable-input-slot-syms 
                                                              settable-defaulted-input-slot-syms settable-computed-slot-syms))
       
         ;; FLAG -- consider pre-cooking these expression lists
         ;;
       
       
         ,@(message-generics (set-difference messages (append method-syms cached-method-syms)))

         ,(input-slots-generics (append (group-remark-strings (remove-if-not #'(lambda(item)
                                                                                 (or (symbolp item) (stringp item)))
                                                                             input-slots))
                                        (remove-if-not #'consp input-slots)))

         ,@(input-slots-section name (group-remark-strings (remove-if-not #'(lambda(item)
                                                                              (or (symbolp item) (stringp item)))
                                                                          input-slots)))

         ,@(optional-input-slots-section 
            name (remove-if-not #'(lambda(slot) (and (consp slot) (null (rest (rest (strip-strings slot))))))
                                input-slots))

         ,@(optional-input-slots-section 
            name (remove-if-not 
                  #'(lambda(slot) 
                      (and (consp slot) 
                           (member :settable (rest (rest (strip-strings slot))))
                           (not (member :defaulting (rest (rest (strip-strings slot)))))))
                  input-slots))

         ,@(optional-input-slots-section 
            name (remove-if-not #'(lambda(slot) 
                                    (and (consp slot) 
                                         (member :defaulting (rest (rest (strip-strings slot))))
                                         (not (member :settable (rest (rest (strip-strings slot)))))))
                                input-slots) t)
       
         ,@(optional-input-slots-section
            name (remove-if-not 
                  #'(lambda(slot) 
                      (and (consp slot) 
                           (member :settable (rest (rest (strip-strings slot))))
                           (member :defaulting (rest (rest (strip-strings slot))))))
                  input-slots) t)
              
         ,@(computed-slots-section 
            name (remove-if-not #'(lambda(slot) (and (consp slot) (null (rest (rest (strip-strings slot))))))
                                computed-slots))
       
         ,@(computed-slots-section 
            name (remove-if-not #'(lambda(slot)
                                    (and (consp slot) (eql (first (rest (rest (strip-strings slot)))) 
                                                           :settable)))
                                computed-slots))
       
         ,@(computed-slots-section name query-slots :query? t)


       
         ,@(objects-section name (append objects hidden-objects))
       
       
         ,@(functions-section 
            name (mapcar #'(lambda(slot)
                             (if (stringp (first slot))
                                 (list (first slot) (second slot) nil (third slot))
                                 (list (first slot) nil (second slot))))
                         (remove-if-not #'(lambda(slot) (and (consp slot) (eql (first (rest (rest (strip-strings slot))))
                                                                               :uncached)))
                                        computed-slots)))
       
         ,@(functions-section name functions)
       
         ,@(methods-section name methods)
       
         ,@(trickle-down-slots-section trickle-down-slot-syms)
       
         ,@(trickle-down-slots-section (append object-syms quantified-object-syms hidden-object-syms quantified-hidden-object-syms)
                                       :from-objects? t)
       
       
       
         #+nil
         (defmethod gdl-rule::%object-keywords%
             ((,self-arg ,name))
           (remove-duplicates
            (append ',object-syms ',quantified-object-syms
                    (when (next-method-p) (call-next-method)))))
       
         #+nil
         (defmethod gdl-rule::%hidden-object-keywords%
             ((,self-arg ,name))
           (remove-duplicates
            (append ',hidden-object-syms ',quantified-hidden-object-syms
                    (when (next-method-p) (call-next-method)))))
       
         ;;
         ;; FLAG -- maybe some of these others can go away as well. 
         ;;


         (let ((,self-arg (find-class ',name nil)))
           (when ,self-arg
             (setf (trickle-down-effective ,self-arg) nil)))

         ;;
         ;; FLAG -- use uiop:list-to-hash-set
         ;;
         (defmethod gdl-rule::%trickle-down-slots% 
             ((,self-arg ,name))
           (let ((class (class-of ,self-arg)))
             (or (trickle-down-effective class)
                 (let* ((trickles (remove-duplicates
                                   (mapcar #'make-keyword
                                           (append ',(append object-syms quantified-object-syms hidden-object-syms 
                                                             quantified-hidden-object-syms trickle-down-slot-syms)
                                                   (when (next-method-p) (list-hash (call-next-method)))))))
                        (ht (glisp:make-sans-value-hash-table :size (length trickles))))
                   (dolist (trickle trickles ht)
                     (setf (gethash trickle ht) t))))))
       
       
         (defmethod gdl-rule::%settable-slot-list%
             ((,self-arg ,name))
           (remove-duplicates
            (mapcar #'make-keyword
                    (append ',(append settable-input-slot-syms 
                                      settable-defaulted-input-slot-syms
                                      settable-computed-slot-syms)
                            (when (next-method-p) (call-next-method))))))
       
         (defmethod gdl-rule::%settable-slots% 
             ((,self-arg ,name))
           (let ((,ht-arg (glisp:make-sans-value-hash-table))
                 (,symbols-arg (gdl-rule::%settable-slot-list% ,self-arg))
                 (,non-settables-arg (mapcar #'make-keyword 
                                             (set-difference (messages (find-class ',name))
                                                             (settable-slots (find-class ',name))))))
             (dolist (,symbol-arg ,symbols-arg ,ht-arg) 
               (when (not (member ,symbol-arg ,non-settables-arg))
                 (setf (gethash ,symbol-arg ,ht-arg) t)))))
       
       
         (defmethod gdl-rule::%message-source%
             ((,self-arg ,name) ,message-arg &optional (,depth-arg 0))
           (let* ((method (nth ,depth-arg (compute-applicable-methods 
                                           #'gdl-rule::%message-source% (list ,self-arg ,message-arg))))
                  (class (first (glisp:gl-method-specializers method))))
             (append (let ((local (let ((ht (message-source class)))
                                    (when ht (gethash ,message-arg ht)))))
                       (when local (list (glisp:gl-class-name class) local)))
                     (when (next-method-p) (call-next-method ,self-arg ,message-arg (1+ ,depth-arg))))))
       

         (defmethod gdl-rule::%message-remarks%
             ((,self-arg ,name) ,message-arg &optional (,depth-arg 0))
           (let* ((method (nth ,depth-arg (compute-applicable-methods 
                                           #'gdl-rule::%message-remarks% (list ,self-arg ,message-arg))))
                  (class (first (glisp:gl-method-specializers method))))
             (append (let ((local (let ((ht (message-documentation class)))
                                    (when ht (gethash ,message-arg ht)))))
                       (when local (list (glisp:gl-class-name class) local)))
                     (when (next-method-p) (call-next-method ,self-arg ,message-arg (1+ ,depth-arg))))))

         ;;
         ;; FLAG -- why is this hardcoded to :all when it is first called?
         ;;
         (defmethod gdl-rule::%message-list%
           ((,self-arg ,name) ,category-arg ,message-type-arg ,base-part-type-arg
            &optional (,depth-arg 0))
         (append (message-list ,self-arg 
                               ;;:all 
                               ,category-arg
                               ,depth-arg ,base-part-type-arg)
                 (when (and (not (eql ,message-type-arg :local)) (next-method-p))
                   (call-next-method ,self-arg ,category-arg ,message-type-arg ,base-part-type-arg (1+ ,depth-arg)))))


	 (eval-when (:load-toplevel)
	     (let ((class (find-class ',name)))
	       (dolist (super (cons class (all-superclasses class)))
		 (let ((ht (or (gethash super *class-used-by-hash*)
			       (setf (gethash super *class-used-by-hash*)
				     (glisp:make-sans-value-hash-table)))))
		   (setf (gethash class ht) t)))
	       (maphash #'(lambda(key value)
			    (declare (ignore value))
			    (setf (gethash key *message-list-hashes*) nil))
			(gethash class *class-used-by-hash*))))

       
       
	 (find-class ',name)))))



(defun make-standard-slots ()
  `((gdl-acc::%version-tree% :accessor gdl-acc::%version-tree% :initform nil)
    (gdl-acc::%toplevel-inputs% :accessor gdl-acc::%toplevel-inputs% :initform nil :initarg :%toplevel-inputs%)
    (gdl-acc::%root% :accessor gdl-acc::%root% :initform nil :initarg :%root%)
    (gdl-acc::%god-parents% :accessor gdl-acc::%god-parents% :initform nil :initarg :%god-parents%)))


(defun first-symbol (list)
  (first (strip-strings list)))

(defun strip-strings (list)
  (if (stringp (first list))
      (strip-strings (rest list))
    list))

(defun message-symbol (message-list)
  (first-symbol message-list))

(defun message-source-code (message-list)
  (second (strip-strings message-list)))

(defun message-strings (list &optional (depth 0))
  (cond ((stringp (first list))
         (string-append (first list)
                        (if (stringp (second list)) " " "")
                        (message-strings (rest list) (1+ depth))))
        ((zerop depth) nil)
        (t "")))

(defun group-remark-strings (list)
  (let (result prev)
    (dolist (item list (mapcar #'nreverse (nreverse result)))
      (if (or (null prev) (symbolp prev))
          (push (list item) result)
        (push item (first result)))
      (setq prev item))))
      
