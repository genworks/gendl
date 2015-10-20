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


(#+allegro
 excl:without-redefinition-warnings
 #-allegro progn
 (#+allegro
  excl:without-package-locks
  #-allegro progn
  (define-object variable-sequence (quantification)
    :documentation (:description "A variable-sequence quantification is generated as a result of specifying 
<tt>:sequence (:indices ...))</tt> in an <tt>:objects</tt> specification. Unlike a normal sequence quantification (specified
with <tt>:sequence (:size ...))</tt>), elements can be surgically inserted and deleted from a variable-sequence.")
                       
    :input-slots
    (:element-index-list
     :child-types)

    :computed-slots
    ((lock (bt:make-lock))

     (ht (progn (the :child-types)
		;;
		;; FLAG -- consider increasing initial size or rehash-size
		;;
		(let ((ht (make-hash-table :size (length (the :element-index-list)))))
		  (dolist (index (the :element-index-list) ht)
		    (setf (gethash index ht) nil)))) :settable)

     (safe-list-elements (multiple-value-bind (list error)
			     (ignore-errors
			       (let ((result-list nil))
				 (dolist (n (the element-index-list) (nreverse result-list))
				   (multiple-value-bind (object error) 
				       (ignore-errors (the (:get-member n)))
				     (let 
					 ((child (cond ((typep error 'error)
							(list :object-key (list (make-keyword (the %name%)) n)
							      :error error))
						       ((typep object 'null-object)
							nil)
						       (t object))))
				       (when child (push child result-list)))))))
			   (if (typep error 'error)
			       (list (list :object-key (make-keyword (format nil "~a[agg]" (the %name%)))
					   :error error))
			       list)))
   
     (list-elements (mapcar #'(lambda(index)
				(the (:get-member index)))
			    (the :element-index-list)))

     (number-of-elements (length (the :element-index-list)))
   
     (first (the (:get-member (first (the :element-index-list)))))
     (last  (the (:get-member (first (last (the :element-index-list)))))))
  
    :functions
    ((exists? (index) (gethash index (the ht)))

     ("Void. Resets the variable sequence to its default list of indices (i.e. clears out any inserted or deleted elements and 
re-evaluates the expression to compute the original list of indices)"
      reset!
      ()
      (progn ;;bt:with-lock-held ((the lock))
	(the (restore-slot-default! :element-index-list))
	(the (restore-slot-default! :ht))))
   
     (instantiated-indices () (remove-if-not #'(lambda(index)
						 (second (multiple-value-list (gethash index (the :ht)))))
					     (the :element-index-list)))
   
     (get-member 
      (index)
    
      (multiple-value-bind (value found?)
	  (gethash index (the ht))
	(cond (value value)
	      (found? (setf (gethash index (the :ht))
			    (make-object-internal 
			     (the :child-types)
			     :%name% (list (glisp:intern 
					    (symbol-name (the :name-for-display)) 
					    :gdl-acc) nil t)
			     :%parent% (list (the :parent) nil t)
			     :%root% (gdl-acc::%root% self)
			     :%aggregate% (list self nil t)
			     :%index% (list index nil t))))
	      (t (error "The element ~a is not active in ~s" index self)))))

     (deactivate! (index) (the (:delete! index)))
   
     ("Void. Deletes the element identified with the given index.

:arguments (index \"Integer, Symbol, or other object matching with <tt>eql</tt>. The identifier used when the element was initialized or inserted.\")"
      delete!
      (index)
      (bt:with-lock-held ((the lock))
	(the (:set-slot! :ht (the :ht) :remember? nil))
	(remhash index (the ht))
	(the (:modify-attribute! :element-index-list
				 (remove index (the :element-index-list))))))
   
     (activate! (index) (the (:insert! index)))
   
   

     ("Void. Inserts a new element identified with the given index.

:arguments (index \"Integer, Symbol, or other object matching with <tt>eql</tt>. The identifier to be used to access this element.\")"
      insert!
      (index)
      (multiple-value-bind (value found?) 
	  (gethash index (the ht))
	(declare (ignore value))
	(if (not found?)
	    (bt:with-lock-held ((the lock))
	      (setf (gethash index (the ht)) nil)
	      (the (set-slot! :ht (the ht) :remember? nil))
	      (the (set-slot! :element-index-list
			      (append (the element-index-list) (list index)))))
	    (error "The element ~a is already active in ~s" index self))))))))

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
  

