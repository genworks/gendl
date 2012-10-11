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


(define-object standard-sequence (quantification)
  :documentation (:description "A standard sequence quantification is generated as a result of specifying 
<tt>:sequence (:size [number-expression]))</tt> in an <tt>:objects</tt> specification. Unlike a variable-sequence 
quantification (specified with <tt>:sequence (:indices ...))</tt>), elements cannot be surgically inserted or 
deleted from a standard sequence. If a value upon which the [number-expression] depends becomes modified,
each member of the sequence will be reinstantiated as it is demanded.")

  :input-slots
  (number-of-elements
   child-types)
               
  :computed-slots
  (
   (safe-list-elements (multiple-value-bind (list error)
                           (ignore-errors
                            (let ((result-list nil))
                              (dotimes (n (length (the :array)) (nreverse result-list))
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

   (list-elements-uncached (let ((result-list nil))
			     (dotimes (n (length (the :array)) (nreverse result-list))
			       (let ((object (the (:get-member n))))
				 (when (not (typep object 'null-object))
				   (push  object result-list))))) :uncached)

   (list-elements (let ((result-list nil))
                    (dotimes (n (length (the :array)) (nreverse result-list))
                      (let ((object (the (:get-member n))))
                        (when (not (typep object 'null-object))
                          (push  object result-list))))))
   
   (array
    (let ((number-of-elements (the number-of-elements)))
      (when (or (not (integerp number-of-elements))
                (minusp number-of-elements))
        (error "
The number of elements specified for a ':sequence (:size ...)' 
must evaluate to a non-negative integer. 

In ~s, it evaluated to ~s.

" 
               (append '(the :root) (symbols-as-keywords (reverse (the :root-path))) )
               number-of-elements))
      
      (let* ((current-array (previous-value self :array))
             (child-types (the child-types))
             (new-array (make-array (list number-of-elements) :initial-element nil)))
        
        (dotimes (n (min (length current-array) (length new-array)) new-array)
          (setf (aref new-array n) (aref current-array n))
          (let ((child-type (if (atom child-types) child-types
                              (or (nth n child-types) (first (last child-types))))))
            (when (and (aref new-array n)
                       (not (eql (class-of (aref new-array n)) child-type)))
              (change-class (aref new-array n) child-type)))))))
        
   (first (the (:get-member 0)))
   (last  (the (:get-member (1- (length (the :array)))))))
  
  :functions
  ((instantiated-indices
    ()
    (let (result (array (the :array)))
      (dotimes (n (the :number-of-elements) (nreverse result))
        (when (aref array n) (push n result)))))
   
   (get-member 
    (index)
    (let ((child-types (the child-types))
          (array (the array)))
      (typecase index
        (number
         (if (>= index (length array))
             (ecase *out-of-bounds-sequence-reference-action*
               (:error
                (error "Attempt to access element index ~a in sequence which only has ~a elements, 

the root-path to the sequence is ~s." 
                       index (length array) (cons 'the (reverse (the root-path)))))
               (:warn
                (warn "Attempt to access element index ~a in sequence which only has ~a elements -- 

the root-path to the sequence is ~s.

  *********** returning nil ************. 

" 
                       index (length array) (cons 'the (reverse (the root-path)))))
               (:silent nil))
           (or (aref array index)
               (setf (aref array index)
                 (make-object-internal (if (atom child-types)
                                           child-types
                                         (or (nth index child-types)
                                             (first 
                                              (last child-types))))
                                       :%name% (list (glisp:intern (symbol-name (the :name-for-display)) :gdl-acc) nil t)
                                       :%parent% (list (the :parent) nil t)
                                       :%root% (gdl-acc::%root% self)
                                       :%aggregate% (list self nil t)
                                       :%index% (list index nil t))))))
        (keyword
         (ecase index
           (:first (the :first))
           (:last  (the :last)))))))))


;; FLAG -- this needs a lot of work -- have to truly get rid of
;; members when deactivated, and not answer for indexes which are not
;; present.
;;
(define-object variable-sequence (quantification)
  :documentation (:description "A variable-sequence quantification is generated as a result of specifying 
<tt>:sequence (:indices ...))</tt> in an <tt>:objects</tt> specification. Unlike a normal sequence quantification (specified
with <tt>:sequence (:size ...))</tt>), elements can be surgically inserted and deleted from a variable-sequence.")
                       
  :input-slots
  (:element-index-list
   :child-types)

  :computed-slots
  ((ht (progn (the :child-types)
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
  (("Void. Resets the variable sequence to its default list of indices (i.e. clears out any inserted or deleted elements and 
re-evaluates the expression to compute the original list of indices)"
    reset!
    ()
    (the (restore-slot-default! :element-index-list)))
   
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
    (the (:modify-attribute! :ht (the :ht)))
    (remhash index (the ht))
    (the (:modify-attribute! :element-index-list
                             (remove index (the :element-index-list)))))
   
   (activate! (index) (the (:insert! index)))
   
   

   ("Void. Inserts a new element identified with the given index.

:arguments (index \"Integer, Symbol, or other object matching with <tt>eql</tt>. The identifier to be used to access this element.\")"
    insert!
    (index)
    (multiple-value-bind (value found?) 
        (gethash index (the ht))
      (declare (ignore value))
      (if (not found?)
          (progn
            (setf (gethash index (the ht)) nil)
            (the (set-slot! :ht (the ht)))
            (the (set-slot! :element-index-list
                            (append (the element-index-list) (list index)))))
        (error "The element ~a is already active in ~s" index self))))))





(define-object radial-sequence (standard-sequence)
  :documentation (:description "A radial sequence quantification is generated as a result of specifying 
<tt>:sequence (:radial [number-expression]))</tt> in an <tt>:objects</tt> specification.")

               
  :computed-slots
  (

   
   )
  
  :functions
  (
   
   
   ))


(define-object matrix-sequence (standard-sequence)
  :documentation (:description "A matrix sequence quantification is generated as a result of specifying 
<tt>:sequence (:matrix &lt;direction-keyword&gt; &lt;number&gt; &lt;direction-keyword&gt; &lt;number&gt;))</tt> in an <tt>:objects</tt> specification.")

               
  :computed-slots
  ((safe-list-elements (multiple-value-bind (list error)
                           (ignore-errors
                            (let ((result-list nil))
                              (dotimes (m (second (the number-of-elements)) (nreverse result-list))
                                (dotimes (n (fourth (the number-of-elements)))
                                  (multiple-value-bind (object error) 
                                      (ignore-errors (the (:get-member m n)))
                                    (let 
                                        ((child (cond ((typep error 'error)
                                                       (list :object-key (list (make-keyword (the %name%)) m n)
                                                             :error error))
                                                      ((typep object 'null-object)
                                                       nil)
                                                      (t object))))
                                      (when child (push child result-list))))))))
                         (if (typep error 'error)
                             (list (list :object-key (make-keyword (format nil "~a[agg]" (the %name%)))
                                         :error error))
                           list)))

   
   
   
   (list-elements (let ((result-list nil))
                    (dotimes (m (second (the number-of-elements)) (nreverse result-list))
                      (dotimes (n (fourth (the number-of-elements)))
                        (let ((object (the (:get-member m n))))
                          (when (not (typep object 'null-object))
                            (push  object result-list)))))))
   
   
   (array 
    (let ((standard-axes '(:longitudinal :lateral :vertical))
          (number-of-elements (the number-of-elements)))
      (when (not (listp number-of-elements))
        (error " 
Matrix quantification needs two direction keywords each with
a value evaluating to a number."))
      (let ((axis-1 (first number-of-elements))
            (rows (second number-of-elements))
            (axis-2 (third number-of-elements))
            (columns (fourth number-of-elements)))
        
        (when (not (and (member axis-1 standard-axes)
                        (integerp rows)
                        (member axis-2 standard-axes)
                        (integerp columns)))
          (error "

Malformed Matrix Sequence"))
        
        (the :child-types)
        
        (make-array (list rows columns) :initial-element nil))))
   
   
   (first (the (get-member 0 0)))
   (last (the (get-member (1- (second (the number-of-elements)))
                          (1- (fourth (the number-of-elements)))))))

  
  :functions
  (
   (get-member 
    (row column)
    (let ((child-types (the child-types))
          (array (the array)))
      (typecase row
        (number
         (or (aref array row column)
             (setf (aref array row column)
               (make-object-internal (if (atom child-types)
                                         child-types
                                       ;;
                                       ;; FLAG -- allow list of list of child types
                                       ;;
                                       (first child-types))
                                     :%name% (list (glisp:intern (symbol-name (the :name-for-display)) :gdl-acc) nil t)
                                     :%parent% (list (the :parent) nil t)
                                     :%root% (gdl-acc::%root% self)
                                     :%aggregate% (list self nil t)
                                     :%index% (list (list row column) nil t)))))
        (keyword
         (ecase row
           (:first (the :first))
           (:last  (the :last)))))))))   



(defun previous-value (object slot)
  (let ((slot (glisp:intern slot :gdl-acc)))
    (let ((value (slot-value object slot)))
      (when (and (consp value) (eq (first value) 'gdl-rule::%unbound%))
        (fourth value)))))
