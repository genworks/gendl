;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
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
                    ,(if query? (error "Query-slots are not supported in this distribution of Genworks GDL.")
                       attr-expr)))))
           
           ;;
           ;;
           ;;

           (when (and *compile-for-dgdl?* (not (string-equal (symbol-name name) "remote-object")))
             `(when (or (not (fboundp ',(glisp:intern attr-sym :gdl-slots)))
                        (not (find-method (symbol-function ',(glisp:intern attr-sym :gdl-slots))
                                          nil (list (find-class 'gdl-remote)) nil)))
                (defmethod ,(glisp:intern attr-sym :gdl-slots) ((,self-arg gdl-remote) &rest ,args-arg)
                  (the-object ,self-arg (send (:apply (cons ,(make-keyword (symbol-name attr-sym)) 
                                                            ,args-arg)))))))
           
           
           `(when (or (not (fboundp ',(glisp:intern (symbol-name attr-sym) :gdl-slots)))
                      (not (find-method (symbol-function ',(glisp:intern (symbol-name attr-sym) :gdl-slots))
                                        nil (list (find-class 'gdl-basis)) nil)))
              (defmethod ,(glisp:intern (symbol-name attr-sym) :gdl-slots) ((,self-arg gdl-basis) &rest ,args-arg)
                (declare (ignore ,args-arg))
                (let ((,parent-arg (the-object ,self-arg %parent%)))
                  (if (null ,parent-arg) (not-handled ,self-arg ,(make-keyword attr-sym))
                    (let ((,val-arg (let (*error-on-not-handled?*)
                                      (,(glisp:intern (symbol-name attr-sym) :gdl-inputs) 
                                       ,parent-arg (the-object ,self-arg :%name%) ,self-arg))))
                      (if (eql ,val-arg 'gdl-rule:%not-handled%) (not-handled ,self-arg ,(make-keyword attr-sym)) ,val-arg))))))
           
           `(eval-when (:compile-toplevel :load-toplevel :execute) (glisp:begin-redefinitions-ok))
	   

	   ;;
	   ;; FLAG -- duplicated from inputs.lisp and objects.lisp, functions.lisp, and below in this file. 
	   ;; 
           `(unless nil #+nil (and (fboundp ',(glisp:intern (symbol-name attr-sym) :gdl-inputs))
				   (find-method (symbol-function ',(glisp:intern (symbol-name attr-sym) :gdl-inputs))
						nil (list (find-class 'gdl-basis) (find-class t) (find-class 'gdl-basis)) nil))
	      (defmethod ,(glisp:intern (symbol-name attr-sym) :gdl-inputs) ((,parent-arg gdl-basis) 
									     ,part-arg 
									     (,self-arg gdl-basis))
		(declare (ignore ,part-arg))
		(let ((,val-arg (getf (the-object ,self-arg %parameters%) 
				      ,(make-keyword (symbol-name attr-sym)) 'gdl-rule:%not-handled%)))
		  (if (eql ,val-arg 'gdl-rule:%not-handled%) (not-handled ,self-arg ,(make-keyword attr-sym)) ,val-arg))))

           `(eval-when (:compile-toplevel :load-toplevel :execute) (glisp:end-redefinitions-ok)))))) computed-slots))


(defun trickle-down-slots-section (slots &key from-objects?)
  (mapcan 
   #'(lambda(slot)
       (let (;; DJC trim effort
             ;; (result (gensym))
             )
         (remove 
          nil
          (list
           `(eval-when (:compile-toplevel :load-toplevel :execute) (glisp:begin-redefinitions-ok))
           (unless from-objects? ;; DJC trim effort
	     ;;
 	     ;; FLAG -- duplicated from objects.lisp, inputs.lisp, functions.lisp, and above in this file. 
	     ;;
             `(unless (and (fboundp ',(glisp:intern (symbol-name slot) :gdl-inputs))
			   (find-method (symbol-function ',(glisp:intern (symbol-name slot) :gdl-inputs))
					nil (list (find-class 'gdl-basis) (find-class t) (find-class 'gdl-basis)) nil))
		(defmethod ,(glisp:intern (symbol-name slot) :gdl-inputs) ((,parent-arg gdl-basis)
									   ,part-arg
									   (,self-arg gdl-basis))
		 (declare (ignore ,part-arg))
		 (let ((,val-arg (getf (the-object ,self-arg %parameters%) 
				       ,(make-keyword (symbol-name slot)) 'gdl-rule:%not-handled%)))
		   (if (eql ,val-arg 'gdl-rule:%not-handled%) (not-handled ,parent-arg ,(make-keyword slot)) ,val-arg)))))

	   `(unless nil #+nil (and (fboundp ',(glisp:intern (symbol-name slot) :gdl-trickle-downs))
				   (find-method (symbol-function ',(glisp:intern (symbol-name slot) :gdl-trickle-downs))
						nil (list (find-class 'gdl-basis)) nil))
	      (defmethod ,(glisp:intern (symbol-name slot) :gdl-trickle-downs) ((,self-arg gdl-basis) &rest ,args-arg)
		
		(trickle-down-basis ,self-arg ',slot ,args-arg)))
           
           `(unless nil #+nil(and (fboundp ',(glisp:intern (symbol-name slot) :gdl-slots))
				  (find-method (symbol-function ',(glisp:intern (symbol-name slot) :gdl-slots))
					       nil (list (find-class 'gdl-basis)) nil))
	      (defmethod ,(glisp:intern (symbol-name slot) :gdl-slots) ((,self-arg gdl-basis) &rest ,args-arg)
		(chase-up-trickle-down ',(glisp:intern (symbol-name slot) :gdl-slots) ,self-arg ,args-arg)))

           
           `(eval-when (:compile-toplevel :load-toplevel :execute) (glisp:end-redefinitions-ok))))))
   slots))
