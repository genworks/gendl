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
 excl:without-package-locks
 #-allegro progn
 (#+allegro
  excl:without-redefinition-warnings
  #-allegro progn
(defun optional-input-slots-section (name input-slots &optional (defaulted? nil))
  (mapcan 
   #'(lambda(input)
       (let ((attr-remarks (message-strings input))
             (attr-sym (glisp:intern (symbol-name (message-symbol input)) :gdl-acc))
             (attr-expr (message-source-code input)))
         (remove 
          nil
          (list
           (when (and *compile-documentation-database?* attr-remarks)
             `(when *load-documentation-database?*
                ;;
                ;; FLAG -- make the hash table the initform rather than this clumsy "or" conditional.
                ;;
                (let ((ht (or (message-documentation (find-class ',name))
                              (setf (message-documentation (find-class ',name)) (make-hash-table)))))
                  (setf (gethash (make-keyword ',attr-sym) ht) ,attr-remarks))))
           (when *compile-source-code-database?*
             `(when *load-source-code-database?*
                ;;
                ;; FLAG -- make the hash table the initform rather than this clumsy "or" conditional.
                ;;
                (let ((ht (or (message-source (find-class ',name))
                              (setf (message-source (find-class ',name)) (make-hash-table)))))
                  (setf (gethash (make-keyword ',attr-sym) ht) ',attr-expr))))
                
           `(eval-when (:compile-toplevel :load-toplevel :execute) (glisp:begin-redefinitions-ok))
           
           (when (and *compile-for-dgdl?* (not (string-equal (symbol-name name) "remote-object")))
             `(unless (find-method (symbol-function ',(glisp:intern attr-sym :gdl-inputs))
                                   nil (list (find-class 'gdl-remote) t (find-class 'gdl-basis)) nil)
                 (when (or (not (fboundp ',(glisp:intern attr-sym :gdl-slots)))
                          (not (find-method (symbol-function ',(glisp:intern attr-sym :gdl-slots))
                                            nil (list (find-class 'gdl-remote)) nil)))
                  (defmethod ,(glisp:intern attr-sym :gdl-slots) ((self gdl-remote) &rest ,args-arg)
                    (the (send (:apply (cons ,(make-keyword (symbol-name attr-sym)) ,args-arg))))))))

           
           (when *compile-for-dgdl?*
	     `(unless (find-method (symbol-function ',(glisp:intern attr-sym :gdl-inputs))
                                   nil (list (find-class 'gdl-remote) t (find-class 'gdl-basis)) nil)
		(defmethod ,(glisp:intern (symbol-name attr-sym) :gdl-inputs) ((,parent-arg gdl-remote) 
									       ,part-arg 
									       (,self-arg gdl-basis))
		  (the-object ,parent-arg (fetch-input ,(make-keyword attr-sym) ,part-arg ,self-arg)))))
           
           `(eval-when (:compile-toplevel :load-toplevel :execute) (glisp:end-redefinitions-ok))
           
           `(defmethod ,(glisp:intern (symbol-name attr-sym) :gdl-slots) ((self ,name) &rest ,args-arg)
              (declare (ignore ,args-arg))
              (let ((*error-on-not-handled?* t))
                (with-dependency-tracking (,attr-sym)
                  (let ((,parent-arg (the :%parent%)))
                    (let ((,val-arg
                           (if ,parent-arg
                               (let (*error-on-not-handled?*)
                                 (,(glisp:intern (symbol-name attr-sym) :gdl-inputs)
                                   ,parent-arg (the :%name%) self))
                               'gdl-rule:%not-handled%)))
                      ,(if defaulted?
                           `(if (eql ,val-arg 'gdl-rule:%not-handled%)
                                (let ((,val-arg (if ,parent-arg
                                                    (let (*error-on-not-handled?*)
                                                      (if (fboundp ',(glisp:intern (symbol-name attr-sym) :gdl-trickle-downs))
                                                          (,(glisp:intern (symbol-name attr-sym) :gdl-trickle-downs) self)
                                                          'gdl-rule:%not-handled%))
                                                    ,val-arg)))
                                  (if (eql ,val-arg 'gdl-rule:%not-handled%) ,attr-expr ,val-arg))
                                ,val-arg)
                           `(if (eql ,val-arg 'gdl-rule:%not-handled%) ,attr-expr ,val-arg))))))))))) input-slots))))



