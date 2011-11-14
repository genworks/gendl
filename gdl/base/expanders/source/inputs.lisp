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

(defun not-handled (object message)
  (if *error-on-not-handled?*
      (not-handled-error object message)
    'gdl-rule:%not-handled%))
      

(defun input-methods (name attr-sym keys expressions special-keys)
  (let ((input (gensym)))
    (let ((remove-defunct-expressions
           (list
            `(dolist (,input (set-difference (getf (child-inputs (find-class ',name)) ',attr-sym) ',keys))
               (let ((method (find-method (symbol-function (glisp:intern (symbol-name ,input) :gdl-inputs)) nil
                                          (list (find-class ',name)
                                                (glisp:eql-specializer ',attr-sym)
                                                (find-class t)) nil)))
                 (when method
                   (remove-method (symbol-function (glisp:intern (symbol-name ,input) :gdl-inputs)) method)
                   (format t "~%Removing object input ~a for object ~a of object definition ~s~%"
                           ,input ',attr-sym ',name))))
            
            `(setf (getf (child-inputs (find-class ',name)) ',attr-sym) (set-difference ',keys ',special-keys)))))
      
      (append remove-defunct-expressions
              (remove 
               nil
               (mapcan
                #'(lambda(input expr)
                    (when (not (member input special-keys))
                      (let ((input-sym (glisp:intern (symbol-name input) :gdl-acc)))
                        (list
                         
                         ;;
                         ;; New stuff
                         ;;
                         `(defmethod ,(glisp:intern (symbol-name input-sym) :gdl-inputs)
                              ((self ,name)
                               (,part-arg (eql ',attr-sym))
                               (child t))

                            (let ((*error-on-not-handled?* t)) ,expr))

                         `(eval-when (compile load eval) (glisp:begin-redefinitions-ok))
                         
                         ;;`(defgeneric ,(glisp:intern (symbol-name input-sym) :gdl-slots) (self &rest ,args-arg))

                         `(eval-when (compile load eval) (glisp:end-redefinitions-ok))
                         
                         
                         `(when (or (not (fboundp ',(glisp:intern (symbol-name input-sym) :gdl-slots)))
                                    (not (find-method (symbol-function ',(glisp:intern (symbol-name input-sym) :gdl-slots))
                                                      nil (list (find-class 'gdl-basis)) nil)))
                            (defmethod ,(glisp:intern (symbol-name input-sym) :gdl-slots) ((,self-arg gdl-basis) &rest ,args-arg)
                              (declare (ignore ,args-arg))
                              (let ((,parent-arg (the-object ,self-arg %parent%)))
                                (if (null ,parent-arg) (not-handled ,self-arg ,(make-keyword input-sym))
                                  (,(glisp:intern (symbol-name input-sym) :gdl-inputs) 
                                   ,parent-arg (the-object ,self-arg :%name%) ,self-arg)))))))))
                
                keys expressions))))))


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
             `(defmethod ,(glisp:intern (symbol-name attr-sym) :gdl-inputs) ((,parent-arg gdl-remote) 
                                                                             ,part-arg 
                                                                             (,self-arg gdl-basis))
                (the-object ,parent-arg (fetch-input ,(make-keyword attr-sym) ,part-arg ,self-arg))))
           
           
           `(eval-when (compile load eval) (glisp:begin-redefinitions-ok))
           
           `(defmethod ,(glisp:intern (symbol-name attr-sym) :gdl-inputs) ((,parent-arg gdl-basis) 
                                                                     ,part-arg 
                                                                     (,self-arg gdl-basis))
              (declare (ignore ,part-arg))
              (let ((,val-arg (getf (the-object ,self-arg %parameters%) 
                                    ,(make-keyword (symbol-name attr-sym)) 'gdl-rule:%not-handled%)))
                (if (eql ,val-arg 'gdl-rule:%not-handled%) 
                    (not-handled ,self-arg ,(make-keyword attr-sym)) ,val-arg)))
           
           
           `(eval-when (compile load eval) (glisp:end-redefinitions-ok))

           `(defmethod ,(glisp:intern (symbol-name attr-sym) :gdl-slots) ((self ,name) &rest ,args-arg)
              (declare (ignore ,args-arg))
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
                             (if (member ,(make-keyword attr-sym) (the-object ,parent-arg :%trickle-down-slots%))
                                 (,(glisp:intern (symbol-name attr-sym) :gdl-slots) ,parent-arg)
                               (funcall (symbol-function ',(glisp:intern (symbol-name attr-sym) :gdl-trickle-downs)) ,parent-arg)))
                            (t (not-handled self ,(make-keyword attr-sym)))))))))))))
   input-slots))

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
                
           ;;
           ;; New stuff
           ;;
           
           `(eval-when (compile load eval) (glisp:begin-redefinitions-ok))
           
           (when (and *compile-for-dgdl?* (not (string-equal (symbol-name name) "remote-object") ))
             `(when (or (not (fboundp ',(glisp:intern attr-sym :gdl-slots)))
                        (not (find-method (symbol-function ',(glisp:intern attr-sym :gdl-slots))
                                          nil (list (find-class 'gdl-remote)) nil)))
                (defmethod ,(glisp:intern attr-sym :gdl-slots) ((self gdl-remote) &rest ,args-arg)
                  (the (send (:apply (cons ,(make-keyword (symbol-name attr-sym)) ,args-arg)))))))
           
           (when *compile-for-dgdl?*
             `(defmethod ,(glisp:intern (symbol-name attr-sym) :gdl-inputs) ((,parent-arg gdl-remote) 
                                                                             ,part-arg 
                                                                             (,self-arg gdl-basis))
                (the-object ,parent-arg (fetch-input ,(make-keyword attr-sym) ,part-arg ,self-arg))))
           
           
           `(defmethod ,(glisp:intern (symbol-name attr-sym) :gdl-inputs) ((,parent-arg gdl-basis) 
                                                                     ,part-arg 
                                                                     (,self-arg gdl-basis))
              (declare (ignore ,part-arg))
              (let ((,val-arg (getf (the-object ,self-arg %parameters%) 
                                    ,(make-keyword (symbol-name attr-sym)) 'gdl-rule:%not-handled%)))
                (if (eql ,val-arg 'gdl-rule:%not-handled%) (not-handled ,parent-arg ,(make-keyword attr-sym)) ,val-arg)))
           
           `(when (and ,defaulted?
                       (or (not (fboundp ',(glisp:intern (symbol-name attr-sym) :gdl-trickle-downs)))
                           (not (find-method (symbol-function ',(glisp:intern (symbol-name attr-sym) :gdl-trickle-downs))
                                             nil (list (find-class 'gdl-basis)) nil))))
              (defmethod ,(glisp:intern (symbol-name attr-sym) :gdl-trickle-downs) ((,self-arg gdl-basis) &rest ,args-arg)
                (not-handled ,self-arg ,(make-keyword attr-sym))))
           
           `(eval-when (compile load eval) (glisp:end-redefinitions-ok))
           
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
                                                      (,(glisp:intern (symbol-name attr-sym) :gdl-trickle-downs) self))
                                                  ,val-arg)))
                                  (if (eql ,val-arg 'gdl-rule:%not-handled%) ,attr-expr ,val-arg))
                              ,val-arg)
                         `(if (eql ,val-arg 'gdl-rule:%not-handled%) ,attr-expr ,val-arg)))))))))))
   
   input-slots))




