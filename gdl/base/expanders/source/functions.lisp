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

(defun functions-section (name functions)
  (mapcar 
   #'(lambda(function)
       (let ((second (second (strip-strings function))))
         (etypecase second
           (list (uncached-function name function))
           (keyword (ecase second
                      ((:cached :cached-equalp) (cached-function name function :test 'equalp))
                      (:cached-=   (cached-function name function :test '=))
                      (:cached-eq  (cached-function name function :test 'eq))
                      (:cached-eql (cached-function name function :test 'eql))
                      (:cached-equal (cached-function name function :test 'equal)))))))
   functions))


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
           
           
           `(when (or (not (fboundp ',(glisp:intern (symbol-name attr-sym) :gdl-slots)))
                      (not (find-method (symbol-function ',(glisp:intern (symbol-name attr-sym) :gdl-slots))
                                        nil (list (find-class 'gdl-basis)) nil)))
              (defmethod ,(glisp:intern (symbol-name attr-sym) :gdl-slots) ((,self-arg gdl-basis) &rest ,args-arg)
                (declare (ignore ,args-arg))
                (let ((,parent-arg (the-object ,self-arg %parent%)))
                  (if (null ,parent-arg) (not-handled ,self-arg ,(make-keyword attr-sym))
                    (,(glisp:intern (symbol-name attr-sym) :gdl-inputs) 
                     ,parent-arg (the-object ,self-arg :%name%) ,self-arg)))))
           
           `(eval-when (compile load eval) (glisp:begin-redefinitions-ok))
           `(defmethod ,(glisp:intern (symbol-name attr-sym) :gdl-inputs) ((,parent-arg gdl-basis) 
                                                                     ,part-arg 
                                                                     (,self-arg gdl-basis))
              (declare (ignore ,part-arg))
              (let ((,val-arg (getf (the-object ,self-arg %parameters%) 
                                    ,(make-keyword (symbol-name attr-sym)) 'gdl-rule:%not-handled%)))
                (if (eql ,val-arg 'gdl-rule:%not-handled%) (not-handled ,parent-arg ,(make-keyword attr-sym)) ,val-arg)))
           `(eval-when (compile load eval) (glisp:end-redefinitions-ok)))))))



(defun cached-function (name function &key (test #'eql))
  (let* ((attr-remarks (message-strings function))
         (attr-sym (glisp:intern (symbol-name (message-symbol function)) :gdl-acc))
         (lambda-list (third (strip-strings function)))
         (body (rest (rest (rest (strip-strings function)))))
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
           
           ;;
           ;; FLAG -- keep up-to-date to correspond to standard with-dependency-tracking macro
           ;;
           ;; add checks for *root-checking-enabled?*
           ;;
           
           (let ((value (gensym)))
             `(defmethod ,(glisp:intern (symbol-name attr-sym) :gdl-slots) ((self ,name) &rest ,args-arg)
                (let ((,value (,attr-sym self)))
                  (if (not (eq (first (ensure-list ,value)) 'gdl-rule::%unbound%))
                      (when (and *run-with-dependency-tracking?* *notify-cons*)
                        (add-notify-cons *notify-cons* ,value))
                    (setq ,value
                      (setf (,attr-sym self)
                        (list 
                         (make-hash-table :test ',test)
                         (when (and *run-with-dependency-tracking?* *notify-cons*)
                           (list (copy-list *notify-cons*)))))))
                  (or (gethash ,(ecase test
                                  ((eql eq =) `(first ,args-arg))
                                  ((equalp equal) `,args-arg)) 
                               (first ,value))
                      (setf (gethash ,(ecase test
                                        ((eql eq =) `(first ,args-arg))
                                        ((equalp equal) `,args-arg))
                                     (first ,value))
                        (let (,(remove nil (when *compile-dependency-tracking?*
                                             `(*notify-cons* (when *run-with-dependency-tracking?* (list self ',attr-sym))))))
                          (apply #'(lambda ,lambda-list
                                     ,(if has-declare? (first body) `(declare))
                                     (block ,(make-keyword attr-sym) 
                                       (let ((*error-on-not-handled?* t))
                                         ,@(if has-declare? (rest body) body)))) ,args-arg))))))))))))

              
           
