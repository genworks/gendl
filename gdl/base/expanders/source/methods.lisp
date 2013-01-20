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

(defun methods-section (name methods)
  (mapcar 
   #'(lambda(method)
       (let ((second (second (strip-strings method))))
         (etypecase second
           (list (uncached-method name method))
           (keyword (ecase second
                      ((:cached :cached-equalp) (cached-method name method :test 'equalp))
                      (:cached-=   (cached-method name method :test '=))
                      (:cached-eq  (cached-method name method :test 'eq))
                      (:cached-eql (cached-method name method :test 'eql))
                      (:cached-equal (cached-method name method :test 'equal)))))))
   methods))


(defun uncached-method (name method)
  (let* ((attr-remarks (message-strings method))
         (attr-sym (glisp:intern (symbol-name (message-symbol method)) :gdl-acc))
         (lambda-list (second (strip-strings method)))
         (body (rest (rest (strip-strings method))))
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
	   ;; FLAG -- put this in when we know how to derive generic-function lambda-list from the lambda-list. 
	   ;; 
	   ;; FLAG -- use extract-lambda-list from glisp (make one) or c2mop package of :closer-mop.
	   ;;
	   #+nil
	   `(eval-when (:compile-toplevel :load-toplevel :execute)
	      (unless (fboundp ',(glisp:intern attr-sym :gdl-slots))
		(defgeneric ,(glisp:intern attr-sym :gdl-slots) (,self-arg ,@lambda-list))))
		     
	   
           `(defmethod ,(glisp:intern attr-sym :gdl-slots) ((self ,name) ,@lambda-list)
              ,(if has-declare? (first body) `(declare))
              
              ,(when *compile-circular-reference-detection?*
                            `(when (and *run-with-circular-reference-detection?*
                                        (member (list self ',attr-sym) *till-now* :test #'equalp))
                               (error "Circular reference detected")))
              
              (let ,(remove nil
                     (list (when *compile-circular-reference-detection?*
                             `(*till-now* (when *run-with-circular-reference-detection?*
                                            (cons (list self ',attr-sym) *till-now*))))))
                (block ,(make-keyword attr-sym) ,@(if has-declare? (rest body) body)))
              
              ))))))


(defun cached-method (name method &key (test #'eql))
  
  (declare (ignore name method test))
  (error "Cached methods not yet supported"))

              
           
