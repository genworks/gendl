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



(defun all-inputs (symbol)
  (cond ((atom symbol)
         (all-inputs-single symbol))
        ((consp symbol)
         (remove-duplicates
          (apply #'append
                 (mapcar #'(lambda(sym)
                             (let ((all (all-inputs-single sym)))
                               (when (consp all) all)))
                         symbol))))
        (t (error "symbol is ~s which doesn't compute.~%" symbol))))


(defun all-inputs-single (symbol)
  (let ((class (find-class symbol nil))
        (gdl-class (find-class 'gdl-class)))
    (if class 
        (let ((supers (remove-if-not #'(lambda(class)
                                         (eql (class-of class) gdl-class))
                                     (cons class (find-all-superclasses class)))))
          (apply #'append
                 (mapcar #'(lambda(super)
                             (append (required-input-slots super)
                                     (optional-input-slots super)
                                     (defaulted-input-slots super)
                                     (settable-optional-input-slots super)
                                     (settable-defaulted-input-slots super))) 
                         supers)))
      :no-class)))


(defun message-list (self category depth base-part-type)
  
  (setq category (normalize-category-name category))
  (let* ((method (nth depth (compute-applicable-methods 
                             #'gdl-rule::%message-list% 
                             (list self t t t))))

         (class (first (glisp:gl-method-specializers method)))
         
         (base-superclasses (when base-part-type
                              (cons (find-class base-part-type)
                                    (find-all-superclasses (find-class base-part-type))))))
    (when (or (null base-part-type)
              (not (member class base-superclasses))
              ;;(not (eql base-part-type type))
              )
      
      (let ((unfiltered
             (case category
               (:all 
                (mapcan #'(lambda(cat)
                            (mapcan #'(lambda(message)
                                        (list message cat))
                                    (let ((keys 
                                           (safe-sort (mapcar #'make-keyword 
                                                              (funcall (glisp:intern (format nil "~a" cat) :gdl) class))
                                                      #'string<)))
                                      keys)))
                        *message-categories*))
               (otherwise (let ((messages (mapcar #'make-keyword 
                                                  (funcall (glisp:intern (format nil "~a" category)) class))))
                            (mapcan #'(lambda(message)
                                        (list message category))
                                    messages))))))
        (let ((messages (plist-keys unfiltered))
              (cats (plist-values unfiltered)))
          (mapcar #'(lambda(mess cat) (list mess cat)) messages cats))))))


(defun normalize-category-name (name)
  (ecase name
    ((:all :required-input-slots :optional-input-slots :settable-optional-input-slots
      :defaulted-input-slots :settable-defaulted-input-slots :computed-slots :query-slots 
      :settable-computed-slots :uncached-computed-slots 
      :functions :objects :quantified-objects :hidden-objects 
      :quantified-hidden-objects :trickle-down-slots) name)
    (:modifiable-required-inputs :required-input-slots)
    (:optional-inputs :optional-input-slots)
    (:modifiable-optional-inputs :settable-optional-input-slots)
    (:defaulted-inputs :defaulted-input-slots)
    (:modifiable-defaulted-inputs :settable-defaulted-input-slots)
    (:attributes :computed-slots)
    (:query-attributes :query-slots)
    (:modifiable-attributes :settable-computed-slots)
    (:uncached-attributes :uncached-computed-slots)
    (:methods :functions)
    (:parts :objects)
    (:quantified-parts :quantified-objects)
    (:pseudo-parts :hidden-objects)
    (:quantified-pseudo-parts :quantified-hidden-objects)
    (:descendant-attributes :trickle-down-slots)))

(defun old-category-name (name)
  (ecase name 
    (:all name)
    (:required-input-slots :required-inputs)
    (:optional-input-slots :optional-inputs )
    (:defaulted-input-slots :defaulted-inputs )
    (:computed-slots :attributes )
    (:query-slots :query-attributes )
    (:settable-computed-slots :modifiable-attributes )
    (:uncached-computed-slots :uncached-attributes )
    (:functions :methods )
    (:objects :parts )
    (:quantified-objects :quantified-parts )
    (:hidden-objects :pseudo-parts )
    (:quantified-hidden-objects :quantified-pseudo-parts )
    (:trickle-down-slots :descendant-attributes )
    ((:required-inputs :optional-inputs :defaulted-inputs
      :attributes :query-attributes :modifiable-attributes
      :methods :parts :quantified-parts :pseudo-parts
      :quantified-pseudo-parts :descendant-attributes :uncached-attributes) name)))

     

(defun get-remarks (message-list)
  (let (result-list on-remark?  on-message?
        (current-remark-string nil))
    (dolist (element message-list (nreverse result-list))
      (cond ((eql element :remark) (setq on-remark? t))
            (on-remark?  
             (setq current-remark-string (if current-remark-string
                                             (string-append current-remark-string element)
                                           element))
             (setq on-remark? nil on-message? t))
            
            (on-message?  
             (push element result-list)
             (push current-remark-string result-list)
             (setq current-remark-string nil on-message? nil))))))


(defparameter *reserved-word-protected-packages* (list (find-package :gdl)
						       (find-package :geom-base)
						       (find-package :surf)))

(defun reserved-word-warning-or-error (name messages)
  (unless (and (atom name)
               (or (member (symbol-package name) *reserved-word-protected-packages*)))
    (let ((reserved-violations (intersection messages +reserved-words+)))
      (when reserved-violations 
        (let ((is-or-are (if (= (length reserved-violations) 1) "is" "are"))
              (s-or-blank (if (= (length reserved-violations) 1) "" "s"))
              (a-or-blank (if (= (length reserved-violations) 1) "a " "")))
                               
          (funcall (if *error-on-reserved-words?*
                       #'error
                     #'warn)
                   "The symbol~a listed below ~a ~areserved word~a and should not be used as ~a 
GDL message name~a or child object input name~a. 

~a.

~{ ~a~^~%~}


"
                   s-or-blank
                   is-or-are
                   a-or-blank
                   s-or-blank
                   a-or-blank
                   s-or-blank
                   s-or-blank
                   (if *error-on-reserved-words?*
                       "You can override this error (AT YOUR OWN RISK) 
by setting *error-on-reserved-words?* to nil."
                     "NOTE that you are getting a warning instead of an error because
*error-on-reserved-words?* is changed from its default of t and set to nil. This is RISKY
and you should proceed this way only if you really know what you are doing.")
                   (safe-sort reserved-violations #'string-lessp :key #'string)))))))


