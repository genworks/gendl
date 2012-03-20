;;
;; Copyright 2002, 2009, 2012 Genworks International and Genworks BV 
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


(in-package :gdlxml)

(define-object definition-tree ()
  
  :documentation (:description "Given a symbol naming a GDL object which is already defined in the system,
this object generates a tree of all the message categories and all the messages within each category.

This tree can then be used as the basis for generating a representation of the object definition in 
an external format, such as GDLXML.")
  
  
  :input-slots (name-symbol)

  :computed-slots ((strings-for-display (format nil "Definition for ~s" (the name-symbol)))
		   
		   
		   (object-package (symbol-package (the name-symbol)))
		   
		   (object-documentation (the canonical-instance documentation))
		   
		   (class (find-class (the name-symbol)))
		   
		   (canonical-instance (make-object (the name-symbol)))
		   
		   ;;
		   ;; FLAG -- message-data should be defined on the meta-class (gdl-class).
		   ;;
		   (message-data (the canonical-instance (message-list :category :all
								       :message-type :local 
								       :return-category? t
								       :sort-order :by-category)))
		   
		   
		   ;;
		   ;; FLAG -- mixin-data should be defined on the meta-class (gdl-class).
		   ;;
		   (mixin-data (the canonical-instance (mixins :local? t)))
		   
		   (message-section-ht (let ((ht (make-hash-table)))
					 (mapc #'(lambda(message section) (push message (gethash section ht)))
					       (plist-keys (the message-data)) (plist-values (the message-data))) ht)))

  
  :trickle-down-slots (canonical-instance name-symbol)
  
  :objects ((sections :type 'message-section 
		      :sequence (:size (hash-table-count (the message-section-ht)))
		      :plist (list-hash (the message-section-ht))
		      :name (nth (the-child index) (plist-keys (the-child plist)))
		      :keys (nth (the-child index) (plist-values (the-child plist))))))

(define-object message-section ()
  
  :input-slots (name keys)
  
  :computed-slots ((strings-for-display (format nil "~a" (the name)))
		   
		   
		   
		   (section-key-and-modifiers
		    (ecase (the name)
		      (:required-input-slots (list :input-slots))
		      (:optional-input-slots (list :input-slots))
		      (:settable-optional-input-slots (list :input-slots :settable))
		      (:defaulted-input-slots (list :input-slots :defaulting))
		      (:settable-defaulted-input-slots (list :input-slots :settable :defaulting))
		      (:computed-slots (list :computed-slots))
		      (:settable-computed-slots (list :computed-slots :settable))
		      (:uncached-computed-slots (list :computed-slots :uncached))
		      (:objects (list :objects))
		      (:quantified-objects (list :objects))
		      (:hidden-objects (list :hidden-objects))
		      (:quantified-hidden-objects (list :hidden-objects))
		      (:functions (list :functions)))))
  
  
  :objects ((messages :type 'message 
		      :category (the name)
		      :sequence (:size (length (the keys)))
		      :section-key (first (the section-key-and-modifiers))
		      :modifiers (rest (the section-key-and-modifiers))
		      :key (nth (the-child index) (the keys)))))



(define-object message ()
  :input-slots (section-key category key modifiers)
  
  :computed-slots ((strings-for-display (format nil "~a" (the key)))
		   
		   (source-list (let ((source-list (the canonical-instance (slot-source (the key)))))
				  (when (eql (first source-list) (the name-symbol)) (second source-list))))

		   
		   (source-code-body (case (the category) 
				       (:functions (second (the source-list)))
				       (otherwise (the source-list))))
		   
		   
		   (source-code-argument-list (case (the category)
						(:functions (first (the source-list)))
						(otherwise nil)))
		   
		   
		   (remark-string (getf (the canonical-instance (slot-documentation (the key)))
					(the name-symbol)))))



