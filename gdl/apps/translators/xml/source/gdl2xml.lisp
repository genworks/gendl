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

(define-object gdl2xml (definition-tree)
  
  
  :input-slots (name-symbol

		(output-file (merge-pathnames (format nil "~a.xml" (string (the name-symbol)))
					      (glisp:temporary-folder))))
  
  :computed-slots
  ((xml-string (with-output-to-string(ss)
		 (with-format (xml ss)
		   (write-the gdlxml))))
   
   
   
   (slot-plists (let (result)
		  (dolist (section (list-elements (the sections)) (nreverse result))
		    (dolist (message (list-elements (the-object section messages)))
		      (let ((section-key (the-object message section-key))
			    (keyword (the-object message key))
			    (source-code-body (the-object message source-code-body))
			    (source-code-argument-list (the-object message source-code-argument-list))
			    (remark-string (the-object message remark-string))
			    (modifiers (the-object message modifiers)))
			(let ((object? (member section-key (list :objects :hidden-objects)))
			      (*package* (the object-package)))
			  (push (append
				 (list :section-key (format nil "~a" section-key)
				       :keyword (format nil "~a" keyword)
				       :default (unless object?
						  (when source-code-body (format nil "~s" source-code-body)))
				       :argument-list (when source-code-argument-list 
							(format nil "~s" source-code-argument-list))
				       :remark-string (when remark-string (format nil "~a" remark-string))
				       :type (when object? (format nil "~s" (getf source-code-body :type)))
				       :sequence (when (and object? (getf source-code-body :sequence))
						   (format nil "~s" (getf source-code-body :sequence)))
				       :pass-down (when (and object? (getf source-code-body :pass-down))
						    (format nil "~s" (getf source-code-body :pass-down)))
				       :inputs (when object? (remove-plist-keys source-code-body 
										(list :type :sequence :pass-down))))
				 (mapcan #'(lambda(modifier)
					     (list modifier (format nil "~a" modifier))) modifiers)) result)))))))
   
   
   (mixin-lists (let ((*package* (the object-package)))
		    (mapcar #'(lambda(mixin)
				`(:mixin :name ,(format nil "~s" mixin)))
			    (the mixin-data))))
   
   
   (lxml `((:gdlxml :package ,(package-name (the object-package)))
	   ((:define-object :name ,(string (the name-symbol)))
	    
	    (:documentation ,@(mapcan #'(lambda(key value)
					  (list key `(escape-string ,value)))
				      (plist-keys (the object-documentation))
				      (plist-values (the object-documentation))))
	    
	    (:mixins ,@(the mixin-lists))
	    (:slots
	     ,@(mapcar #'(lambda(slot-plist)
			   (if (getf slot-plist :type)
			       `((:slot ,@(mapcan #'(lambda(key)
						      (let ((value (getf slot-plist key)))
							`(,key ,(when value `(escape-string ,value)))))
						  (list :section-key :keyword :remark-string :type :sequence :pass-down)))
				 (:inputs ,@(mapcar #'(lambda(key value)
							`(:input :keyword (escape-string ,(string key))
								 :default (escape-string ,(format nil "~s" value))))
						    (plist-keys (getf slot-plist :inputs))
						    (plist-values (getf slot-plist :inputs)))))
			     `(:slot
			       ,@(mapcan #'(lambda(key)
					     (let ((value (getf slot-plist key)))
					       `(,key ,(when value `(escape-string ,value)))))
					 (list :section-key :keyword :default :argument-list :remark-string
					       :settable :defaulting)))))
		       (the slot-plists)))))))
  
  
  :functions
  ((write-xml-file!
    (&key (output-file (the output-file)))
    (with-open-file (out output-file :direction :output
		     :if-exists :supersede :if-does-not-exist :create)
      (write-string (the xml-string) out)
      (values)))))
	    



(define-format xml (base-format))


(define-lens (xml definition-tree)()
  :output-functions
  ((gdlxml 
    ()
    ;;
    ;; FLAG be on the lookout for an alternative to this use of eval
    ;; 
    (eval 
     `(cl-who:with-html-output (*stream* nil :indent t)
	,(the lxml))))))



(defun gdl2xml (name-symbol &key (output-file (format nil "/tmp/~a.xml" (string name-symbol))))
  "Void. Emits an XML file representing the object definition named by name-symbol. The outputted file will be named 
after the name symbol (default is /tmp/[name].xml where [name] is the symbol name given).

:arguments (name-symbol \"Symbol. This quoted symbol must name an already-defined GDL object.\")

:&key ((output-file \"/tmp/[symbol-name].xml\") \"string or pathname naming the desired output file\")"

  (the-object (make-object 'gdl2xml :name-symbol name-symbol :output-file output-file)
	      (write-xml-file!)))

