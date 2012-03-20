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

(define-object xml2gdl ()
  
  
  :input-slots
  ((xml-file "/tmp/test-file.xml") ;;(xml-file "/tmp/test.xml")
   (output-file (format nil "/tmp/~a.lisp" (pathname-name (the xml-file))))
   (print-right-margin 80)
   
   (definition-lists (remove-if-not #'listp (rest (the gdlxml)))))


  
  :computed-slots
  ((lxml (with-open-file (in (the xml-file))
	   (glisp:parse-xml in)))
   
   (gdlxml (let ((list (first (the lxml))))
	     (when (eql (first list) :xml) (setq list (second (the lxml))))
	     (unless  (ignore-errors (eql (make-keyword (first (first list))) :gdlxml))
	       (error "Badly formed gdlxml file. Exiting."))
	     list))

   (package (find-package (getf (make-plist (rest (first (the gdlxml)))) :package))))

  
  
  :objects
  ((object-definitions :type 'object-definition
		       :sequence (:size (length (the definition-lists)))
		       :definition-list (remove-if-not #'listp (nth (the-child index) (the definition-lists)))
		       :print-right-margin (the print-right-margin)
		       :package (the package)))
  
  
  :functions 
  ((write-gdl-file!
    (&key (output-file (the output-file)))
    (with-open-file (out output-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (let ((*package* (the package)))
	(format out "(in-package ~s)~%~%" (make-keyword (package-name (the package))))
	
	(dolist (definition (list-elements (the object-definitions)))
	  (the-object definition (write-code-to-stream! out))))))))







(define-object object-definition ()
  :input-slots 
  ((expression `(define-object ,(the object-type-symbol) ,(the mixin-symbols)
		  ,@(when (the documentation-plist)
		      `(:documentation ,(the documentation-plist)))
		     
		  ,@(mapcan #'(lambda(section)
				(list (the-object section section-key)
				      (the-object section expression)))
		     (list-elements (the slot-sections)))))
   
   definition-list 
   (package  *package*)
   (print-right-margin *print-right-margin*))

  
  :computed-slots
  ((definition-key-list (mapcar #'(lambda(list)
				    (if (consp (first list))
					(cons (make-keyword (first (first list)))
					      (make-plist (rest (first list))))
				      (cons (make-keyword (first list)) (rest list))))
				(the definition-list)))
   
   
   (define-object-plist (make-plist (rest (assoc :define-object (the definition-key-list)))))
   
   (object-type-symbol (let ((*package* (the package)))
			 (read-safe-string (getf (the define-object-plist) :name))))
   
   
   (documentation-plist (rest (assoc :documentation (the definition-key-list))))
			
   
   (mixin-plists (let ((list (remove-if #'stringp (assoc :mixins (the definition-key-list)))))
		   (mapcar #'make-plist (mapcar #'rest (mapcar #'first (rest list))))))
   
   (mixin-symbols (mapcar #'(lambda(plist) 
			      (let ((*package* (the package)))
				(read-safe-string (getf plist :name))))
			  (the mixin-plists)))
   
   (slot-plists (let ((list (remove-if #'stringp (assoc :slots (the definition-key-list)))))
		  (let ((compound-ones (remove-if #'(lambda(list) (null (rest list))) (rest list)))
			(simple-ones (remove-if-not #'(lambda(list) (null (rest list))) (rest list))))
		    (append (mapcar #'make-plist (mapcar #'rest (mapcar #'first simple-ones)))
			    (mapcar #'(lambda(compound)
					(setq compound (remove-if-not #'listp compound))
					(append (make-plist (rest (first compound)))
						(mapcan #'(lambda(input-spec)
							    (setq input-spec (make-plist (rest (first input-spec))))
							    (list (make-keyword (getf input-spec :keyword))
								  (getf input-spec :default)))
							(remove-if-not #'listp (second compound)))))
				    compound-ones)))))
   
   
   (section-hash (let ((ht (make-hash-table)))
		   (dolist (plist (the slot-plists) ht)
		     (push (rest (rest plist))
			   (gethash (make-keyword (getf plist :section-key)) ht)))))
   
   (section-keys (let (result) (maphash #'(lambda(key value)
					    (declare (ignore value))
					    (push key result)) (the section-hash))
		      (nreverse result)))

   (strings-for-display (format nil "Definition for ~s" (the object-type-symbol))))

  
  :objects
  ((slot-sections :type 'object-slot-section
		  :sequence (:size (length (the section-keys)))
		  :package (the package)
		  :print-right-margin (the print-right-margin)
		  :section-key (nth (the-child index) (the section-keys))
		  :spec-lists (gethash (the-child section-key) (the section-hash))))
  
  :functions
  ((write-code-to-stream!
    (stream)
    
    (let ((*package* (the package)))
      (format stream "(~s ~s ~s~%~%" 
	      (first (the expression))
	      (second (the expression))
	      (third (the expression)))

      (when (the documentation-plist)
	(let ((*print-right-margin* (the print-right-margin)))
	  (write-string (with-output-to-string (ss)
			  (write-string "  " ss)
			  (prin1 :documentation ss)
			  (format ss "~%")
			  (write-string "  " ss)
			  (prin1 (the documentation-plist) ss)) stream)
	  (format stream "~%~%")))

      
      (mapc #'(lambda(section)
		(format stream "  ~s~%" (the-object section section-key))
		
		(format stream "  (")
		(write-string (the-object section pretty-string) stream)
		(format stream ")")
		(unless (the-object section last?)
		  (format stream "~%~%")))
	    (list-elements (the slot-sections)))
    
      (format stream ")~%~%")))))



(define-object object-slot-section ()
  :input-slots (section-key spec-lists package print-right-margin)
  
  :computed-slots ((strings-for-display (format nil "~s" (the section-key)))
		   
		   (expression (mapcar #'(lambda(message)
					   (the-object message expression))
				       (list-elements (the messages))))
		   
		   
		   (pretty-string (with-output-to-string(ss)
				    (mapc #'(lambda(message)
					      (let ((message-string (the-object message pretty-string)))
						(when (the-object message first?)
						  (setq message-string (subseq message-string 1)))
						(setq message-string
						  (glisp:replace-regexp message-string "\\n" (format nil "~%   ")))
						(unless (the-object message first?)
						  (setq message-string
						    (string-append (format nil "~%") message-string)))
						(write-string message-string ss)))
					  (list-elements (the messages))))))
  
  :objects
  ((messages :type 'object-slot-section-message
	     :sequence (:size (length (the spec-lists)))
	     :package (the package)
	     :print-right-margin (the print-right-margin)
	     :section-key (the section-key)
	     :spec-list (nth (the-child index) (the spec-lists)))))
   


(define-object object-slot-section-message ()
  :input-slots (spec-list section-key package print-right-margin)
  
  :computed-slots ((keyword (make-keyword (getf (the spec-list) :keyword)))
		   (default (let ((*package* (the package)))
			      (case (the section-key)
				((:objects :hidden-objects)
				 (let ((plist (remove-plist-key (the spec-list) :keyword)))
				   (mapcan #'(lambda(key value)
					       (list key (read-safe-string value)))
					   (plist-keys plist) (plist-values plist))))
				(otherwise (read-safe-string (getf (the spec-list) :default))))))
		   (remark-string (getf (the spec-list) :remark-string))
		   (settable (when (getf (the spec-list) :settable) :settable))
		   (defaulting  (when (getf (the spec-list) :defaulting) :defaulting))
		   (uncached (when (getf (the spec-list) :uncached) :uncached))
		   (modifiers (remove nil (list (the settable) (the uncached) (the defaulting))))
		   (argument-list (let ((*package* (the package)))
				    (read-safe-string (getf (the spec-list) :argument-list))))
		   
		   (strings-for-display (format nil "~s" (the keyword)))
		   
		   (key-symbol (let ((*package* (the package)))
				 (intern (the keyword) *package*)))
		   
		   (pretty-string (let ((*print-right-margin* (the print-right-margin))(*package* (the package)))
				    (with-output-to-string(ss)
				      (ecase (the section-key)
					((:input-slots :computed-slots)
					 (pprint (the expression) ss))
					(:functions 
					 (when (the first?) 
					   (write-string " " ss))
					 (unless (the first?)
					   (write-string "   " ss))
					 (write-string "(" ss)
					 (when (the remark-string)
					   (prin1 (the remark-string) ss))
					 (format ss "~% ")
					 (prin1 (the key-symbol) ss)
					 (format ss "~% ")
					 (prin1 (the argument-list) ss)
					 (dolist (expression (the default))
					   (format ss  "~% ")
					   (prin1 expression ss))
					 (write-string ")" ss))
					((:objects :hidden-objects)
					 (when (the first?) 
					   (write-string " " ss))
					 (unless (the first?)
					   (write-string "   " ss))
					 (write-string "(" ss)
					 (when (the remark-string)
					   (print (the remark-string) ss))
					 (prin1 (the key-symbol) ss)
					 (mapc #'(lambda(key value)
						   (format ss "~% ")
						   (prin1 key ss)
						   (write-string " " ss)
						   (prin1 value ss))
					       (plist-keys (rest (the expression)))
					       (plist-values (rest (the expression))))
					 (write-string ")" ss)
					 )))))

		   (expression (if (the default)
				   (ecase (the section-key)
				     ((:input-slots :computed-slots)
				      (append (when (the remark-string) (list (the remark-string)))
					      `(,(the key-symbol) ,(the default))
					      (the modifiers)))
				     (:functions (append (when (the remark-string)
							   (list (the remark-string)))
							 `(,(the key-symbol)
							   ,(the argument-list)
							   ,@(the default))))
				     ((:objects :hidden-objects)
				      (append (when (the remark-string) (list (the remark-string)))
					      `(,(the key-symbol)
						,@(the default)))))
				 (the key-symbol)))))




(defun make-plist (list)
  (mapcan #'(lambda(key value) (list (make-keyword key) value)) 
	  (plist-keys list) (plist-values list)))




(defun xml2gdl (xml-file &key (output-file (format nil "/tmp/~a.lisp" (pathname-name xml-file)))
			      (print-right-margin 80)
			      (compile-and-load? nil))
  
  "Void. Emits GDL source code representing the object definition in the given XML file. Optionally 
compiles and loads this definition.

:arguments (xml-file \"String or pathname. Identifies the source XML file, which must be well-formed GDLXML.\")

:&key ((output-file \"/tmp/[name].lisp\") \"String or pathname. Identifies target output file of GDL source.\"
       (print-right-margin 80) \"Number. The width for pretty-printing the GDL source code.\"
       (compile-and-load? nil) \"Boolean. Determines whether the source should be compiled and loaded after writing.\")"

  (the-object (make-object 'xml2gdl 
			   :xml-file xml-file
			   :output-file output-file
			   :print-right-margin print-right-margin) (write-gdl-file!))
  
  (format t "Wrote GDL source to ~a.~%" output-file)
  
  (when compile-and-load? (load (compile-file output-file))))

