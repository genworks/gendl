;;
;; Copyright 2002, 2009, 2012 Genworks International
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

(in-package :com.genworks.dom)

(define-object base-document-component (base-html-sheet)
  :input-slots
  (data
   level
   heading-number)

  :computed-slots
  ((body (when (listp (the :data)) (rest (the :data))))
   
   (element-types (mapcar #'(lambda(element)
			      (the (:compute-type element)))
			  (the :body)))
   
   (elements-indices (let ((ht (make-hash-table)))
		       (mapcar #'(lambda(type)
				   (let ((current (gethash type ht)))
				     (setf (gethash type ht)
				       (if current (1+ current) 1))))
			       (the :element-types))))
   
   (elements-data (when (the :body) (make-array (length (the :body)) :initial-contents (the :body))))
   
   
   (sections-list (remove-if-not #'(lambda(element) (typep element 'section)) (list-elements (the elements)))))

  :objects
  ((elements :type (:sequence (the :element-types))
	     :sequence (:size (length (the :element-types)))
	     :data (svref (the :elements-data) (the-child :index))
	     :heading-number (format nil "~a.~a" (the :heading-number) 
				     (nth (the-child :index) (the :elements-indices)))
	     :level (1+ (the :level))))
  
  :functions
  ((compute-type 
    (element)
    (cond ((stringp element) 'text-string)

	  ((keywordp element) 'tag)

	  ((and (listp element)
		(listp (first element)))
	   (ecase (first (first element))
	     (:section 'section)
	     (:section* 'section)
	     (:subsection 'section)
	     (:subsection* 'section)
	     (:subsubsection 'section)
	     (:subsubsection* 'section)
	     (:subsubsubsection 'section)
	     (:subsubsubsection* 'section)
	     (:subsubsubsubsection 'section)
	     (:subsubsubsubsection* 'section)	     
	     (:chapter 'section)
	     (:chapter* 'section)
	     (:list    'item-list)
	     (:item    'item-list-item)
	     (:footnote 'footnote)
	     (:figure 'figure)
	     (:boxed-figure 'figure)
	     (:image-figure 'figure)
	     (:rendered-figure 'rendered-figure)
	     ))
	  ((listp element) 
	   (case (first element)
	     ((:item :li) 'item-list-item)
	     (:dt 'description-title)
	     (:dd 'description-body)
	     (:quote 'blockquote)
	     (:ol 'ordered-item-list)
	     (:ul 'unordered-item-list)
	     (:dl 'definition-item-list)
	     (:tt 'text-tt-marked-up-string)
	     (:pre 'verbatim-text-marked-up-string)
	     (:strong 'strong-text-marked-up-string)
	     (otherwise 'marked-up-string)))))))


(define-object tag (base-document-component)
  :computed-slots
  ((strings-for-display (format nil "Tag (~s)" (the :data)))))


(define-object assembly (section)
  
  :input-slots ((style-url "/gdl/style/top.css"))

  :computed-slots ((index-ht (the yadd index-ht)))
  
  :trickle-down-slots (index-ht)
  
  :hidden-objects
  ((yadd :type 'yadd::all)))



(define-object section (base-document-component)
  
  :input-slots
  ((data (symbol-value (read-from-string "com.genworks.books::*book-data*")))
   (level 0)
   (heading-number nil))
  
  :trickle-down-slots (class)
  
  :computed-slots
  ((attributes (rest (first (the :data))))
   
   (class (when (= (the :level) 0)
	    (or (getf (the :attributes) :documentclass)
		(getf (the :attributes) :class))))
   
   (copyright (when (= (the :level) 0)
		(getf (the :attributes) :copyright)))
   
   (abstract (when (= (the :level) 0)
	       (getf (the :attributes) :abstract)))
   
   (links (getf (the :attributes) :links))
   
   (section-style (first (first (the :data))))
   (title (getf (the :attributes) :title))
   (strings-for-display (format nil "~a: ~a" 
				(the :section-descriptor)
				(the :title)))
   
   (label (string-append
	   (ecase (the :section-style)
	     (:document "doc")
	     ((:chapter :chapter*) "chap")
	     ((:section :section*) "sec")
	     ((:subsection :subsection*) "subsec")
	     ((:subsubsection :subsubsection*) "subsubsec"))
	   ":"
	   (or (getf (the :attributes) :label) (title-to-label (the :title)))))
   
   (section-descriptor (case (the :level)
			 (0 "Doc")
			 ( 1 "Ch")
			 (otherwise
			  (format nil "~{~a-~}Sec" (make-list (- (the :level) 2) :initial-element "sub")))))))



(define-object marked-up-string (base-document-component)

  :computed-slots
  ((strings-for-display (format nil "Marked-up String (~s)" (first (the :data))))
   
   (markup-tag (if (keywordp (first (the :data)))
		   (first (the data))
		   :no-op))
   (body (if (keywordp (first (the data)))
	     (rest (the data))
	     (the data)))))

(define-object description-title (base-document-component))

(define-object description-body (base-document-component))
   
(define-object text-tt-marked-up-string (marked-up-string)

  :computed-slots
  ((strings-for-display (format nil "Marked-up String (~s)" (the markup-tag)))
   
   (markup-tag :texttt)))


(define-object verbatim-text-marked-up-string (marked-up-string)

  :computed-slots
  ((strings-for-display (format nil "Verbatim marked-up String (~s)" (the markup-tag)))
   
   (markup-tag :verbatim)))

(define-object strong-text-marked-up-string (marked-up-string)

  :computed-slots
  ((strings-for-display (format nil "Strong-text marked-up String (~s)" (the markup-tag)))
   
   (markup-tag :textbf)))



(define-object blockquote (base-document-component)
  :computed-slots
  ((tag :quote)))


(define-object item-list (base-document-component)
  :computed-slots
  (
   (strings-for-display "Item-List")
   (style (getf (rest (first (the :data))) :style))))

(define-object ordered-item-list (item-list)
  :computed-slots ((style :enumerate)))

(define-object unordered-item-list (item-list)
  :computed-slots ((style :itemize)))

(define-object definition-item-list (item-list)
  :computed-slots ((style :description)))

(define-object item-list-item (base-document-component)
  :computed-slots
  (
   (strings-for-display (format nil "Item-list Item: ~a..." 
				(let ((string (format nil "~s" (the data))))
				  (subseq string 0 (min 24 (1- (length string)))))))
   (word-data (when (listp (first (the :data)))
		(getf (rest (first (the :data))) :word)))
   
   (word-type (when (the :word-data) 
		(the (:compute-type (the :word-data))))))
   
  
  :objects
  ((word :type (or (the :word-type) 'null-part)
	 :data (the :word-data))))


(define-object text-string (base-document-component)
  :input-slots
  ((markup-tag nil))
  
  :computed-slots
  ((strings-for-display "Text-String")

   (word-list (split (the :data)))))


(define-object figure (base-document-component)
  :computed-slots
  ((strings-for-display "Figure")
   (style (first (first (the :data))))
   (label (or (getf (rest (first (the :data))) :label) ""))
   (caption (or (getf (rest (first (the :data))) :caption) ""))
   (width (getf (rest (first (the :data))) :width))
   (height (getf (rest (first (the :data))) :height))
   (image-file (when (eql (the :style) :image-figure) 
		 (getf (rest (first (the :data))) :image-file)))))


(define-object rendered-figure (base-document-component)
  


  :computed-slots
  ((width (or (getf (rest (first (the :data))) :width) "4in"))
   (height (or (getf (rest (first (the :data))) :height) "4in"))

   (strings-for-display "Rendered Figure")
   (caption (or (getf (rest (first (the :data))) :caption) ""))
   (label (or (getf (rest (first (the :data))) :label) 
	      (format nil "fig:~a" (the object))))

   (object (getf (rest (first (the :data))) :object))))

(defun title-to-label (title)
  (replace-substring
   (replace-substring
    (replace-substring
     (replace-substring
      (replace-substring
       (replace-substring 
	(string-downcase title) 
	" " "") 
       "{" "")
      "}" "")
     "'" "")
    "`" "")
   "\\texttt" ""))
