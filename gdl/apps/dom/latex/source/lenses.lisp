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

(in-package :com.genworks.dom-latex)

(define-lens (latex assembly) ()
  :output-functions
  ((base
    (&rest args)
    (declare (ignore args))
    (let* ((specials (list :company :author :copyright :class :abstract :links))
	   (attributes (remove-keys specials (the :attributes))))
      
      (when (getf (the :attributes) :class)
	(let ((value (getf (the :attributes) :class)))
	  (write-env (:a (format nil "\\documentclass ~a"
				 (cond ((consp value)
					(format nil "~{[~a]~}{~a}" (butlast value) (lastcar value)))
				       (t (format nil "{~(~a~)}" value)))))
		     (:newline-out) (:newline-out))))
      
      ;;
      ;; FLAG -- Query whether we want these all the time.
      ;;
      #+nil(write-env "\\usepackage[dvips]{graphicx}"
		      (:newline-out)
		      "\\usepackage[usenames, dvipsnames]{color}"
		      (:newline-out)(:newline-out)
		      "\\newsavebox{\\boxedverb}"
		      (:newline-out)(:newline-out))
      
      (when (getf (the :attributes) :author)
	(let ((value (getf (the :attributes) :author))
	      (copyright (getf (the :attributes) :copyright))
	      (company (getf (the :attributes) :company)))
	  (write-env "\\author {" 
		     (:a value)
		     (:a (if copyright
			     (format nil "\\thanks{\\copyright ~a}" copyright)
			   ""))
		     (:a (if company
			     (format nil "\\\\~a" company)
			   ""))
		     "}"
		     (:newline-out) (:newline-out))))
      
      (mapcar #'(lambda(key value)
		  (write-env (:a (format nil "\\~(~a~) ~a" 
					 key
					 (cond ((consp value)
						(format nil "~{[~a]~}{~a}" (butlast value) (lastcar value)))
					       ((member key (list :title :date))
						(format nil "{~a}" value))
					       ((null value) "")
					       ((numberp value) (format nil "~ain" value))
					       ((stringp value) (escape-string value))
					       (t value))))
			     (:newline-out)(:newline-out)))
	      (plist-keys attributes)
	      (plist-values attributes)))
    
    (write-env (:newline-out)(:newline-out)
	       "\\begin{document}"
	       (:newline-out)(:newline-out))
    
    (when (getf (the :attributes) :abstract)
      (write-env "\\begin{abstract}" (:newline-out) 
		 "\\emph{"
		 (:a (getf (the :attributes) :abstract))
		 "}"
		 (:newline-out)
		 "\\end{abstract}"
		 (:newline-out) (:newline-out)))

    (when (getf (the :attributes) :links)
      (write-env "\\section{Links}"
		 (:newline-out)
		 "\\begin{itemize}"
		 (:newline-out))
      
      (dolist (link (getf (the :attributes) :links))
	(write-env "\\item "
		   (:a (escape-string (getf link :text)))
		   "\\footnote{"
		   (:a (escape-string (getf link :url)))
		   "}"
		   (:newline-out)))
      
      (write-env "\\end{itemize}"
		 (:newline-out)))
    
    (dolist (element (list-elements (the :elements)))
      (write-the-object element (base)))
    
    (when (eql (the :class) :book)
      (write-env (:newline-out)(:newline-out)
		 "\\addcontentsline{toc}{chapter}{Index}"
		 (:newline-out) (:newline-out)
		 "\\printindex"))
    
    (write-env (:newline-out) (:newline-out)
	       "\\end{document}"
	       (:newline-out)(:newline-out)))))

      
      
(define-lens (latex tag)()
  :output-functions
  ((base
    (&rest args)
    (declare (ignore args))
    
    (write-env (:newline-out)(:newline-out)
	       "\\" (:a (string-downcase (the :data)))
	       (:newline-out)(:newline-out)))))


(define-lens (latex section)()

  :output-functions
  ((base
    (&rest args)
    (declare (ignore args))
    (write-env (:newline-out)
	       (:newline-out))
    (write-env (:a (format nil "\\~(~a~)" (the :section-style)))
	       "{" (:a (the :title)) "}")
    (write-env (:newline-out)(:newline-out))
    (write-env "\\label{" (:a (the :label)) "}")
    (write-env (:newline-out)(:newline-out))
    (dolist (element (list-elements (the :elements)))
      (write-the-object element (base))))))


(define-lens (latex marked-up-string)()
  :output-functions
  ((base
    (&rest args)
    (declare (ignore args))

    (case (the :markup-tag)


      (:include (dolist (element (list-elements (the elements)))
		  (with-open-file (in (the-object element data))
		    (do ((line (read-line in nil nil) (read-line in nil nil)))
			((null line))
		      (write-env (:a line))
		      (write-env (:newline-out))))))
      
      (:p (write-env (:newline-out) (:newline-out))
	  (dolist (element (list-elements (the :elements)))
	    (write-the-object element (base)))
	  (write-env (:newline-out) (:newline-out)))

      (:define-object 
       (warn "define-object markup-tag is not handled. In ~s~%" (the root-path)))

      (:href 
       (write-env "\\href")
       (dolist (element (list-elements (the :elements)))
	 (write-env "{") (write-the-object element (base)) (write-env "}")))
      

      (otherwise 
       (when (eql (the :markup-tag) :verbatim)
	 (write-env (:newline-out) (:newline-out)))
       (let ((markup-tag (case (the :markup-tag)
			   (:indexed :index)
			   (:verbatim "begin{verbatim}")
			   (:small "{\\small")
			   (otherwise (the :markup-tag)))))
	 (when (not (member (the :markup-tag) (list :verbatim :footnote :emph :indexed :index :texttt)))
	   (write-env (:newline-out)))
	 (when (not (eql (the markup-tag) :small)) (write-env "\\"))
	 (write-env (:a (string-downcase markup-tag)))
	 (when (not (member (the :markup-tag) '(:verbatim :small)))
	   (write-env "{")))
       (dolist (element (list-elements (the :elements)))
	 (write-the-object element (base :escape-strings? (not (eql (the :markup-tag) :verbatim)))))
       (if (eql (the :markup-tag) :verbatim)
	   (write-env (:newline-out)
		      "\\end{verbatim}"
		      )
	 (write-env "}" ))
       (when (eql (the :markup-tag) :indexed)
	 (dolist (element (list-elements (the :elements)))
	   (write-the-object element (base)))))))))

(define-lens (latex item-list)()
  :output-functions
  ((base
    (&rest args)
    (declare (ignore args))
    (write-env (:newline-out)
	       (:newline-out)
	       "\\begin{" (:a (string-downcase (the :style))) "}"
	       (:newline-out))
    (dolist (element (list-elements (the :elements)))
      (write-the-object element (base)))
    
    (write-env (:newline-out)
	       "\\end{" (:a (string-downcase (the :style))) "}"
	       (:newline-out)
	       (:newline-out)))))


(define-lens (latex item-list-item)()
  :output-functions
  ((base
    (&rest args)
    (declare (ignore args))
    (write-env (:newline-out)
	       "\\item ")
    (when (not (typep (the :word) 'null-part))
      (write-env "[")
      (write-the :word (base))
      (write-env "]" (:newline-out)))
    (dolist (element (list-elements (the :elements)))
      (write-the-object element (base)))
    (write-env (:newline-out)))))


(define-lens (latex figure)()
  :output-functions
  ((base
    (&rest args)
    (declare (ignore args))
    (write-env (:newline-out)
	       "\\begin{figure}"
	       (:newline-out))
    (ecase (the :style)
      (:image-figure 
       (write-env "\\begin{center}"
		  (:newline-out)
		  "\\includegraphics"
		  (:a (if (and (the :width) (the :height))
			  (format nil "[width=~a,height=~a]" (the :width) (the :height))
			""))
		  "{"
		  "../images/"
		  (:a (pathname-name (the :image-file)))
		  "."
		  (:a (or (pathname-type (the :image-file)) "pdf"))
		  "}"
		  (:newline-out)
		  "\\end{center}"))
      (:boxed-figure 
       (write-env "\\begin{lrbox}{\\boxedverb}"
		  (:newline-out)
		  "\\begin{minipage}{\\linewidth}")))
    
    (dolist (element (list-elements (the :elements)))
      (write-the-object element (base)))

    (when (eql (the :style) :boxed-figure)
      (write-env (:newline-out)
		 "\\end{minipage}"
		 (:newline-out)
		 "\\end{lrbox}"
		 (:newline-out)
		 "\\fbox{\\usebox{\\boxedverb}}"))
    (write-env (:newline-out)(:newline-out)
	       "\\caption{" (:a (the :caption)) "}"
	       (:newline-out)(:newline-out)
	       "\\label{" (:a (the :label)) "}"
	       (:newline-out)(:newline-out)
	       "\\end{figure}"
	       (:newline-out)))))


(define-lens (latex text-string)()
  :output-functions
  ((base
    (&key (escape-strings? t))
    (write-env (:a (funcall (if escape-strings? #'escape-string #'identity) (the :data)))))))
