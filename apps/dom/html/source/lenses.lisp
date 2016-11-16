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

(in-package :com.genworks.dom-html)


(defparameter *warn-on-unrecognized-tags?* t)

(publish-directory :prefix "/gdl/style/"
		   :destination (format nil "~a" (translate-logical-pathname "~/genworks/gwl-apps/training/g101/style/")))


(defparameter *index-hash* nil)
(defparameter *debug-index* nil)

(define-lens (html-format assembly) ()
  :output-functions
  (
   (main-sheet
    ()
    (html
     (:html (:head (:title (:princ (the title)))

	    (:body (:p (write-the contents))
		   (:p (write-the base)))))))

   (base
    ()
    (dolist (element (list-elements (the elements)))
      (write-the-object element base)))
   

   (cl-who-out 
    ()
    (with-cl-who () 
      (:html (:head (:title (str (the title)))
		    ((:link :href (the style-url) :rel "stylesheet" :type "text/css")))
	     (let ((*index-hash* (make-hash-table :test #'equalp)))
	       (htm
		(:body (:div (write-the cl-who-contents-out))
		       (:div (write-the cl-who-base))
		       (:div (write-the cl-who-index))))))))
   
   (cl-who-index 
    ()
    (with-cl-who ()
      (:p (:h2 "Index"))
      (let* ((index-plist (list-hash *index-hash*))
	     (keys (sort (plist-keys index-plist) #'string-lessp)))
	(htm (:ul
	      (dolist (key keys)
		(dolist (target (gethash key *index-hash*))
		  (htm (:li ((:a :href (format nil "#~a" target)) (str key)))))))))))

   (cl-who-base
    ()
    (dolist (element (list-elements (the elements)))
      (write-the-object element cl-who-base)))

   ))



(define-lens (html-format yadd:assy)()

  :output-functions
  ((cl-who-contents-out ())
   (cl-who-out ())
   (cl-who-base ())))

      
(define-lens (html-format base-document-component)()
  :output-functions
  ((base ())
   


   (contents 
    ()
    (html ((:a :href (format nil "#~a" (the label))) (:princ (the title)))
	  (:ol
	   (dolist (section (the sections-list))
	     (html (:li (write-the-object section contents)))))))

   (cl-who-contents-out 
    ()
    (with-cl-who ()
      ((:a :href (format nil "#~a" (the label))) (str (the title)))
      (:ol
       (dolist (section (the sections-list))
	 (htm (:li (write-the-object section cl-who-contents-out)))))))

   (cl-who-base ())))




(define-lens (html-format section)()
  :output-functions
  ((base
    ()
    (html ((:a :name (the label)))
	  (:h2 (:princ (the title)))
	  (dolist (element (list-elements (the elements)))
	    (write-the-object element base))))

   (cl-who-base
    ()
    (with-cl-who ()
      ((:a :name (the label)))
      (:h2 (str (the title)))
      (dolist (element (list-elements (the elements)))
	(write-the-object element cl-who-base))))))


(define-lens (html-format marked-up-string)()
  :output-functions
  ((base
    ()
    
    (print-messages markup-tag)
    
    (case (the :markup-tag)
      (:p (html (:p (dolist (element (list-elements (the :elements)))
		      (write-the-object element (:base))))))
      
      (:define-object 
	  (format t "Hey now")
	  (write-the yadd (find-object-doc (the (elements 0))) pretty-definition))
      
      (:verbatim
       
       (html (:pre 
	      (dolist (element (list-elements (the :elements)))
		(write-the-object element base)))))

      (:emph (html (:i (dolist (element (list-elements (the :elements)))
			 (write-the-object element base)))))
      
      (:texttt (html (:tt (dolist (element (list-elements (the :elements)))
			    (write-the-object element base)))))))


   (cl-who-base
    ()
    
    (with-cl-who ()
      (case (the markup-tag)
	(:p (htm (:p (dolist (element (list-elements (the :elements)))
			(write-the-object element cl-who-base)))))
      
	(:define-object 
	 (let ((net.html.generator:*html-stream* *stream*))
	   (write-the yadd (find-object-doc (the (elements 0))) pretty-definition)))
      
	(:verbatim
       
	 (htm (:pre 
	       (dolist (element (list-elements (the elements)))
		 (write-the-object element cl-who-base)))))

	((:emph :i) (htm (:i (dolist (element (list-elements (the elements)))
                               (write-the-object element cl-who-base)))))

	((:textbf :b) (htm (:b (dolist (element (list-elements (the elements)))
                                 (write-the-object element cl-who-base)))))
      
	(:texttt (htm (:tt (dolist (element (list-elements (the elements)))
			     (write-the-object element cl-who-base)))))
	
        (:underline (htm (:span :style "text-decoration: underline;"
                                (dolist (element (list-elements (the elements)))
                                  (write-the-object element cl-who-base)))))
	
	(:quote (htm (:blockquote (dolist (element (list-elements (the elements)))
				    (write-the-object element cl-who-base)))))

	(:index (dolist (element (list-elements (the elements)))
		  (let* ((data (the-object element data))
			 (anchor-tag (string (gensym)))
			 (current (gethash data *index-hash*)))
		    (if current (pushnew anchor-tag current)
			(setf (gethash data *index-hash*) (list anchor-tag)))
		    (htm ((:a :name anchor-tag))
			 (write-the-object element cl-who-base)))))

        (:no-op
         (dolist (element (list-elements (the elements)))
           (write-the-object element cl-who-base)))

	(otherwise (when *warn-on-unrecognized-tags?*
		     (warn "Markup tag ~s was not recognized~%" (the markup-tag))))
	

	)))))



(define-lens (html-format item-list)()
  :output-functions
  ((:base
    ()
    (ecase (the style)
      (:itemize (html (:ul (dolist (element (list-elements (the :elements)))
			     (write-the-object element (:base))))))
      (:enumerate (html (:ol (dolist (element (list-elements (the :elements)))
			       (write-the-object element (:base))))))
      (:description (html (:dl (dolist (element (list-elements (the :elements)))
				 (write-the-object element (:base))))))))

   (cl-who-base
    ()
    (with-cl-who ()
      (ecase (the style)
	(:itemize (htm (:ul (dolist (element (list-elements (the :elements)))
			      (write-the-object element cl-who-base)))))
	(:enumerate (htm (:ol (dolist (element (list-elements (the :elements)))
				(write-the-object element cl-who-base)))))
	(:description (htm (:dl (dolist (element (list-elements (the :elements)))
				  (write-the-object element cl-who-base))))))))))


(define-lens (html-format item-list-item)()
  :output-functions
  ((base
    ()
    (if (typep (the word) 'null-part)
	(html (:li 
	       (dolist (element (list-elements (the :elements)))
		 (write-the-object element (:base)))))
      (html (:dt (write-the word base))
	    (:dd (dolist (element (list-elements (the :elements)))
		   (write-the-object element (:base)))))))

   (cl-who-base
    ()
    (with-cl-who ()
      (if (typep (the word) 'null-part)
	  (htm (:li 
		(dolist (element (list-elements (the :elements)))
		  (write-the-object element cl-who-base))))
	  (htm (:dt (write-the word cl-who-base))
		(:dd (dolist (element (list-elements (the :elements)))
		       (write-the-object element cl-who-base)))))))))
      

(define-lens (html-format figure)()
  :output-functions
  ((base
    ()
    (html (:p (:table (:tr (:td (ecase (the style)
				  (:image-figure (html "Image Goes Here"))
				  (:boxed-figure (dolist (element (list-elements (the :elements)))
						   (write-the-object element (:base)))))))
		      (:tr (:td (:i (:princ (the caption)))))))))

   (cl-who-base
    ()
    (with-cl-who ()
      (:p (:table (:tr (:td (ecase (the style)
			      (:image-figure "Image Goes Here")
			      (:boxed-figure (dolist (element (list-elements (the :elements)))
					       (write-the-object element cl-who-base))))))
	    (:tr (:td (:i (:princ (the caption)))))))))))



(define-lens (html-format blockquote) ()
  :output-functions
  ((cl-who-base 
    ()
    (with-cl-who ()
      (:blockquote (dolist (element (list-elements (the elements)))
		     (write-the-object element cl-who-base)))))))


(define-lens (html-format text-string)()
  :output-functions
  ((base () (html (:princ-safe (the data))))

   (cl-who-base () (with-cl-who () (esc (the data))))))


    
