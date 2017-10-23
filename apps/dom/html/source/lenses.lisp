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
(defparameter *footnotes* nil)
(defparameter *footnotetext* nil)
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
	     (let ((*index-hash* (make-hash-table :test #'equalp))
                   (*footnotes* (make-array 50 :adjustable t :fill-pointer 0))
                   (*footnotetext* nil))
	       (htm
		(:body (:div (write-the cl-who-contents-out))
		       (:div (write-the cl-who-base))
                       (when *footnotetext*
                         (htm (:div (:hr)
                                    (dolist (elements *footnotetext*)
                                      (htm (:p (dolist (element (list-elements elements))
                                                 (write-the-object element cl-who-base))))))))
                       (when (> (length *footnotes*) 0)
                         (htm (:div (write-the cl-who-footnotes))))
		       (:div (write-the cl-who-index))))))))
   
   (cl-who-footnotes
    ()
    (with-cl-who ()
      (:p (:h2 "Footnotes"))
      (loop for elements across *footnotes* as index upfrom 1
        do (htm
            (:p "[" (:a :name (format nil "FtNt~d" index) :href (format nil "#FtNtR~d" index)
                        (fmt "~d" index))
                "] "
                (dolist (element (list-elements elements))
                  (write-the-object element cl-who-base)))))))
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

        (:copyright (htm "&copy;"))
        (:rightarrow (htm "&rarr;"))
        (:$ (htm "&dollar;"))
        (:textgreater (htm "&gt;"))
        (:textasciitilde (htm "&tilde;"))

	((:index :indexed)
         (dolist (element (list-elements (the elements)))
           (let* ((data (the-object element data))
                  (anchor-tag (string (gensym))))
             (push anchor-tag (gethash data *index-hash*))
             (htm (:a :name anchor-tag))
             (when (eq (the markup-tag) :indexed)
               (write-the-object element cl-who-base)))))

        (:footnote
         (let ((index (1+ (vector-push-extend (the elements) *footnotes*))))
           (htm (:a :name (format nil "FtNtR~d" index) :href (format nil "#FtNt~d" index)
                    (fmt "[~d]" index)))))

        (:footnotetext
         (push (the elements) *footnotetext*))

        ((:cite :href)
         (let* ((args (list-elements (the elements)))
                (target (the-object (car args) data)))
           (htm (:a :href (if (eq (the markup-tag) :cite)
                           (format nil "#~a" target)
                           target)
                   (dolist (element (or (cdr args) args))
                     (write-the-object element cl-who-base))))))

        ;; :small and :tiny are always used as (:small (:verbatim ...)) and in most browsers,
        ;; verbatim is already small, so don't do anything.
        ((:no-op :small :tiny)
         (dolist (element (list-elements (the elements)))
           (write-the-object element cl-who-base)))

        (:include (dolist (element (list-elements (the elements)))
                    (let ((filename (the-object element data)))
                      (when (and (> (length filename) 8)
                                 (string= filename "~/gendl/" :end1 8))
                        (setq filename (merge-pathnames (subseq filename 8)
                                                        (asdf:system-source-directory :gendl))))
                      (with-open-file (s filename)
                        (let ((buffer (make-string (file-length s))))
                          (read-sequence buffer s)
                          (htm (esc buffer)))))))

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
			      (:image-figure
                               (htm (:img :src (let* ((image-file (if (stringp (the image-file))
								      (concatenate 'string "images/" (the image-file))
								      (namestring (the image-file))))
						      
						      (png-file (when (string-equal (pathname-type image-file) "pdf")
								  (make-pathname :type "png" :defaults image-file))))
						 (when png-file
						   (uiop:run-program
						    (list (format nil "~a" *gs-path*)
							  "-q"
							  (format nil "-sDEVICE=~a" "png256")
							  (format nil "-sOutputFile=~a" png-file)
							  (format nil "-dTextAlphaBits=~a" *gs-text-alpha-bits*)
							  (format nil "-dGraphicsAlphaBits=~a" *gs-graphics-alpha-bits*)
							  "-dSAFER"
							  "-dBATCH"
							  "-dNOPAUSE"
							  image-file))
						   ;;(uiop:copy-file png-file (merge-pathnames (file-namestring png-file) "~/tmp/images/"))
						   )
						 (namestring (merge-pathnames (file-namestring (or png-file image-file)) "images/")))
                                          :style (format nil "width:~a;height:~a;" (the width) (the height))
                                          :alt (the caption))))
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


    
