(in-package :com.genworks.dom-html)


(publish-directory :prefix "/gdl/style/"
		   :destination (format nil "~a" (translate-logical-pathname "~/genworks/gwl-apps/training/g101/style/")))

(define-lens (html-format assembly) ()
  :output-functions
  ((main-sheet
    ()
    (html
     (:html (:head (:title (:princ (the title)))
		   ((:link :href (the style-url) :rel "stylesheet" :type "text/css")))
	    (:body (:p (write-the contents))
		   (:p (write-the base))))))

   
   (base
    ()
    (print-messages attributes elements)
    (dolist (element (list-elements (the elements)))
      (write-the-object element base)))))

      
(define-lens (html-format base-document-component)()
  :output-functions
  ((base ())
   
   (contents 
    ()
    (html ((:a :href (format nil "#~a" (the label))) (:princ (the title)))
	  (:ol
	   (dolist (section (the sections-list))
	     (html (:li (write-the-object section contents)))))))))
   



(define-lens (html-format section)()
  :output-functions
  ((base
    ()
    (html ((:a :name (the label)))
	  (:h2 (:princ (the title)))
	  (dolist (element (list-elements (the elements)))
	    (write-the-object element base))))))

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
			    (write-the-object element base)))))))))


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
				 (write-the-object element (:base))))))))))


(define-lens (html-format item-list-item)()
  :output-functions
  ((:base
    ()
    (if (typep (the word) 'null-part)
	(html (:li 
	       (dolist (element (list-elements (the :elements)))
		 (write-the-object element (:base)))))
      (html (:dt (write-the word base))
	    (:dd (dolist (element (list-elements (the :elements)))
		   (write-the-object element (:base)))))))))
      

(define-lens (html-format figure)()
  :output-functions
  ((base
    ()
    (html (:p (:table (:tr (:td (ecase (the style)
				  (:image-figure (html "Image Goes Here"))
				  (:boxed-figure (dolist (element (list-elements (the :elements)))
						   (write-the-object element (:base)))))))
		      (:tr (:td (:i (:princ (the caption)))))))))))



(define-lens (html-format text-string)()
  :output-functions
  ((base () (html (:princ-safe (the data))))))


    
