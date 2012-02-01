(in-package :yadd)

(defparameter *data-pathname*
  (make-pathname :directory (pathname-directory excl:*source-pathname*)
		 :device (pathname-device  excl:*source-pathname*)))


(define-object document-yadd (base-object)
  :computed-slots ((generate-documentation 
                    (progn
                      (the gdl write-package-index)
                      (the gdl write-class)
                 
		      ;;#+nil
		      (the gwl write-package-index)
		      (the gwl write-class)
                  
		      (the geom-base write-package-index)
		      (the geom-base write-class)
		      
		      (the surf write-package-index)
		      (the surf write-class)
		      ;;-!!!!!!!!

                      (pprint 'Generate-Documentation---OK)) :uncached))
  :objects
  ((gdl :type 'yadd2pdf
        :package :gdl)
   ;;#+nil
   (gwl :type 'yadd2pdf
        :package :gwl)
   ;;#+nil
   (surf :type 'yadd2pdf
         :package :surf )
   ;;#+nil
   (geom-base :type 'yadd2pdf
              :image? t
              :package :geom-base)))
   
(define-object  yadd2pdf (base-object)
  :input-slots ((image? nil)
                (package :surf ))
  :computed-slots 
  ((data-directory *data-pathname*)
   (sub-dir (concatenate 'string "yadd/" 
                         (getf 
                          (first (the part-documentation-plist)) :packager) "/"))

   (directory-name (ensure-directories-exist (merge-pathnames (the sub-dir) (the data-directory))))

   (doc (make-object 'yadd::assy)) 
   
   (documented-package-index #+nil
                             (list-elements 
                              (the doc (:package-dokumentations 
                                        (position (the package) 
                                                  (the doc packages-to-document)))
                                   :object-docs ))
                             
                             (list-elements
                              (the doc (:package-dokumentations 
                                        (position (the package) 
                                                  (the doc packages-to-document)))
                                   object-docs dokumentation-external)))
   
   (part-documentation-plist 
    (mapcar #'(lambda (doc-list) 
                (append
                 '(:image-file) (list (the-object doc-list image-file))  
                 '(:packager) (list (the-object doc-list part-package))
                 '(:classr) (list (the-object doc-list strings-for-display))
                 (the-object doc-list part-documentation-plist)))
                                     (the  documented-package-index)))
   
   (doc-messages 
    (mapcar #'(lambda (doc-sections) 
                (mapcar #'(lambda (doc-messages) 
                            (append 
                             (list (the-object doc-messages category) 
                                   (the-object doc-messages message-names)) 
                             (list 
                              (mapcar #'(lambda (doc)  
                                          (the-object doc ;;;remark-string
						      section-plist))
                                      (list-elements 
                                       (the-object doc-messages messages))))
                             (list (the-object doc-messages message-and-remarks)) 
                             ))
                        (list-elements 
                         (the-object doc-sections sections)))) 
            (the documented-package-index)))
   (doc-messages-s 
    (mapcar #'(lambda (doc-sections) 
                (mapcar #'(lambda (doc-messages) 
                            (append 
                             (list (the-object doc-messages category) 
                                   (the-object doc-messages message-names)) 
                             (list 
                              (mapcar #'(lambda (doc)  
                                          (the-object doc remark-string
						      ))
                                      (list-elements 
                                       (the-object doc-messages messages))))
                             (list (the-object doc-messages message-and-remarks)) 
                             ))
                        (list-elements 
                         (the-object doc-sections sections)))) 
            (the documented-package-index)))
 
   (doc-sections 
    (mapcar #'(lambda (doc-sections) 
                (mapcar #'(lambda (doc-messages)
                            (list-elements (the-object doc-messages messages)))
                        (list-elements 
                         (the-object doc-sections sections)))) 
            (the documented-package-index))))
  
  
  :objects ((pdf-files :type 'pdf-gen
                       :package-index (the documented-package-index)
                       :directory  (ensure-directories-exist (the directory-name))
                       #+nil
                       (if (probe-directory (the directory-name))
                           (the directory-name) 
                         (excl.osi:mkdir (the directory-name) :all t))))


  :functions
  ((write-package-index 
    ()
    (let ((file-name 
           (concatenate 'string  
             (getf (first (the part-documentation-plist)) 
                   :packager) ".texi")))
      (with-open-file (out 
                       (merge-pathnames 
                        file-name 
                        (merge-pathnames "../"(the pdf-files directory)))
                       :direction :output
                       :if-exists :new-version
                       :if-does-not-exist :create)
        (format out  "@subsection ~@(~a~)~%~% " 
                (getf (first (the part-documentation-plist)) :packager))
       
        (format out " ~{@include ~a~%~}"  
                (mapcar #'(lambda (files-name)  
                            (concatenate 'string (the sub-dir )
                                         (remove #\* (getf files-name :classr)) ".texi"))
                        (the part-documentation-plist)))
        ;;-------------------------------------------------------------------------------        
        ;; Used for global pathnames 
        #+nil  
        (format out " ~{@include ~a~%~}"  
                (mapcar #'(lambda (files-name)  
                            (merge-pathnames 
                             (the pdf-files directory) 
                             (concatenate 'string 
                               (remove #\* (getf files-name :classr)) ".texi")))
                        (the part-documentation-plist))))))
        ;;-------------------------------------------------------------------------------   
   (write-class
    ()
    (let ((index (length (the documented-package-index))))
      (dotimes (n index)
        (let ((file-name 
               (remove #\* (concatenate 'string  
                             (getf (nth n (the part-documentation-plist)) :classr)
                             ".texi"))))
          ;;it is necessary to remove characters * : ? .etc from the ilfe-names
          
          (with-open-file (out 
                           (merge-pathnames 
                            (the pdf-files directory) 
                            file-name)
                           :direction :output
                           :if-exists :new-version
                           :if-does-not-exist :create)
            (format out "@subsubsection ~@(~a~)~%~%" 
                    (getf (nth n(the part-documentation-plist)) :classr))
            
            (format out "@b{Description}~%~%" )
            (format out "~a~%~%" 
                    (if (getf (nth n (the part-documentation-plist)) 
                              :description) 
                        (excl:replace-re 
                         (excl:replace-re 
			  (excl:replace-re 
			   (excl:replace-re 
			    (excl:replace-re 
			     (excl:replace-re 
			      (excl:replace-re
			       (excl:replace-re
				(excl:replace-re 
				 (excl:replace-re
				  (excl:replace-re 
				   (excl:replace-re 
				    (getf (nth n (the part-documentation-plist)) 
					  :description)  
				    "@" "(at)")
				   "" "")
				  "<ol>" " @enumerate ")
				 "</ol>" "
@end enumerate

")
				"<li>" " @item " )
			       "</li>" "" )
			      "<ul>" " @itemize ")
			     "</ul>" "
@end itemize 

")
			    "<tt>" "@i{" )
			   "</tt>" "}") 
			  "<i>" "@i{" ) 
			 "</i>" "} ")
			
			;;italic it has to be replaced too.
			"!!! Not applicable for this object !!!"))
	    

	    (mapcar #'(lambda (documentation documentation-s) 
			(if  (fourth documentation) 
			     (if (eql :functions (first documentation))
				 (progn	
				   (format out "@noindent @b{~@(~a~)}~%~%" (first documentation))

				   (let ((index (length (second documentation)))) 
				     (dotimes (n index)
				       (when (nth n (third documentation))
					 
					 ;; Flag!!!
					 ;; condition used to remove the undocumented slots
					 ;;

					 (if (getf (nth n (third documentation)) :type);; condition used to remove the undocumented slots    
					     (format out "~%~%@noindent @b{~a} @i{~a}~%~%" 
						     (nth n (second documentation)) (getf (nth n (third documentation)) :type))
					     (format out "~%~%"))
					 
					 (format out "~a~%~%"  (html2texi-string (getf (nth n (third documentation)) :intro)))
					 (if (getf (nth n (third documentation)) :&key) 
					     (format out "~%~%@i{Keyword Arguments:} ~%~% @itemize {}~%~% ~{@item ~a~}~%~%@end itemize~%~%"
						     (mapcar #'(lambda (arg-1 arg-2) 
								 (format nil "@b{~a} ~a, Default Value: ~a, ~{~a.~} ~%~%"
									 (if  (listp arg-1) (first arg-1) arg-1)
									 (first (split arg-2 #\.))
									 (if  (listp arg-1) (second arg-1) "@code{nil}")
									 (cdr (split arg-2 #\.))))
							     (plist-keys  (getf (nth n (third documentation)) :&key))
							     (plist-values  (getf (nth n (third documentation)) :&key))))
					     (format out "~%~%"))
					 
					 (if (getf (nth n (third documentation)) :&optional) 
					     (format out "~%~%@i{Optional Arguments:} ~%~% @itemize {}~%~% ~{@item ~a~}~%~%@end itemize~%~%"
						     (mapcar #'(lambda (arg-1 arg-2) 
								 (format nil "@b{~a} ~a, Default Value: ~a, ~{~a.~} ~%~%"
									 (if  (listp arg-1) (first arg-1) arg-1)
									 (first (split arg-2 #\.))
									 (if  (listp arg-1) (second arg-1) "@code{nil}")
									 (cdr (split arg-2 #\.))))
							     (plist-keys  (getf (nth n (third documentation)) :&optional))
							     (plist-values  (getf (nth n (third documentation)) :&optional))))
					     (format out "~%~%"))

					 
					 (if (getf (nth n (third documentation)) :&rest) 
					     (format out "~%~%@i{Optional Arguments:} ~%~% @itemize {}~%~% ~{@item ~a~}~%~%@end itemize~%~%"
						     (mapcar #'(lambda (arg-1 arg-2) 
								 (format nil "@b{~a} ~a, Default Value: ~a, ~{~a.~} ~%~%"
									 (if  (listp arg-1) (first arg-1) arg-1)
									 (first (split arg-2 #\.))
									 (if  (listp arg-1) (second arg-1) "@code{nil}")
									 (cdr (split arg-2 #\.))))
							     (plist-keys  (getf (nth n (third documentation)) :&rest))
							     (plist-values  (getf (nth n (third documentation)) :&rest))))
					     (format out "~%~%"))
					 
					 (if (getf (nth n (third documentation)) :&body) 
					     (format out "~%~%@i{Optional Arguments:} ~%~% @itemize {}~%~% ~{@item ~a~}~%~%@end itemize~%~%"
						     (mapcar #'(lambda (arg-1 arg-2) 
								 (format nil "@b{~a} ~a, Default Value: ~a, ~{~a.~} ~%~%"
									 (if  (listp arg-1) (first arg-1) arg-1)
									 (first (split arg-2 #\.))
									 (if  (listp arg-1) (second arg-1) "@code{nil}")
									 (cdr (split arg-2 #\.))))
							     (plist-keys  (getf (nth n (third documentation)) :&body))
							     (plist-values  (getf (nth n (third documentation)) :&body))))
					     (format out "~%~%"))
					 
					
					 
					 
					 
)))) 
					
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;;#+nil	
				 (progn	
				  (format out "@noindent @b{~@(~a~)}~%~%" (first documentation-s))
				  (let ((index (length (second documentation-s)))) 
				    (dotimes (n index)
				      (when (nth n (third documentation-s))
					(format out "@noindent @b{~a}~%~%" 
						(nth n (second documentation-s)))
					(format out "~%~% @itemize {}~%~% ~%~% @item ~%~% ~a~%~%@end itemize~%~%"
						(excl:replace-re
						 (excl:replace-re
						  (excl:replace-re
						   (excl:replace-re
						    (excl:replace-re
						     (excl:replace-re
						      (excl:replace-re
						       (excl:replace-re
							(excl:replace-re
							 (excl:replace-re
							  (excl:replace-re
							   (excl:replace-re
							    (excl:replace-re
							     (excl:replace-re
							      (excl:replace-re
							       (excl:replace-re
								(excl:replace-re
								 (excl:replace-re
								  (excl:replace-re
								   (excl:replace-re
								    (excl:replace-re
								     (excl:replace-re
								      (excl:replace-re
								      ;; (excl:replace-re
								;;	(excl:replace-re
								;;	 (excl:replace-re
									  (nth n (third documentation-s))
									  ;;"" "")
									 "<p>" "" )
									"</p>" "" )
								       "<ol>" " @enumerate ")
								      "</ol>" "
@end enumerate

")
								    ;; "<pre>" " @smallexample ")
								   ;; "</pre>" " @end smallexample ")
								   "<i>" "@i{" )
								  "</i>" "} ")
								 "<b>" "@b{" )
								"</b>" "} ")
							       "<tt>" "" )
							      "</tt>" "")
							     "
" "")
							    "<ul>" " @itemize ")
							   "</ul>" "
@end itemize

")
							  "<li>" " @item " )
							 "</li>" "" )
							"<dl>" " @itemize {} " )
						       "</dl>" "
@end itemize

")
						      "<dt>" " @item " )
						     "</dt>" "" )
						    "<strong>" " @b{")
						   "</strong>" "} ")
						  "<dd>" " @itemize @minus ")
						 "</dd>" "
@end itemize

"))))))) 
			     
			;;;;;;;;;;	       
			     (format out "~%~%")))
		    
                    (nth n  (the  doc-messages )) (nth n  (the  doc-messages-s )))            
            
            (if (getf (nth n (the part-documentation-plist)) :examples)
		
                (format out "@noindent @b{Examples}~%~%" )
		
                
              (format out "~%~%"))
            
           
            (format out "~a~%~%" 
                    (if (getf (nth n (the part-documentation-plist)) :examples)
                      (excl:replace-re 
                       (excl:replace-re 
                        (excl:replace-re 
                         (getf (nth n  (the part-documentation-plist)) :examples)  
                         "@" "(at)" ) 
                        "<pre>" "@smallexample")
     


;; @cartouche" ) 
                       "</pre>" "@end smallexample")
;;"@end cartouche 
;;@end smallexample")
                    ""))
          (if (the image?)
              (if (getf (nth n (the part-documentation-plist)) :examples)
       
                  (format out "@center @image{~aimage/~a,,2.8in}~%~%" (the pdf-files directory)  
                          (getf (nth n (the part-documentation-plist)) :classr))
                "" ) 
             
            ;;(format out "~%~% @b{Example image is not generated!}~%~%")
	     (format out "~%~%~%~%"))

          )))))))

(define-object pdf-gen (base-object)
  :input-slots (package-index directory)
  :computed-slots ()
  :hidden-objects
  ((general-description :type 'base-object)))

(defun html2texi-string (my-string) 


	(excl:replace-re
						 (excl:replace-re
						  (excl:replace-re
						   (excl:replace-re
						    (excl:replace-re
						     (excl:replace-re
						      (excl:replace-re
						       (excl:replace-re
							(excl:replace-re
							 (excl:replace-re
							  (excl:replace-re
							   (excl:replace-re
							    (excl:replace-re
							     (excl:replace-re
							      (excl:replace-re
							       (excl:replace-re
								(excl:replace-re
								 (excl:replace-re
								  (excl:replace-re
								  ; (excl:replace-re
								  ;  (excl:replace-re
								    ;; (excl:replace-re
								     ;; (excl:replace-re
								  
									my-string  
									 
									 "<p>" "" )
									"</p>" "" )
								      ;; "<ol>" " @enumerate ")
								    ;;  "</ol>" "
;@end enumerate

;")
								   "<i>" "@i{" )
								  "</i>" "} ")
								 "<b>" "@b{" )
								"</b>" "} ")
							       "<tt>" "@code{" )
							      "</tt>" "}")
							     "
" "")
							    "<ul>" " @itemize ")
							   "</ul>" "
@end itemize

")
							 ;; "<li>" " @item " )
							 ;;"</li>" "" )
							"<dl>" " @itemize {} " )
						       "</dl>" "
@end itemize

")
						      "<dt>" " @item " )
						     "</dt>" "" )
						    "<strong>" " @b{")
						   "</strong>" "} ")
						  "<dd>" " @itemize @minus ")
						 "</dd>" "
@end itemize

"))



