(in-package :ta2)

(define-object part-type-form (base-html-sheet)
  :input-slots
  (make-instance-function set-root-function tatu-root package-default)
  
  :computed-slots
  ((object-type "" :settable)
   (object-expression "" :settable)
   (respondent (the tatu-root)))
  
  :functions
  ((after-set!
    ()
    (cond ((not (string-equal (the object-type) ""))
	   (funcall (the make-instance-function) (the object-type)))
	  ((not (string-equal (the object-expression) ""))
	   (funcall (the set-root-function) 
		    (let ((*package* (find-package (the package-default))))
		      (eval (read-safe-string (the object-expression))))))))))


(define-lens (html-format part-type-form)()
  :output-functions
  ((main-sheet     
    () 
    (html 
     (:html 
      (:head (:title "Specify Object Package and Type"))
      (:body 
       (with-html-form ()
	 
	 (:table (:tr ((:td :colspan 2 :align :center)
		       (:h3 "Specify object package and type, or an expression which returns an object.")
		       (:i "Default Package: the " (:princ-safe (package-name (find-package (the package-default)))) " Package")))
		 (:tr ((:td :bgcolor (gethash :gold-bright *color-table*)) (:b "Class Package:Type"))
		      (:td ((:input :type :string :name :object-type :size 40 :value (the object-type)))))
		 (:tr ((:td :bgcolor (gethash :gold-bright *color-table*)) (:b "Object Expression"))
		      (:td ((:input :type :string :name :object-expression :size 40 :value (the object-expression)))))
		 (:tr (:td :br) (:td ((:input :type :submit :name :submit :value " Browse!"))))))))))))
