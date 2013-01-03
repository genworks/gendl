(in-package :gwl-user)

(eval-when (compile load)
  (use-package :surf)
  (export 'index))

(define-object index (base-html-sheet)
  :objects
  ((named-box-1 :type 'hey-now)
   (named-box-2 :type 'hey-now)))

(define-view (html-format index)()
  :output-functions
  ((main-sheet
    ()
    (html 
     (:html 
      (:head (:title "List of Boxes"))
      (:body (:h2 (:center "List of Boxes"))
	     (:ul
	      (dolist (child (the children))
		(html (:li (the-object child
				       write-self-link)))))))))))
		   
		   
  

(define-object hey-now (base-html-graphics-sheet)
  
  :computed-slots
  ((iges-url (let ((url (format nil "/iges-~a.igs" 
				(the instance-id))))
	       (publish 
		:path url
		:function 
		#'(lambda(req ent)
		    (with-http-response 
			(req ent :content-type "model/iges")
		      (with-http-body (req ent)
			(let ((stream 
			       (request-reply-stream req)))
			  (with-format (iges stream)
			    (write-the box cad-output)))))))
	       url))
   
   (username "Jake" :settable)
   
   (universal-time (get-universal-time))
   
   (time-string (multiple-value-bind 
		    (seconds minutes hours date month year)
		    (get-decoded-time)
		  (format nil "~a:~a:~a, ~a-~a-~a"
			  hours minutes seconds
			  year month date))))
  
  
  :objects
  ((box :type 'surf::test-b-spline-surface)
   
   (view-object :type 'web-drawing
		:page-width 500
		:page-length 300
		:projection-vector 
		(getf *standard-views* :trimetric)
		:objects (list (the box))))
  
  :functions
  ((before-present!
    ()
    (format t "I am in the before-present!~%"))
   
   (after-present!
    ()
    (format t "I am in the after-present!~%"))
   
   (before-set!
    ()
    (format t "I am in the before-set!~%"))
   
   (after-set!
    ()
    (format t "I am in the after-set!~%"))))

 
(define-view (html-format hey-now) ()
  :output-functions
  ((main-sheet 
    ()
    (html (:html (:head (:title "Hey Now"))
		 ((:body  :bgcolor "#eeeeee")
		  (the write-development-links)
		  (the write-back-link)
		  (with-html-form ()
		    (:p ((:input :type :text
				 :name :username
				 :value (the username))))
		    
		    ((:a :href (the iges-url))
		     "Download an IGES file of this thing")
		    
		    (:p ((:input :type :submit
				 :name :accept
				 :value " OK ")))
		    (the write-geometry))
		  
		  (:h1 "Hey now " (:princ (the username)))
		  (:p "The time is now: "
		      (:princ (the time-string)))))))))
  
(publish :path "/index"
	 :function #'(lambda(req ent)
		       (gwl-make-object req ent
					"gwl-user:index")))
	 
