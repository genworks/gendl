(in-package :gwl-user)

(define-object president-page (base-html-sheet)
  :input-slots
  (name term)
  
  :functions
  ((write-html-sheet
    ()
    (with-cl-who ()
      (let ((title (format nil "Term for President ~a:" 
                           (the name))))
        (htm
         (:html 
          (:head (:title (str title)))
          (:body 
           (the (write-back-link :display-string "&lt;Back"))
           (:p (:c (:h3 (str title))))
           (:p (str (the term)))))))))))

      
