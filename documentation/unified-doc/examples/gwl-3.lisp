(in-package :gwl-user)

(define-object presidents-with-pages (base-html-sheet)
  :input-slots
  ((presidents (list (list :name "Ford" :term 1974)
                     (list :name "Carter" :term 1976)
                     (list :name "Clinton" :term 1992)
                     (list :name "Bush" :term 2000)
                     (list :name "Obama" :term 2008)))
   
   (table-border 1))
  
  
  :objects
  ((president-pages :type 'president-page
                    :sequence (:size (length (the presidents)))
                    :name (getf (nth (the-child index) (the presidents))
                                :name)
                    :term (getf (nth (the-child index) (the presidents))
                                :term)))


  :functions
  ((write-html-sheet
    () 
    (with-cl-who (:indent t)
      (let ((title (format nil "Info on ~a Presidents:" 
                           (length (the presidents)))))
        (htm
         (:html 
          (:head (:title (str title)))
          (:body 
           (:p (:c (:h3 (str title))))
           (:ol
            (dolist (page (list-elements (the president-pages)))
              (htm      
               (:li
                (the-object 
                 page 
                 (write-self-link :display-string 
                                  (the-object page name)))))))))))))))

;;
;; Access the above example with 
;; http://localhost:9000/make?object=gwl-user::presidents-with-pages
;;
