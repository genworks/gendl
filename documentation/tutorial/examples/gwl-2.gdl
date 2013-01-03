(in-package :gwl-user)

(define-object presidents (base-html-sheet)
  :input-slots
  ((presidents (list (list :name "Ford"
                           :term 1974)
                     (list :name "Carter"
                           :term 1976)
                     (list :name "Clinton"
                           :term 1992)
                     (list :name "Bush"
                           :term 2000)
                     (list :name "Obama"
                           :term 2008)))
   
   (table-border 1))

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
           ((:table :border (the table-border))
            (:tr (:th "Name") (:th "Term"))
            (dolist (president (the presidents))
              (htm      
               (:tr (:td (str (getf president :name)))
                    (:td (str (getf president :term)))))))))))))))
;;
;; Access the above example with 
;; http://localhost:9000/make?object=gwl-user::presidents
;;
