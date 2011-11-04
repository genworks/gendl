(in-package :gwl-user)

(define-object revenue-lookup-old-school (base-ajax-sheet)
  
  :input-slots
  
  ((revenue-data '(2003 25000
                   2004 34000
                   2005 21000
                   2006 37000
                   2007 48000
                   2008 54000
                   2009 78000)))
  
  :functions
  
  ((write-html-sheet
    ()
    (with-cl-who ()
      (when *developing?* (str (the development-links)))
      (with-html-form (:cl-who? t)
        (:p (str (the table-border html-string)))
        (:p (str (the cell-padding html-string)))
        (:p (str (the selected-year html-string)))
        (:p ((:input :type :submit :value " OK "))))
      (:p ((:table :border (the table-border value)
                   :cellpadding (the cell-padding value))
           (:tr (:th (fmt "Revenue for Year ~a:" 
                          (the selected-year value)))
                (:td (str (getf (the revenue-data) 
                                (the selected-year value))))))))))

  :objects
  
  ((table-border :type 'menu-form-control
                 :size 1 :choice-list '(0 1)
                 :default 0)
   
   (cell-padding :type 'menu-form-control
                 :size 1 :choice-list '(0 3 6 9 12)
                 :default 0)
   
   (selected-year :type 'menu-form-control
                  :size 1 :choice-list (plist-keys (the revenue-data))
                  :default (first (the-child choice-list)))))
   

(publish-gwl-app "/revenue-lookup-old-school" 
                 "gwl-user::revenue-lookup-old-school")


;;
;; Access the above example with 
;; http://localhost:9000/make?object=gwl-user::revenue-lookup-old-school
;;
