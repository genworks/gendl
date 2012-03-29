(in-package :gwl-user)

(define-object revenue-lookup (base-ajax-sheet)
  
  :input-slots
  
  ((revenue-data '(2003 25000
                   2004 34000
                   2005 21000
                   2006 37000
                   2007 48000
                   2008 54000
                   2009 78000)))
  
  :computed-slots
  
  ((main-sheet-body 
    (with-cl-who-string ()
      (str (the main-section main-div)))))
  
  :objects
  
  ((table-border :type 'menu-form-control
                 :size 1
                 :choice-list '(0 1)
                 :default 0
                 :ajax-submit-on-change? t)
   
   (cell-padding :type 'menu-form-control
                 :size 1
                 :choice-list '(0 3 6 9 12)
                 :default 0
                 :ajax-submit-on-change? t)
   
   (selected-year :type 'menu-form-control
                  :size 1
                  :choice-list (plist-keys (the revenue-data))
                  :default (first (the-child choice-list))
                  :ajax-submit-on-change? t)
   
   (main-section 
    :type 'sheet-section
    :inner-html (with-cl-who-string ()
                 (:p (str (the development-links)))
                 (:p (str (the table-border html-string)))
                 (:p (str (the cell-padding html-string)))
                 (:p (str (the selected-year html-string)))
                 (:p ((:table :border (the table-border value)
                              :cellpadding (the cell-padding value))
                      (:tr (:th (fmt "Revenue for Year ~a:" 
                                     (the selected-year value)))
                           (:td (str (getf (the revenue-data) 
                                           (the selected-year value)))))))))))

(publish-gwl-app "/revenue-lookup" 
                 "gwl-user::revenue-lookup")




