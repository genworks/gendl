;;
;; Copyright 2002, 2009 Genworks International
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


(in-package :training-g108)


(define-object hello-world (base-ajax-sheet)

  :computed-slots
  ((main-sheet-body (with-cl-who-string ()
                      "Hello, World!"))))

(publish-gwl-app "/hello-world"
                 "training-g108::hello-world")


(define-object hello-world-htm (base-ajax-sheet)
  
  :computed-slots
  ((main-sheet-body (with-cl-who-string ()
                      (:p 
                       (:b (:i "Hello, World!")))))))


(publish-gwl-app "/hello-world-htm"
                 "training-g108::hello-world-htm")


(define-object revenue-table (base-ajax-sheet)
  
  :input-slots
  ((sample-table-data '(("Year" "Revenue")
                        (2003  25000)
                        (2004  34000)
                        (2005  21000)
                        (2006 37000)
                        (2007 48000)
                        (2008 54000)
                        (2009 78000))))

  :computed-slots
  ((main-sheet-body 
    (with-cl-who-string ()
      (:p
       ((:table :border 1)
        (:tr (dolist (cell (first (the sample-table-data)))
               (htm (:th (str cell)))))
        (dolist (row (rest (the sample-table-data)))
          (htm (:tr
                (dolist (cell row)
                  (htm (:td (str cell)))))))))))))


(publish-gwl-app "/revenue-table"
                 "training-g108::revenue-table")




(define-object revenue-table (base-ajax-sheet)
  
  :computed-slots
  ((sample-table-data '(("Year" "Revenue")
                        (2003  25000)
                        (2004  34000)
                        (2005  21000)
                        (2006 37000)
                        (2007 48000)
                        (2008 54000)
                        (2009 78000)))

   (main-sheet-body 
    (with-cl-who-string ()
      (:p
       ((:table :border 1)
        (:tr (dolist (cell (first (the sample-table-data)))
               (htm (:th (str cell)))))
        (dolist (row (rest (the sample-table-data)))
          (htm (:tr
                (dolist (cell row)
                  (htm (:td (str cell)))))))))))))



(define-object text-form (base-ajax-sheet)
  
  :computed-slots
  ((main-sheet-body
    (the main-area main-div))
   
   (result (* (the number-input value) 2)))
  
  :functions ((restore-defaults! 
	       ()
	       (the number-input :restore-defaults!)))
				    

  :objects ((reset :type 'button-form-control
		   :label "Reset"
		   :onclick (the (gdl-ajax-call :function-key :restore-defaults!)))

	    (number-input :type 'text-form-control
                          :prompt "Enter a number"
                          :default 42
                          )
	    
	    (main-area :type 'sheet-section
		       ;;
		       ;; FLAG -- use :inner-html instead of :main-view starting in 1581!
		       ;;
		       :main-view (with-cl-who-string ()
				    (with-html-form (:cl-who? t)
				      (:p (str (the number-input html-string)) :br
					  ((:input :type :submit :value "ok"))
					  " "
					  (str (the reset form-control-string)))
				      (:p (str (the result))))))))



(publish-gwl-app "/text-form"
                 "training-g108::text-form")


;;
;; Basic UI with graphics area
;;

(define-object box-with-inputs (base-ajax-sheet)
  
  :computed-slots
  (
   #+nil
   (html-sections (list (the inputs-section)
                        (the top-viewport-section)
                        (the tri-viewport-section)))
   
   (box-width nil :settable)
   
   (use-raphael? t)
   
   (main-sheet-body 
    (with-cl-who-string ()
      
      (:p (when *developing?* (the write-development-links)))
      
      (:p ((:span 
            :style "cursor: pointer;"
            :onclick (the (gdl-ajax-call :function-key
                                         :print-time)))
           "Click here"))
      
      (:p
       (str (the inputs-section main-div)))
      
      (:table
       (:tr
        (:td (str (the top-viewport-section main-div)))
        (:td (str (the tri-viewport-section main-div)))
        
        )))))

  :functions
  ((print-time 
    ()
    (format *trace-output* "~a" (get-universal-time)))
   
   (print-root-path
    (object &key fancy?)
    (let ((root-path (the-object object root-path)))
      (format *trace-output* "~&~s~%" root-path))
    (when fancy?
      (format *trace-output* "isn't that fancy?~%"))))


  :objects
  ((box :type 'box
        :height 10 
        :width (or (the  box-width)
                   (the inputs-section box-width))
        :length (the inputs-section box-length))
   
   
   
   (inputs-section :type 'inputs-section
                   :box-width-default (the box-width))

   
   (top-viewport-section :type 'base-ajax-graphics-sheet
                         :view-direction-default :top
                         :image-format-default :raphael
                         :display-list-objects (list (the box))
                         :length 300
                         :width 300)

   
   (tri-viewport-section :type 'base-ajax-graphics-sheet
                         :view-direction-default :trimetric
                         :image-format-default :raphael
                         :display-list-objects (list (the box))
                         :length 300
                         :width 300)))
  


(define-object inputs-section (sheet-section)

  :input-slots
  (box-width-default)
  
  :computed-slots 
  ((box-length (the box-length-input value))
   (box-width (the box-width-input value))
   
   
   (inner-html (with-cl-who-string ()
                (:p (str (the box-length-input html-string)))
                (:p (str (the box-width-input html-string)))
                (:p (str (the button form-control-string)))
                )))

  
  :objects 
  ((button :type 'button-form-control
           :label " Reset "
           :onclick (the (gdl-ajax-call :function-key
                                        :restore-defaults!)))

   
   (box-width-input :type 'text-form-control
                    :default (or (the box-width-default) 25)
                    :allow-invalid? nil
                    :validation-function #'(lambda(width)
                                             (< 10 width 30))

                    :size (length (format nil "~a" (the-child value)))
                    :prompt "Box Width: "
                    :ajax-submit-on-change? t)
   
   (box-length-input :type 'text-form-control
                     :default 42
                     :prompt "Box Length: "
                     :ajax-submit-on-change? t))
  :functions
  ((restore-defaults!
    ()
    (format t "~&Should restore defaults.~%~%")
    (the box-width-input (restore-slot-default! :value))
    (the box-length-input (restore-slot-default! :value))
    
    )))


(publish-gwl-app "/box-with-inputs" 
                 "training-g108::box-with-inputs")






(define-object readable-inputs-form (base-ajax-sheet)
  
  :computed-slots
  ((main-sheet-body
    
    (with-cl-who-string ()
      (str (the development-links))
      (with-html-form (:cl-who? t)
        (:p (str (the expression-input form-control-string)) :br
            (str (the symbol-input form-control-string)) :br
            ((:input :type :submit :value "ok"))))
      
      (print-variables (the expression-input value))
      
      (:p "Value is: " 
          (str (escape-string 
                (format nil "~s" (the expression-input value))))))))
      
  
  :objects ((expression-input :type 'text-form-control
                              :domain :pass-thru
                              :prompt "Enter an expression"
                              :default 42)
            
            (symbol-input :type 'text-form-control
                              :domain :pass-thru
                              :prompt "Enter an expression"
                              :default t)
            ))

  
(publish-gwl-app "/rif" 
                 "training-g108::readable-inputs-form")


(define-object stretch-box (box)
  
  
  :computed-slots
  (
   (display-controls (list :color :green))
   
   (label "hey now

man 

here is another line" :settable)
   (length 10 :settable)
   (width 10 :settable)
   (height 10 :settable))
  
  :objects
  ((box :type 'stretch-box)))



