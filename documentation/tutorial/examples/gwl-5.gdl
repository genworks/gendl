(in-package :gwl-user)

(define-object box-with-inputs (base-ajax-sheet)
  
  :computed-slots
  ((use-raphael? t)
   
   (main-sheet-body 
    (with-cl-who-string ()
      (:p (when *developing?* (str (the development-links))))
      (:p (str (the inputs-section main-div)))
      (:table
       (:tr
        (dolist (viewport (list-elements (the viewport-sections)))
          (htm (:td (:td (str (the-object viewport main-div)))))))))))
  
  :objects
  ((box :type 'box
        :height (the inputs-section box-height value)
        :width (the inputs-section box-width value)
        :length (the inputs-section box-length value))
   
   (inputs-section :type 'inputs-section)

   (viewport-sections
    :type 'base-ajax-graphics-sheet
    :sequence (:size 2)
    :view-direction-default (ecase (the-child index)
                              (0 :top) (1 :trimetric))
    :image-format-default :raphael
    :display-list-objects (list (the box))
    :length 250 :width 250)))
  

 
(define-object inputs-section (sheet-section)
  
  :computed-slots
  ((inner-html (with-cl-who-string ()
                (:p (str (the box-length html-string)))
                (:p (str (the box-width html-string)))
                (:p (str (the box-height html-string))))))

  :objects 
  ((box-length :type 'text-form-control
               :default 25
               :ajax-submit-on-change? t)
   (box-width :type 'text-form-control
              :default 35
              :ajax-submit-on-change? t)
   (box-height :type 'text-form-control
               :default 45
               :ajax-submit-on-change? t)))


(publish-gwl-app "/box-with-inputs" 
                 "gwl-user::box-with-inputs")

;;
;; Access the above example with 
;; http://localhost:9000/make?object=gwl-user::box-with-inputs
;;
