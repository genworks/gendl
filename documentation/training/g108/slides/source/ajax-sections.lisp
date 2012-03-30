(in-package :training-g108)


(define-object ajax-sections (slide-show-leaf)
  
  :computed-slots
  ((strings-for-display "Updating Page Sections with Ajax")
   
   (slide-data `((:title "Using text-form-control"
			 :bullet-points
			 ((:description "text-form-control for type-in values")

			  (:description "Can be string, number, symbol, or list")

			  (:description
			   "Must decompose the page into at least one section"
			   
			   :examples
			   ((:define-object inputs-outputs)
			    (:define-object inputs-outputs-page-content)

			    ))))))))
