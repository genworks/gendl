;;
;; Copyright 2002, 2012 Genworks International
;;
;; This source file is part of the General-purpose Declarative
;; (roLanguage project (GDL).
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

(in-package :gwl-lift-tests)

(define-object large-data (base-ajax-sheet)
 

  :computed-slots
  ((array-element-value (the array-element value))
   (array-size-value (the array-size value))

   (large-array (make-array (the array-size-value) :initial-element (the array-element-value)))

   (sum (progn (sleep 1) (reduce #'+ (the large-array))))

   (html-sections (list (the main-section)))

   (main-sheet-body (with-cl-who-string ()
		      (str (the main-section main-div)))))


  :objects
  ((main-section :type 'sheet-section 
		 :inner-html (with-cl-who-string ()
			       (:p (str (the development-links)))
			       (:p ((:span :id "gdlStatus") "Done."))
			       (:p (str (the sum)))
			       (:p (:fieldset (str (the array-element html-string))
					      (str (the array-size html-string))))))

   (array-element :type 'number-form-control
		  :size 8
		  :prompt "Value for Array Element"
		  :ajax-submit-on-change? t
		  :default 10)

   (array-size :type 'number-form-control
	       :size 8
	       :prompt "Value for Array Element"
	       :ajax-submit-on-change? t
	       :default 10000)))
   
(publish-gwl-app "/large-data" 'large-data)