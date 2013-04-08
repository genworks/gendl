;;
;; Copyright 2002, 2012 Genworks International
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

(in-package :gwl-user)



(defparameter *locale-hash*
  (let ((strings 
	 `(:hey-now
	   (:english "Hey Now"
	    :chinese "嘿，现在"))))
    (let ((ht (make-hash-table :size (length strings))))
      (mapc #'(lambda(key value) (setf (gethash key ht) value))
	    (plist-keys strings) (plist-values strings)) ht)))


(defun locale-string (key)
  (gethash key *locale-hash*))
  
(define-object hey-now (base-ajax-sheet)
 
  :input-slots ((language-default :english))

  :computed-slots
  ((lang (the language-choice value))

   (html-sections (list (the main-section)))

   (main-sheet-body (with-cl-who-string ()
		      (str (the main-section main-div)))))


  :objects
  ((main-section :type 'sheet-section 
		 :inner-html (with-cl-who-string ()
			       (:p (str (the development-links)))
			       (:p (:h1 (str (locale-string :hey-now))))
			       (:p (:fieldset (str (the language-choice html-string))))))

   (language-choice :type 'menu-form-control
		    :size 1
		    :prompt "Choose Language"
		    :ajax-submit-on-change? t
		    :default (the language-default)
		    :choice-plist (list :english "English" :chinese "中国的"))))
		    
(publish-gwl-app "/hey-now" 'hey-now)
