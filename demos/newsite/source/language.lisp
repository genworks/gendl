;;
;; Copyright 2002-2011, 2012 Genworks International
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

(in-package :www.genworks.com)


(define-object language (base-site-sheet)


  :computed-slots
  ((title (locale-string :language-selection))
   (link-title (locale-string :language))

   (right-section-inner-html 
    (with-cl-who-string ()
      (:h2 (str (locale-string :language-selection)))
      ((:div :id "contact"))
      
      (:p "Please select your desired language for the site. Note that site
localization is an ongoing process, and some sections remain English-only at this moment.")
      
      (:fieldset
       (str (the choice html-string ))))))


  :objects
  ((choice :type 'radio-form-control 
	   :description-position :table-row-prepend
	   :default :english
	   :ajax-submit-on-change? t
	   :choice-plist 
	   (list :english (with-cl-who-string ()
			    ((:img :src "/newsite-static/images/british-flag.jpg" :width 150))
			    " "
			    (when (eql (the lang) :english)
			      (htm ((:img :src "/newsite-static/images/green-checkmark.jpg" :width 80)))))
		 :chinese (with-cl-who-string ()
			    ((:img :src "/newsite-static/images/china-flag.gif" :width 150))
			    " "
			    (when (eql (the lang) :chinese)
			      (htm ((:img :src "/newsite-static/images/green-checkmark.jpg" :width 80)))))))))
