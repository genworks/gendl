;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
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



(define-object test-seq (base-html-sheet)
  
  :computed-slots
  ((ordered-form-controls (append (list (the number-of-choices))
                                  (list-elements (the color-choices))
                                  )))
  
  :objects
  ((number-of-choices :type 'text-form-control
                      :default 3
                      :allow-nil? nil
                      :domain :number
                      )
   
   (color-choices :type 'menu-form-control
                  :sequence (:size (the number-of-choices value))
                  :default :green
                  :choice-list (list :red :green :blue))
   
   (submit :type 'button-form-control
           :button-type :submit
           :default " OK ")))


(define-lens (html-format test-seq)()
  :output-functions
  ((main-sheet 
    ()
    (with-cl-who()
      (:html (:head (:title "Hey Now"))
             (:body (the write-development-links)
                    (:c (:h3 "Hey Now"))
                    (with-html-form (:cl-who? t)
                      (:p 
                       (str (the number-of-choices html-string)))
                      (:p 
                       (dolist (choice (list-elements (the color-choices)))
                         (htm (str (the-object choice html-string))
                              :br)))
                      (:p (str (the submit html-string))))))))))

                    

(publish-gwl-app "/test-seq" "gwl-user::test-seq")
