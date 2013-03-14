;;
;; Copyright 2002-2011 Genworks International 
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


(define-object validation-test (base-ajax-sheet)

  :objects
  ((radio-1 :type 'radio-form-control
            :default :orange
            :description-position :paragraph-prepend
            :validation-function #'(lambda(value)
                                     (member value (plist-keys (the-child choice-plist))))
            :choice-plist (list :red "The color Red"
                                :blue "The color Blue"
                                :green "The color Green"))
   
   ("user-management-text-form-control. Form control for users first name. 
     Required input, minimum length 2 characters"
    first-name-input :type 'text-form-control
                     :size 20
                     :validation-function 
                     #'(lambda(val) (cond ((not val)
                                           (list :error "Error: First name must be specified"))
                                          ((<= (length val) 2)
                                           (list :error "Error: First name must be more than 2 characters long"))
                                          (t t)))
                     :default "a")
                    
   
   (age :type 'text-form-control
        :size 5
        :validation-function #'(lambda(input) (if (null input)  (list :error "Error Message") t))
        :domain :number
        :default nil)))



(define-lens (html-format validation-test)()
  :output-functions
  ((main-sheet
    ()
    (html
      (:h2 (:center "Test Form"))
      (the write-development-links)
      (:table
      (with-html-form ()
        
        (:tr (:td  "colour") 
             (:td (:princ (the radio-1 form-control-string))))
        (:tr (:td "first name") 
             (:td (:princ (the first-name-input form-control-string)))
        (when (the first-name-input error )
          (html (:td (:princ (the first-name-input error))))))
        (:tr (:td "age")
             (:td (:princ (the age form-control-string)))
        (when (the age error )
          (html (:td (:princ (the age error))))))
        (:tr (:td ((:input :type :submit :value " OK "))))))))))
        

                      
(publish-gwl-app "/vt" "gwl-user::validation-test")
