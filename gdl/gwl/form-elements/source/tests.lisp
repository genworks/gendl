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



(define-object test-form (base-html-sheet session-control-mixin)
  
  
  :computed-slots ((force-validation-for (list (the radio-1) (the radio-2)))

                   (session-duration 1)
                   
                   #+nil(grid-results (the grid-1 value))
                   
                   )
  
  :objects
  ((button-1 :type 'button-form-control
             :label "Hey Now"
             )
             
   
   (radio-1 :type 'radio-form-control
            :default :orange
            :description-position :paragraph-prepend
            :validation-function #'(lambda(value)
                                     (member value (plist-keys (the-child choice-plist))))
            :choice-plist (list :red "The color Red"
                                :blue "The color Blue"
                                :green "The color Green"))
   
   (radio-2 :type 'radio-form-control
            :default :maroon
            :description-position :paragraph-prepend
            :validation-function #'(lambda(value)
                                     (member value (plist-keys (the-child choice-plist))))
            :choice-plist (list :red "The color Red"
                                :blue "The color Blue"
                                :green "The color Green"))
   
   (username :type 'text-form-control
             :size 35
             :maxlength 30
             :allow-nil? t
             :default "Ron Paul")
   
   (age :type 'text-form-control
        :size 5
        :validation-function #'(lambda(input) (or (null input) (> 80 input 70)))
        :domain :number
        :default nil
        )
   
   (bio :type 'text-form-control
        :rows 8
        :size 120
        :default "
Congressman Ron Paul is the leading advocate for freedom in our nation's capital. 
As a member of the U.S. House of Representatives, Dr. Paul tirelessly works for limited 
constitutional government, low taxes, free markets, and a return to sound monetary policies. 
He is known among his congressional colleagues and his constituents for his consistent 
voting record. Dr. Paul never votes for legislation unless the proposed measure is expressly 
authorized by the Constitution. In the words of former Treasury Secretary William Simon, 
Dr. Paul is the one exception to the Gang of 535 on Capitol Hill.")
   
   (issues :type 'menu-form-control
           :choice-list (list "Taxes" "Health Care" "Foreign Policy")
           :default "Taxes"
           :multiple? t)
   
   (color :type 'menu-form-control
          :size 7
          
          :choice-styles (list :red "color: red;"
                               :green "color: green;"
                               :blue "color: blue;"
                               :magenta "color: magenta;"
                               :cyan "color: cyan;"
                               :yellow "color: yellow;"
                               :orange "color: orange;")
                               
          :choice-plist (list :red "red"
                              :green "green" 
                              :blue "blue" 
                              :magenta "magenta" 
                              :cyan "cyan" 
                              :yellow "yellow" 
                              :orange "orange")
          
          :validation-function #'(lambda(color)
                                   (if (intersection (ensure-list color) (list :yellow :magenta))
                                       (list :error :disallowed-color-choice)
                                     t))
          :multiple? t
          :default :red)

   
   (early-riser? :type 'checkbox-form-control
                 :default nil)
   
   
   
   (favorite-links :type 'text-form-control
                   :sequence (:size 3)
                   :size 70
                   :default "http://")
   
   (form-1 :type 'menu-form-control
           :default nil
           :domain :number
           :multiple? t
           :choice-list (when t (list 1 2 3 4 5))
           :choice-plist (when nil (list :a 5 :b 6 :c 7)))
   
   
   (list-choices :type 'menu-form-control
                 :default '(:A36 nil 35000 12000)
                 :choice-plist '((:A36 nil 35000 12000) :A36
                                 (:A40 nil nil 45000) :A40
                                 (:ST43 nil nil) :ST43))
   
   
   (number-of-grid-columns :type 'text-form-control
                           :default 3
                           :allow-invalid? nil
                           :validation-function 
                           #'(lambda(value)
                               (if (and (integerp value)
                                        (<= 1 value 3))
                                   t
                                 (list :validated-value nil
                                       :error "Must be between 1 and 3")))
                           :prompt "columns: ")
   
;;;;    (grid-1 :type 'grid-form-control
;;;;            :include-delete-buttons? nil
;;;;            :keys (subseq (list :col-a :col-b :col-c)
;;;;                          0 (the number-of-grid-columns value))
;;;;            :row-labels (list "<i>take-off angle</i>"
;;;;                              "take-off gap"
;;;;                              "take-off overlap"
;;;;                              "landing angle"
;;;;                              "landing gap"
;;;;                              )

;;;;            :default (make-list 5
;;;;                                :initial-element
;;;;                                (make-list (the number-of-grid-columns
;;;;                                             value)
;;;;                                           :initial-element nil)))
;;;;    
   (reason :type 'text-form-control
           :rows 5
           :cols 100
           :domain :string
           :validation-function #'(lambda(input)
                                    (if (< (length input) 4)
                                        (list :error "Error: A reason for rejection is required")
                                      t))
           :default "")))
  

(define-lens (html-format test-form)()
  :output-functions
  ((main-sheet
    ()
    (with-html-output (*stream* nil :indent t)
      (:html (:head (:title "Test Form"))
             (:body (:h2 (:center "Test Form"))
                    (the write-development-links)
                    (with-html-form (:cl-who? t)
                      
                      (:p (str (the age html-string)))
                      
                      (:p (str (the button-1 html-string)))
                      
                      (:p (str (the radio-1 html-string)))
                      (:p (str (the radio-2 html-string)))
                      
                      (:p (str (the reason html-string)))
                      
                      (:p (str (the username html-string)))
                      (:p "(internal value is: " (fmt "~s" (the username value)) ")")
                      (:p (str (the age html-string)))
                      (:p "(internal value is: " (fmt "~s" (the age value)) ")")
                      (:p (str (the bio html-string)))
                      (:p (:table 
                      (:tr (:td (str (the issues html-string))))
                      (:tr (:td (str (the color html-string))))
                      (:tr (:td (str  (the color value))))
                      (:p (str (the early-riser? html-string)))
                      
                      (dolist (link (list-elements (the favorite-links)))
                        (htm (str (the-object link html-string))))

                      (:p (str (the number-of-grid-columns
                                 html-string)))
                      
                      (:table
                       (:tr
                        (:td
                         ((:table :border 1)
                          (:tr (:td " " :br))
                          (:tr (:td "take-off angle"))
                          (:tr (:td "take-off gap"))
                          (:tr (:td "take-off overlap"))))
                        (:td
                         #+nil(str (the grid-1 form-control-string)))))
                      
                      
                      (:p #+nil(str (the grid-1 value)))
                      
                      
                      (:p (str (the form-1 html-string)))
                      (:p (str (the list-choices html-string)))
                      
                      (:p (fmt "~s" (the list-choices value)))
                      
                      (:p ((:input :type :submit :value " OK "))))))))))))
                      
  
(publish-gwl-app  "/fe" "gwl-user::test-form")




