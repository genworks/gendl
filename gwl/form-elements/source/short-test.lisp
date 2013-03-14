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


(define-object test-form-short (base-ajax-sheet)
  
  :computed-slots 
  ((main-sheet-body (with-cl-who-string ()
                      (str (the development-links))
                      (str (the main-section main-div))
                      )))
  
  :objects
  ((main-section :type 'short-main-section)))

(define-lens (html-format test-form-short)())

(define-object short-main-section (base-ajax-sheet)

  :objects
  ((username :type 'text-form-control   
             :size 35
             :maxlength 30
             :allow-nil? t
             :default "Ron Paul"
             :ajax-submit-on-change? t
             )))


(define-lens (html-format short-main-section)()
  :output-functions
  ((inner-html
    ()
    (with-cl-who ()
      (with-html-form (:cl-who? t)
        (str (the username html-string))
        ((:input :type :submit :value " OK " )))))))

                                     
                                     
(publish-gwl-app  "/fes" "gwl-user::test-form-short")
